{-|
Module      : Htcc.Parser.Combinators.Var
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.Var (
    varInit
) where
import           Control.Monad                          (foldM, forM, unless,
                                                         void, when, (>=>))
import           Control.Monad.Extra                    (andM)
import           Control.Monad.Fix                      (fix)
import           Control.Monad.Trans                    (MonadTrans (..))
import           Control.Monad.Trans.Reader             (ReaderT (..), asks,
                                                         runReaderT)
import           Control.Monad.Trans.State              (get, gets, put)
import           Data.Bits                              (Bits)
import           Data.Bool                              (bool)
import           Data.Char                              (ord)
import           Data.Foldable                          (toList)
import           Data.Functor                           ((<&>))
import           Data.List                              (sortBy)
import qualified Data.Map                               as MP
import           Data.Maybe                             (fromJust, fromMaybe)
import qualified Data.Sequence                          as SQ
import qualified Data.Text                              as T
import           Data.Tuple.Extra                       (second)
import qualified Htcc.CRules.Types                      as CT
import           Htcc.Parser.AST                        (ATKind (..),
                                                         ATree (..), addKind,
                                                         atAssign, atBlock,
                                                         atCast, atExprStmt,
                                                         atMemberAcc, atNumLit,
                                                         atUnary, treealize)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Utils          (bracket,
                                                         isInvalidAggregateValueConversion,
                                                         isInvalidFunctionPointerInitializer,
                                                         isInvalidObjectPointerValue,
                                                         maybeToParser,
                                                         registerLVar,
                                                         requiresUnsupportedNonAddressableArrayDecay)
import           Htcc.Parser.ConstructionData.Core      (ConstructionData (suppressUnsupportedValueChecks),
                                                         incomplete, lookupLVar)
import           Htcc.Parser.ConstructionData.Scope.Var (Var (vtype))
import           Htcc.Utils                             (tshow)
import           Numeric.Natural                        (Natural)
import qualified Text.Megaparsec                        as M
import qualified Text.Parsec.Error                      as PE

type DesignatorParser i r = ReaderT (T.Text, Parser i (ATree i)) (Parser i) r

runDesignator :: (SQ.Seq (ATree i) -> SQ.Seq (CT.Desg i) -> DesignatorParser i r)
    -> T.Text
    -> Parser i (ATree i)
    -> Parser i r
runDesignator p ident assignParser = runReaderT (p SQ.empty SQ.empty) (ident, assignParser)

validateScalarInitializer :: (Ord i, Bits i, Integral i) => CT.StorageClass i -> ATree i -> DesignatorParser i (ATree i)
validateScalarInitializer targetTy at@(ATNode _ ty _ _) = do
    unsupportedChecksSuppressed <- lift $ lift $ gets suppressUnsupportedValueChecks
    if isVoidExpressionType ty then
        fail "void value not ignored as it ought to be"
    else if isInvalidFunctionInitializer || isInvalidObjectInitializer || isInvalidAggregateInitializer then
        fail "invalid initializer for scalar object"
    else if not unsupportedChecksSuppressed && requiresUnsupportedNonAddressableArrayDecay at then
        fail "unsupported non-addressable array member decay"
    else
        pure at
    where
        isVoidExpressionType = isVoidTypeKind . CT.toTypeKind

        isVoidTypeKind ty' = case ty' of
            CT.CTVoid        -> True
            CT.CTShort ty''  -> isVoidTypeKind ty''
            CT.CTLong ty''   -> isVoidTypeKind ty''
            CT.CTSigned ty'' -> isVoidTypeKind ty''
            _                -> False

        isInvalidFunctionInitializer =
            isInvalidFunctionPointerInitializer targetTy at

        isInvalidObjectInitializer =
            isInvalidObjectPointerValue targetTy at

        isInvalidAggregateInitializer =
            isInvalidAggregateValueConversion targetTy at
validateScalarInitializer _ _ = fail "expected to assign"

withDesignatorCheckpoint :: DesignatorParser i a -> DesignatorParser i a
withDesignatorCheckpoint p = ReaderT $ \ctx ->
    bracket M.getParserState M.setParserState $ const $
        bracket (lift get) (lift . put) $ const $
            runReaderT p ctx

tryDesignator :: DesignatorParser i a -> DesignatorParser i a
tryDesignator p = ReaderT $ \ctx ->
    M.try $ runReaderT p ctx

arrayElementType :: Ord i => CT.StorageClass i -> CT.StorageClass i
arrayElementType ty = CT.mapTypeKind (const elemTy) ty
    where
        elemTy = case CT.deref ty of
            Just ty' -> CT.toTypeKind ty'
            Nothing -> case CT.toTypeKind ty of
                CT.CTIncomplete (CT.IncompleteArray ty') -> ty'
                _ -> error "internal compiler error"

fixedArrayLength :: Ord i => CT.StorageClass i -> Maybe Natural
fixedArrayLength ty
    | not (CT.isArray ty) = Nothing
    | elemBytes == 0 = Nothing
    | totalBytes == 0 = Nothing
    | otherwise = Just $ totalBytes `div` elemBytes
    where
        totalBytes = CT.sizeof ty
        elemBytes = CT.sizeof $ arrayElementType ty

inferredArrayBoundElementType :: Ord i => CT.StorageClass i -> CT.StorageClass i
inferredArrayBoundElementType ty = fromMaybe (arrayElementType ty) logicalElemTy
    where
        (baseTy, rebuild) = CT.dctorArray $ CT.toTypeKind ty
        logicalElemTy =
            (\elemTy -> CT.mapTypeKind (const $ rebuild elemTy) ty)
                <$> CT.fromIncompleteArray baseTy

fixedCharArrayStringFits :: Integral i => i -> String -> Bool
fixedCharArrayStringFits len s = toInteger (length s) <= toInteger len + 1

isCharArrayType :: Ord i => CT.StorageClass i -> Bool
isCharArrayType ty =
    CT.isArray ty && maybe False isCharType (CT.deref ty)
    where
        isCharType ty' = case CT.toTypeKind ty' of
            CT.CTChar -> True
            _         -> False

lookInitializerStringFor :: Ord i => CT.StorageClass i -> Parser i ()
lookInitializerStringFor ty = bool M.empty (pure ()) =<< andM
    [ pure $ isCharArrayType ty
    , M.option False (True <$ M.lookAhead stringLiteral)
    ]

failCommitted :: String -> DesignatorParser i a
failCommitted msg = do
    pos <- lift M.getSourcePos
    lift $ M.parseError $ M.ParsecError True $ PE.newErrorMessage (PE.Message msg) pos

data ArrayBoundInference i
    = InferArrayBoundLength Int
    | InferArrayBoundType (CT.StorageClass i)

inferArrayBoundFromInitializer :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> DesignatorParser i (ArrayBoundInference i)
inferArrayBoundFromInitializer ty =
    withDesignatorCheckpoint $ inferArrayBoundFromInitializer' ty

inferArrayBoundFromInitializer' :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> DesignatorParser i (ArrayBoundInference i)
inferArrayBoundFromInitializer' ty = do
    void $ lift lbrace
    bracedStringInitializerLength M.<|> inferElementCount
    where
        elemTy = inferredArrayBoundElementType ty
        emptyInitializerError = "cannot initialize incomplete array with an empty initializer list"

        inferElementCount = do
            len <- countElements 0
            lift rbrace
            pure $ InferArrayBoundLength len

        bracedStringInitializerLength
            | isCharArrayType ty = do
                lift $ lookInitializerStringFor ty
                len <- length <$> lift stringLiteral
                void $ lift $ M.option () (void comma)
                void $ lift rbrace
                pure $ InferArrayBoundLength len
            | otherwise = M.empty

        countElements acc = M.choice
            [ do
                void $ lift (M.lookAhead rbrace)
                if acc == 0
                    then fail emptyInitializerError
                    else pure acc
            , do
                skipInitializer True elemTy
                continue <- continueBracedAggregate
                bool
                    (pure $ succ acc)
                    (countElements $ succ acc)
                    continue
            ]

lookAheadTrailingCommaBeforeRbrace :: DesignatorParser i Bool
lookAheadTrailingCommaBeforeRbrace =
    lift $ M.option False $ True <$ M.try (M.lookAhead (comma *> rbrace))

continueBracedAggregate :: DesignatorParser i Bool
continueBracedAggregate = M.choice
    [ False <$ lift (M.lookAhead rbrace)
    , False <$ lift (M.try (comma *> M.lookAhead rbrace))
    , True <$ lift comma
    ]

continueBraceElidedAggregate :: DesignatorParser i Bool
continueBraceElidedAggregate = do
    trailingOuterComma <- lookAheadTrailingCommaBeforeRbrace
    if trailingOuterComma
        then pure False
        else M.choice
            [ True <$ lift comma
            , pure False
            ]

skipInitializer :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Bool
    -> CT.StorageClass i
    -> DesignatorParser i ()
skipInitializer allowStructBraceElision ty = M.choice
    [ lift lookInitializerString *> void (lift stringLiteral)
    , lift lookInitializerList *> leadingBraceInitializer
    , braceElidedAggregateInit
    , rejectScalarFallback *> void (asks snd >>= lift >>= validateScalarInitializer ty)
    ]
    where
        lookInitializerString = lookInitializerStringFor ty
        lookInitializerList = bool M.empty (pure ()) =<< M.option False (True <$ M.lookAhead lbrace)
        leadingBraceInitializer = skipInitializerList ty
        braceElidedAggregateInit
            | allowStructBraceElision = case CT.toTypeKind ty of
                CT.CTArray _ _ ->
                    rejectAggregateExpr "expected '{' to initialize an array" *> skipArrayNoBraces ty
                CT.CTStruct mems ->
                    rejectAggregateExpr "expected '{' to initialize a struct" *> skipStructNoBraces (orderedStructMembers mems)
                CT.CTNamedStruct _ _ mems ->
                    rejectAggregateExpr "expected '{' to initialize a struct" *> skipStructNoBraces (orderedStructMembers mems)
                _ ->
                    M.empty
            | otherwise = M.empty
        rejectScalarFallback = rejectScalarFallbackFor ty

skipInitializerList :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> DesignatorParser i ()
skipInitializerList ty = do
    void $ lift lbrace
    case CT.toTypeKind ty of
        CT.CTArray _ _   -> skipBracedInitializerString ty M.<|> (skipArrayList ty <* lift rbrace)
        CT.CTStruct mems -> skipStructList (orderedStructMembers mems) <* lift rbrace
        CT.CTNamedStruct _ _ mems -> skipStructList (orderedStructMembers mems) <* lift rbrace
        _                -> do
            skipInitializer False ty
            void $ lift $ M.option () (void comma)
            void $ lift rbrace
    where
        skipBracedInitializerString aty = tryDesignator $ do
            lift $ lookInitializerStringFor aty
            void $ lift stringLiteral
            void $ lift $ M.option () (void comma)
            void $ lift rbrace

        skipArrayList aty = M.choice
            [ void $ lift (M.lookAhead rbrace)
            , do
                skipInitializer True (arrayElementType aty)
                continue <- continueBracedAggregate
                bool
                    (pure ())
                    (skipArrayList aty)
                    continue
            ]

        skipStructList [] = pure ()
        skipStructList (mem:rest) = M.choice
            [ void $ lift (M.lookAhead rbrace)
            , do
                skipInitializer True (CT.SCAuto $ CT.smType mem)
                continue <- continueBracedAggregate
                bool
                    (pure ())
                    (skipStructList rest)
                    continue
            ]

skipArrayNoBraces :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> DesignatorParser i ()
skipArrayNoBraces ty = case fixedArrayLength ty of
    Just n  -> skipArrayLoop (fromIntegral n) 0
    Nothing -> fail "internal compiler error"
    where
        elemTy = arrayElementType ty

        skipArrayLoop len idx
            | idx >= len = pure ()
            | otherwise = do
                skipInitializer True elemTy
                if succ idx == len
                    then pure ()
                    else do
                        continue <- continueBraceElidedAggregate
                        bool
                            (pure ())
                            (skipArrayLoop len (succ idx))
                            continue

skipStructNoBraces :: (Integral i, Bits i, Read i, Show i, Ord i)
    => [CT.StructMember i]
    -> DesignatorParser i ()
skipStructNoBraces [] = pure ()
skipStructNoBraces (mem:rest) = do
    skipInitializer True (CT.SCAuto $ CT.smType mem)
    case rest of
        [] -> pure ()
        _ -> do
            continue <- continueBraceElidedAggregate
            bool
                (pure ())
                (skipStructNoBraces rest)
                continue

desgNode :: (Num i, Ord i, Show i)
    => ATree i
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (ATree i)
desgNode nd desg = atExprStmt . flip atAssign nd <$> desgLVal desg

desgLVal :: (Num i, Ord i, Show i)
    => SQ.Seq (CT.Desg i)
    -> DesignatorParser i (ATree i)
desgLVal desg = flip (`foldr` facc) desg $ \idx acc -> case idx of
        CT.DesgIdx idx' -> do
            nd' <- maybeToParser' . (`addKind` atNumLit idx') =<< acc
            flip (atUnary ATDeref) nd' <$> maybeToParser' (CT.deref (atype nd'))
        CT.DesgMem mem -> atMemberAcc mem <$> acc
    where
        facc = asks fst
            >>= lift . lift . gets . lookupLVar
            >>= maybeToParser'
            <&> treealize
        maybeToParser' = lift . maybeToParser "invalid initializer-list"

zeroFillByteOffsets :: (Integral i, Ord i, Show i)
    => SQ.Seq (CT.Desg i)
    -> [i]
    -> DesignatorParser i (SQ.Seq (ATree i))
zeroFillByteOffsets desg offsets = do
    base <- desgLVal desg
    let bytePtrTy = CT.SCAuto $ CT.CTPtr CT.CTChar
        baseAddrTy = CT.mapTypeKind CT.CTPtr (atype base)
        byteBase = atCast bytePtrTy $ atUnary ATAddr baseAddrTy base
    fmap SQ.fromList $ forM offsets $ \offset -> do
        bytePtr <- case offset of
            0 -> pure byteBase
            _ -> lift
                $ maybeToParser "invalid initializer-list"
                $ addKind byteBase (atNumLit offset)
        pure $ atExprStmt $ atAssign (atUnary ATDeref (CT.SCAuto CT.CTChar) bytePtr) (atNumLit 0)

zeroFillObject :: (Integral i, Ord i, Show i)
    => CT.StorageClass i
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
zeroFillObject ty desg = zeroFillByteOffsets desg [0 .. pred totalBytes]
    where
        totalBytes = fromIntegral $ CT.sizeof ty

zeroFillRemainingStructBytes :: (Integral i, Ord i, Show i)
    => CT.StorageClass i
    -> [CT.StructMember i]
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
zeroFillRemainingStructBytes ty explicitMembers desg =
    zeroFillByteOffsets desg $ filter (`notElem` explicitOffsets) [0 .. pred totalBytes]
    where
        totalBytes = fromIntegral $ CT.sizeof ty
        explicitOffsets = concatMap memberByteOffsets explicitMembers
        memberByteOffsets mem
            | memberBytes == 0 = []
            | otherwise = [start .. start + pred memberBytes]
            where
                start = fromIntegral $ CT.smOffset mem
                memberBytes = fromIntegral $ CT.sizeof $ CT.smType mem

initLoop :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i), i)
initLoop ty ai desg = second fromIntegral <$> initLoop' ai <* lift rbrace
    where
        initLoop' ai' = case fixedArrayLength ty of
            Just n -> fix (\f (idx, rl) -> do
                let arrayLen = fromIntegral n
                when (idx >= arrayLen) $ failCommitted "excess elements in array initializer"
                rs <- desgInit True elemTy rl (CT.DesgIdx (fromIntegral idx) SQ.<| desg)
                continue <- continueBracedAggregate
                bool
                    (pure (rs, succ idx))
                    (f (succ idx, rs))
                    continue
                ) (0 :: Natural, ai')
            Nothing -> fail "internal compiler error"
        elemTy = arrayElementType ty

initZero :: (Integral i, Ord i, Show i)
    => CT.TypeKind i
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initZero (CT.CTArray n ty) desg =
    foldM
        (\acc idx -> (SQ.>< acc) <$> initZero ty (CT.DesgIdx idx SQ.<| desg))
        SQ.empty
        [0..fromIntegral (pred n)]
initZero t@(CT.CTStruct _) desg = zeroFillObject (CT.SCAuto t) desg
initZero t@CT.CTNamedStruct {} desg = zeroFillObject (CT.SCAuto t) desg
initZero _ desg = SQ.singleton <$> desgNode (atNumLit 0) desg

orderedStructMembers :: MP.Map T.Text (CT.StructMember i) -> [CT.StructMember i]
orderedStructMembers = sortBy (\x y -> compare (CT.smOffset x) (CT.smOffset y)) . MP.elems

structStorageClassFromMembers :: [CT.StructMember i] -> CT.StorageClass i
structStorageClassFromMembers mems =
    CT.SCAuto $ CT.CTStruct $ MP.fromList $ zipWith (\idx mem -> (tshow idx, mem)) [(0 :: Int)..] mems

isAggregateType :: CT.StorageClass i -> Bool
isAggregateType ty = CT.isArray ty || CT.isCTStruct ty

peekAssignType :: DesignatorParser i (Maybe (CT.StorageClass i))
peekAssignType = do
    assignParser <- asks snd
    constructionData <- lift $ lift get
    observed <- lift $ M.option Nothing $ Just <$> M.try (M.lookAhead assignParser)
    lift $ lift $ put constructionData
    pure $ atype <$> observed

aggregateInitializerBraceError :: CT.StorageClass i -> Maybe String
aggregateInitializerBraceError ty
    | CT.isCTStruct ty = Just "expected '{' to initialize a struct"
    | CT.isArray ty = Just "expected '{' to initialize an array"
    | otherwise = Nothing

rejectAggregateExpr :: String -> DesignatorParser i ()
rejectAggregateExpr msg = do
    startsWithString <- lift $ M.option False (True <$ M.lookAhead stringLiteral)
    unless startsWithString $ do
        isAggregateExpr <- maybe False isAggregateType <$> peekAssignType
        bool (pure ()) (fail msg) isAggregateExpr

rejectScalarFallbackFor :: CT.StorageClass i -> DesignatorParser i ()
rejectScalarFallbackFor ty =
    maybe
        rejectInvalidScalarAggregate
        fail
        (aggregateInitializerBraceError ty)
    where
        rejectInvalidScalarAggregate = do
            invalidAggregate <- maybe False (scalarAggregateNeedsBraces ty) <$> peekAssignType
            when invalidAggregate $ fail "invalid initializer for scalar object"

        scalarAggregateNeedsBraces target rhs = case CT.toTypeKind target of
            CT.CTPtr _ -> CT.isCTStruct rhs
            _          -> isAggregateType rhs

arType :: Integral i => CT.StorageClass i -> i -> CT.StorageClass i
arType ty len = case CT.toTypeKind ty of
    CT.CTIncomplete (CT.IncompleteArray innerTy) ->
        CT.mapTypeKind (const $ inferredOuterArrayType innerTy len) ty
    _ -> snd (CT.dctorArray ty) $
        CT.mapTypeKind (CT.CTArray (fromIntegral len) . fromJust . CT.fromIncompleteArray) ty

inferredOuterArrayType :: Integral i => CT.TypeKind i -> i -> CT.TypeKind i
inferredOuterArrayType innerTy len = case innerTy of
    CT.CTArray _ _ ->
        fromMaybe fallback $
            CT.concatCTArray
                (CT.makeCTArray [fromIntegral len] $ CT.removeAllExtents innerTy)
                innerTy
    _ -> fallback
    where
        fallback = CT.CTArray (fromIntegral len) innerTy

registerInferredArrayBound :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> ArrayBoundInference i
    -> DesignatorParser i (CT.StorageClass i)
registerInferredArrayBound ty inferred = do
    ident <- asks fst
    let applyInference target = case inferred of
            InferArrayBoundLength len -> arType target (fromIntegral len)
            InferArrayBoundType ty'   -> ty'
        newt = applyInference ty
    currentTy <- lift $ lift $ gets (fmap vtype . lookupLVar ident)
    case currentTy of
        Just currentTy'
            | CT.isIncompleteArray currentTy' ->
                void $ lift $ registerLVar (applyInference currentTy') ident
        _ -> pure ()
    pure newt

initializerString :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Bool
    -> CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerString allowStructBraceElision ty ai desg
    | CT.isIncompleteArray ty = do
        len <- lift $ bracket M.getParserState M.setParserState (const $ length <$> stringLiteral)
        newt <- registerInferredArrayBound ty $ InferArrayBoundLength len
        desgInit allowStructBraceElision newt ai desg
    | otherwise = case CT.toTypeKind ty of
        CT.CTArray n _ -> do
            s <- lift stringLiteral
            unless (fixedCharArrayStringFits n s) $
                failCommitted "initializer-string for array of chars is too long"
            let s' = s <> replicate (fromIntegral n - pred (length s)) (toEnum 0)
                inds = sortBy (flip (.) reverse . compare . reverse) $ CT.accessibleIndices $ CT.toTypeKind ty
            fmap ((ai SQ.><) . SQ.fromList)
                $ mapM (uncurry desgNode)
                $ zipWith (flip (.) ((SQ.>< desg) . SQ.fromList) . (,) . atNumLit . fromIntegral . ord) s' inds
        _ -> fail "internal compiler error"

bracedInitializerString :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Bool
    -> CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
bracedInitializerString allowStructBraceElision ty ai desg = do
    lift $ lookInitializerStringFor ty
    rs <- initializerString allowStructBraceElision ty ai desg
    void $ lift $ M.option () (void comma)
    void $ lift rbrace
    pure rs

initializerList :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerList ty ai desg = M.choice
    [ allZeroInit
    , withInitElements
    ]
    where
        allZeroInit
            | (CT.isArray ty && not (CT.isIncompleteArray ty)) || CT.isCTStruct ty = do
                void $ lift $ M.try (lbrace *> rbrace)
                (ai SQ.><) <$> initZero (CT.toTypeKind ty) desg
            | otherwise = M.empty

        withInitElements
            | CT.isIncompleteArray ty = do
                inferred <- inferArrayBoundFromInitializer ty
                newt <- registerInferredArrayBound ty inferred
                desgInit False newt ai desg
            | otherwise = do
                void $ lift lbrace
                case CT.toTypeKind ty of
                    CT.CTArray _ _ ->
                        bracedInitializerString False ty ai desg M.<|> do
                            (ast, idx) <- initLoop ty ai desg
                            (ast SQ.><)
                                <$> foldM
                                    (\acc idx' -> (SQ.>< acc) <$> initZero (CT.toTypeKind elemTy) (CT.DesgIdx idx' SQ.<| desg))
                                    SQ.empty
                                    [fromIntegral idx..pred (fromIntegral arrayLen)]
                    CT.CTStruct mems -> do
                        (ast, explicitMems, _) <- initStructLoop (orderedStructMembers mems) ai
                        (ast SQ.><) <$> zeroFillRemainingStructBytes ty explicitMems desg
                    CT.CTNamedStruct _ _ mems -> do
                        (ast, explicitMems, _) <- initStructLoop (orderedStructMembers mems) ai
                        (ast SQ.><) <$> zeroFillRemainingStructBytes ty explicitMems desg
                    _ -> do
                        rs <- desgInit False ty ai desg
                        void $ lift $ M.option () (void comma)
                        void $ lift rbrace
                        pure rs
            where
                elemTy = arrayElementType ty
                arrayLen = fromMaybe 0 $ fixedArrayLength ty
                initStructLoop mems ai' = initStructLoop' [] mems ai' <* lift rbrace

                initStructLoop' explicit [] ai' = pure (ai', explicit, [])
                initStructLoop' explicit (mem:rest) ai' = do
                    rs <- desgInit True (CT.SCAuto $ CT.smType mem) ai' (CT.DesgMem mem SQ.<| desg)
                    let explicit' = mem : explicit
                    continue <- continueBracedAggregate
                    bool
                        (pure (rs, explicit', rest))
                        (initStructLoop' explicit' rest rs)
                        continue

initializerStructNoBraces :: (Integral i, Bits i, Read i, Show i, Ord i)
    => [CT.StructMember i]
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerStructNoBraces mems ai desg = do
    let structTy = structStorageClassFromMembers mems
    (ast, explicitMems, _) <- initStructLoop [] mems ai
    (ast SQ.><) <$> zeroFillRemainingStructBytes structTy explicitMems desg
    where
        initStructLoop explicit [] ai' = pure (ai', explicit, [])
        initStructLoop explicit (mem:rest) ai' = do
            rs <- desgInit True (CT.SCAuto $ CT.smType mem) ai' (CT.DesgMem mem SQ.<| desg)
            let explicit' = mem : explicit
            case rest of
                [] -> pure (rs, explicit', [])
                _ -> do
                    continue <- continueBraceElidedAggregate
                    bool
                        (pure (rs, explicit', rest))
                        (initStructLoop explicit' rest rs)
                        continue

initializerArrayNoBraces :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerArrayNoBraces ty ai desg = case fixedArrayLength ty of
    Just n -> do
        let len = fromIntegral n
        (ast, idx) <- initArrayLoop len bt 0 ai
        (ast SQ.><)
            <$> foldM
                (\acc idx' -> (SQ.>< acc) <$> initZero bt (CT.DesgIdx idx' SQ.<| desg))
                SQ.empty
                [fromIntegral idx..pred (fromIntegral n)]
    Nothing -> fail "internal compiler error"
    where
        bt = CT.toTypeKind $ arrayElementType ty
        initArrayLoop n bt idx ai'
            | idx >= n = pure (ai', idx)
            | otherwise = do
                rs <- desgInit True (CT.SCAuto bt) ai' (CT.DesgIdx idx SQ.<| desg)
                if idx == pred n
                    then pure (rs, succ idx)
                    else do
                        continue <- continueBraceElidedAggregate
                        bool
                            (pure (rs, succ idx))
                            (initArrayLoop n bt (succ idx) rs)
                            continue

desgInit :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Bool
    -> CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
desgInit allowStructBraceElision ty ai desg = M.choice
    [ lift (lookInitializerStringFor ty) *> initializerString allowStructBraceElision ty ai desg
    , lift lookInitializerList *> leadingBraceInitializer
    , braceElidedAggregateInit
    , rejectScalarFallback *> scalarFallback
    ]
    where
        lookInitializerList = bool M.empty (pure ()) =<< M.option False (True <$ M.lookAhead lbrace)
        leadingBraceInitializer = initializerList ty ai desg
        braceElidedAggregateInit
            | allowStructBraceElision = case CT.toTypeKind ty of
                CT.CTArray _ _ ->
                    rejectAggregateExpr "expected '{' to initialize an array" *> initializerArrayNoBraces ty ai desg
                CT.CTStruct mems ->
                    rejectAggregateExpr "expected '{' to initialize a struct" *> initializerStructNoBraces (orderedStructMembers mems) ai desg
                CT.CTNamedStruct _ _ mems ->
                    rejectAggregateExpr "expected '{' to initialize a struct" *> initializerStructNoBraces (orderedStructMembers mems) ai desg
                _ ->
                    M.empty
            | otherwise = M.empty
        rejectScalarFallback = rejectScalarFallbackFor ty
        scalarFallback = do
            rhs <- asks snd >>= lift >>= validateScalarInitializer ty
            (ai SQ.|>) <$> desgNode rhs desg

varInit' :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> ATree i
    -> Parser i (ATree i)
varInit' p ty ident _ = atBlock . toList <$> runDesignator (desgInit False ty) ident p

varInit :: (Integral i, Bits i, Read i, Show i, Ord i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
varInit p ty ident = lift (gets $ fromMaybe ty . incomplete ty)
    >>= flip registerLVar ident
    >>= varInit' p ty ident
