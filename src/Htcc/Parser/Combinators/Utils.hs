{-|
Module      : Htcc.Parser.Combinators.Utils
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE LambdaCase, Rank2Types, TypeOperators #-}
module Htcc.Parser.Combinators.Utils (
    maybeToParser
  , registerLVar
  , registerGVar
  , registerGVarWith
  , registerStringLiteral
  , registerFunc
  , registerTypedef
  , decayExprType
  , conditionalResultType
  , isNullPointerConstant
  , functionDesignatorSourcePointerType
  , carriesFunctionDesignatorValue
  , containsEscapingStmtExprControlFlow
  , hasInvalidStmtExprControlFlow
  , requiresUnsupportedNonAddressableArrayDecay
  , isInvalidAggregateValueConversion
  , isInvalidObjectPointerValue
  , isInvalidFunctionPointerValue
  , isInvalidFunctionPointerInitializer
  , captureFunctionParamScopes
  , bracket
  , getPosState
  , isConstexprArithmeticCastType
  , applyConstexprCast
) where
import           Control.Applicative                             ((<|>))
import           Control.Monad.State                             (gets, modify,
                                                                  put)
import           Control.Natural                                 (type (~>))
import           Data.Bits                                       (Bits (..))
import qualified Data.ByteString                                 as B
import           Data.Maybe                                      (isJust)
import qualified Data.Set                                        as S
import qualified Data.Text                                       as T
import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (ATKind (..),
                                                                  ATKindFor (..),
                                                                  ATree (..),
                                                                  fromATKindFor)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.ConstructionData.Core               (ConstructionData (functionParamScopes),
                                                                  FunctionParamScope,
                                                                  addFunction,
                                                                  addGVar,
                                                                  addGVarWith,
                                                                  addLVar,
                                                                  addLiteral,
                                                                  addTypedef,
                                                                  hasIncompleteObjectType)
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import           Htcc.Parser.ConstructionData.Scope.Var          (GVarInitWith)
import qualified Htcc.Tokenizer.Token                            as HT
import qualified Text.Megaparsec                                 as M

maybeToParser :: String -> Maybe ~> Parser i
maybeToParser s = maybe (fail s) pure

type PureAdder i = CT.StorageClass i
    -> HT.TokenLC i
    -> ConstructionData i
    -> Either (ASTError i) (ATree i, ConstructionData i)

registerVar :: (Bits i, Integral i)
    => PureAdder i
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerVar adder ty ident = gets (adder ty (tmpTKIdent ident))
    >>= \case
        Right (lat, scp') -> lat <$ put scp'
        Left err -> fail $ T.unpack $ fst err

registerLVar :: (Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerLVar = registerVar addLVar

registerStringLiteral :: (Bits i, Integral i)
    => B.ByteString
    -> Parser i (ATree i)
registerStringLiteral s = gets (addLiteral ty (HT.TokenLCNums 1 1, HT.TKString bytes))
    >>= \case
        Right (n, scp) -> n <$ put scp
        Left err -> fail $ T.unpack $ fst err
    where
        bytes = s
        ty = CT.SCAuto $ CT.CTArray (fromIntegral $ B.length bytes) CT.CTChar

registerGVar :: (Ord i, Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerGVar = registerVar addGVar

registerGVarWith :: (Ord i, Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> GVarInitWith i
    -> Parser i (ATree i)
registerGVarWith ty ident to = gets (addGVarWith ty (tmpTKIdent ident) to)
    >>= \case
        Right (_, scp) -> ATEmpty <$ put scp
        Left err -> fail $ T.unpack $ fst err

registerFunc :: (Bits i, Integral i)
    => Bool
    -> Bool
    -> CT.StorageClass i
    -> T.Text
    -> Parser i ()
registerFunc isDefined isImplicit ty ident = gets (addFunction isDefined isImplicit ty (tmpTKIdent ident))
    >>= \case
        Right scp -> put scp
        Left err -> fail $ T.unpack $ fst err

registerTypedef :: (Eq i, Num i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i ()
registerTypedef ty ident = gets (addTypedef ty (tmpTKIdent ident))
    >>= \case
        Right scp -> put scp
        Left err -> fail $ T.unpack $ fst err

captureFunctionParamScopes :: Parser i a -> Parser i (a, [FunctionParamScope i])
captureFunctionParamScopes parser = do
    initialDepth <- gets (length . functionParamScopes)
    result <- parser
    scopes <- gets functionParamScopes
    let newCount = length scopes - initialDepth
        (newScopes, remainingScopes) = splitAt newCount scopes
    modify $ \cd -> cd { functionParamScopes = remainingScopes }
    pure (result, newScopes)

decayExprType :: Ord i => CT.StorageClass i -> CT.StorageClass i
decayExprType ty = case CT.toTypeKind ty of
    CT.CTArray _ _                     -> decayArrayType ty
    CT.CTIncomplete (CT.IncompleteArray elemTy) -> CT.mapTypeKind (const $ CT.CTPtr elemTy) ty
    CT.CTFunc _ _                      -> CT.mapTypeKind CT.CTPtr ty
    _                                  -> ty
    where
        decayArrayType arrTy = maybe arrTy (CT.mapTypeKind CT.CTPtr) $ CT.deref arrTy

conditionalResultType :: (Ord i, Bits i, Integral i) => ATree i -> ATree i -> Maybe (CT.StorageClass i)
conditionalResultType lhs rhs
    | isPointerType lhsTy || isPointerType rhsTy =
        nullPointerConditionalType lhsTy rhs
            <|> nullPointerConditionalType rhsTy lhs
            <|> mergePointerConditionalType lhsTy rhsTy
    | isAggregateType lhsTy || isAggregateType rhsTy =
        mergeCompatibleStorageClasses lhsTy rhsTy
            <|> mergeCompatibleStorageClasses rhsTy lhsTy
    | otherwise =
        Just $ CT.conversion lhsTy rhsTy
    where
        lhsTy = decayExprType $ atype lhs
        rhsTy = decayExprType $ atype rhs

        isAggregateType ty =
            CT.isCTStruct ty || CT.isIncompleteStruct ty

        mergePointerConditionalType lTy rTy
            | isPointerType lTy && isPointerType rTy =
                voidObjectPointerConditionalType lTy rTy
                    <|> voidObjectPointerConditionalType rTy lTy
                    <|> mergeCompatibleStorageClasses lTy rTy
                    <|> mergeCompatibleStorageClasses rTy lTy
            | otherwise =
                Nothing

        voidObjectPointerConditionalType voidTy objectTy
            | isVoidPointerType voidTy && isObjectPointerType objectTy =
                Just voidTy
            | otherwise =
                Nothing

        nullPointerConditionalType pointerTy expr
            | isPointerType pointerTy && isConditionalNullPointerConstantFor pointerTy expr =
                Just pointerTy
            | otherwise =
                Nothing

        isPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr _ -> True
            _          -> False

        isFunctionPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr (CT.CTFunc _ _) -> True
            _                        -> False

        isVoidPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr CT.CTVoid -> True
            _                  -> False

        isObjectPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr (CT.CTFunc _ _) -> False
            CT.CTPtr _               -> True
            _                        -> False

        mergeCompatibleStorageClasses lTy rTy =
            CT.SCAuto <$> CT.mergeCompatibleTypeKinds (CT.toTypeKind lTy) (CT.toTypeKind rTy)

        isConditionalNullPointerConstantFor pointerTy expr =
            isZeroIntegerNullPointerConstant expr
                || (isObjectPointerType pointerTy && isTypedNullPointerConstant isObjectPointerType expr)
                || (isFunctionPointerType pointerTy && isTypedNullPointerConstant isFunctionNullPointerCastType expr)

        isFunctionNullPointerCastType ty =
            isVoidPointerType ty || isFunctionPointerType ty

        isZeroIntegerNullPointerConstant = \case
            ATNode (ATNull inner) _ _ _ ->
                isZeroIntegerNullPointerConstant inner
            ATNode ATExprStmt _ inner _ ->
                isZeroIntegerNullPointerConstant inner
            expr ->
                isZeroIntegerConstexpr expr

        isTypedNullPointerConstant matchesTy = \case
            ATNode (ATNull inner) _ _ _ ->
                isTypedNullPointerConstant matchesTy inner
            ATNode ATExprStmt _ inner _ ->
                isTypedNullPointerConstant matchesTy inner
            ATNode ATCast ty inner _
                | matchesTy ty ->
                    isZeroIntegerNullPointerConstant inner
            _ ->
                False

        isZeroIntegerConstexpr expr =
            either (const False) (== 0) $ evalIntegerConstexprTree expr

isNullPointerConstant :: (Bits i, Integral i) => ATree i -> Bool
isNullPointerConstant = \case
    ATNode (ATNull inner) _ _ _ ->
        isNullPointerConstant inner
    ATNode ATExprStmt _ inner _ ->
        isNullPointerConstant inner
    ATNode ATCast ty inner _
        | isVoidPointerType ty ->
            isZeroIntegerConstexpr inner
    expr ->
        isZeroIntegerConstexpr expr
    where
        isVoidPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr CT.CTVoid -> True
            _                  -> False

        isZeroIntegerConstexpr expr =
            either (const False) (== 0) $ evalIntegerConstexprTree expr

functionDesignatorSourcePointerType :: Ord i => ATree i -> Maybe (CT.StorageClass i)
functionDesignatorSourcePointerType at@(ATNode _ ty _ _)
    | carriesFunctionDesignatorValue at = functionPointerValueType at
    | otherwise = Nothing
functionDesignatorSourcePointerType _ = Nothing

functionPointerValueType :: Ord i => ATree i -> Maybe (CT.StorageClass i)
functionPointerValueType (ATNode _ ty _ _) = case CT.toTypeKind ty of
    CT.CTFunc _ _            -> Just $ decayExprType ty
    CT.CTPtr (CT.CTFunc _ _) -> Just ty
    _                        -> Nothing
functionPointerValueType _ = Nothing

isVoidPointerType :: CT.StorageClass i -> Bool
isVoidPointerType ty = case CT.toTypeKind ty of
    CT.CTPtr CT.CTVoid -> True
    _                  -> False

isObjectPointerType :: CT.StorageClass i -> Bool
isObjectPointerType ty = case CT.toTypeKind ty of
    CT.CTPtr (CT.CTFunc _ _) -> False
    CT.CTPtr _               -> True
    _                        -> False

objectPointerTypesCompatible :: Ord i => CT.StorageClass i -> CT.StorageClass i -> Bool
objectPointerTypesCompatible targetTy sourceTy =
    isVoidPointerType targetTy
        || isVoidPointerType sourceTy
        || compatibleKinds (CT.toTypeKind targetTy) (CT.toTypeKind sourceTy)
        || compatibleKinds (CT.toTypeKind sourceTy) (CT.toTypeKind targetTy)
    where
        compatibleKinds lhs rhs =
            isJust $ CT.mergeCompatibleTypeKinds lhs rhs

isInvalidObjectPointerValue :: (Ord i, Bits i, Integral i) => CT.StorageClass i -> ATree i -> Bool
isInvalidObjectPointerValue targetTy expr
    | not (isObjectPointerType targetTy) = False
    | isNullPointerConstant expr = False
    | otherwise = not $ any (maybe False isCompatibleObjectPointerSource) sourceTypes
    where
        sourceTypes =
            [Just $ decayExprType $ atype expr]

        isCompatibleObjectPointerSource sourceTy = case CT.toTypeKind sourceTy of
            CT.CTPtr (CT.CTFunc _ _) ->
                False
            CT.CTPtr _ ->
                objectPointerTypesCompatible targetTy sourceTy
            _ ->
                False

isInvalidAggregateValueConversion :: Eq i => CT.StorageClass i -> ATree i -> Bool
isInvalidAggregateValueConversion targetTy expr =
    (isStructLikeValueType targetTy || isStructLikeValueType exprTy)
        && not (compatibleKinds (CT.toTypeKind targetTy) (CT.toTypeKind exprTy))
        && not (compatibleKinds (CT.toTypeKind exprTy) (CT.toTypeKind targetTy))
    where
        exprTy = atype expr

        isStructLikeValueType ty =
            CT.isCTStruct ty || CT.isIncompleteStruct ty

        compatibleKinds lhs rhs =
            isJust $ CT.mergeCompatibleTypeKinds lhs rhs

isInvalidFunctionPointerValue :: (Ord i, Bits i, Integral i) => CT.StorageClass i -> ATree i -> Bool
isInvalidFunctionPointerValue targetTy at
    | hasInvalidFunctionPointerCast at = isFunctionPointerType targetTy
    | otherwise = case inferredFunctionPointerValueType at of
        Just sourcePtrTy ->
            not $ isCompatibleFunctionPointerType targetTy sourcePtrTy
        Nothing ->
            isFunctionPointerType targetTy && not (isNullPointerConstant at)
    where
        isFunctionPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr (CT.CTFunc _ _) -> True
            _                        -> False

        isVoidPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr CT.CTVoid -> True
            _                  -> False

        isNullPointerCastType ty = isVoidPointerType ty || isFunctionPointerType ty

        isNullPointerConstant = \case
            ATNode (ATNull inner) _ _ _ ->
                isNullPointerConstant inner
            ATNode ATExprStmt _ inner _ ->
                isNullPointerConstant inner
            ATNode ATCast ty inner _
                | isNullPointerCastType ty ->
                    isZeroIntegerConstexpr inner
            expr ->
                isZeroIntegerConstexpr expr

        isZeroIntegerConstexpr expr =
            either (const False) (== 0) $ evalIntegerConstexprTree expr

        inferredFunctionPointerValueType expr =
            if isNullPointerConstant expr
                then Nothing
                else case functionPointerValueType expr of
                    Just ty ->
                        Just ty
                    Nothing -> case expr of
                        ATNode (ATNull inner) _ _ _ ->
                            inferredFunctionPointerValueType inner
                        ATNode ATExprStmt _ inner _ ->
                            inferredFunctionPointerValueType inner
                        ATNode ATComma _ _ rhs ->
                            inferredFunctionPointerValueType rhs
                        ATNode (ATConditional cond ATEmpty el) _ _ _ ->
                            conditionalFunctionPointerValueType cond el
                        ATNode (ATConditional _ th el) _ _ _ ->
                            conditionalFunctionPointerValueType th el
                        ATNode (ATStmtExpr stmts) _ _ _ ->
                            inferredFunctionPointerValueType =<< lastMaybe stmts
                        _ ->
                            Nothing

        conditionalFunctionPointerValueType lhs rhs = case (inferredFunctionPointerValueType lhs, inferredFunctionPointerValueType rhs) of
            (Just lhsTy, Just rhsTy)
                | functionPointerTypesCompatible lhsTy rhsTy ->
                    Just lhsTy
            (Just lhsTy, Nothing)
                | isNullPointerConstant rhs ->
                    Just lhsTy
            (Nothing, Just rhsTy)
                | isNullPointerConstant lhs ->
                    Just rhsTy
            _ ->
                Nothing

        functionPointerTypesCompatible lhsTy rhsTy =
            isCompatibleFunctionPointerType lhsTy rhsTy
                || isCompatibleFunctionPointerType rhsTy lhsTy

        isCompatibleFunctionPointerType targetTy' sourcePtrTy =
            isFunctionPointerType targetTy'
                &&
                ( isJust
                    (CT.mergeCompatibleTypeKinds
                        (CT.toTypeKind targetTy')
                        (CT.toTypeKind sourcePtrTy)
                    )
                || oldStyleFunctionPointerCompatible
                    (CT.toTypeKind targetTy')
                    (CT.toTypeKind sourcePtrTy)
                )

        oldStyleFunctionPointerCompatible
            (CT.CTPtr (CT.CTFunc targetRet targetParams))
            (CT.CTPtr (CT.CTFunc sourceRet sourceParams)) =
                isJust (CT.mergeCompatibleTypeKinds targetRet sourceRet)
                    &&
                    (oldStyleParamListCompatible targetParams sourceParams
                        || oldStyleParamListCompatible sourceParams targetParams)
        oldStyleFunctionPointerCompatible _ _ = False

        oldStyleParamListCompatible [] params =
            all oldStyleCallCompatibleParamType $ normalizedFunctionParamKinds params
        oldStyleParamListCompatible _ _ = False

        normalizedFunctionParamKinds [(CT.CTVoid, Nothing)] = []
        normalizedFunctionParamKinds params =
            map (normalizeFunctionParamType . fst) params

        normalizeFunctionParamType = \case
            CT.CTArray _ elemTy -> CT.CTPtr elemTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) -> CT.CTPtr elemTy
            CT.CTFunc retTy params -> CT.CTPtr $ CT.CTFunc retTy params
            ty -> ty

        oldStyleCallCompatibleParamType ty =
            isJust (CT.mergeCompatibleTypeKinds ty promotedTy)
            where
                promotedTy = CT.integerPromotedTypeKind ty

        hasInvalidFunctionPointerCast = \case
            ATNode ATCast ty inner _
                | carriesFunctionDesignatorValue inner ->
                    not (isFunctionPointerType ty) || hasInvalidFunctionPointerCast inner
            ATNode ATAddr _ inner _ ->
                hasInvalidFunctionPointerCast inner
            ATNode (ATNull inner) _ _ _ ->
                hasInvalidFunctionPointerCast inner
            ATNode ATExprStmt _ inner _ ->
                hasInvalidFunctionPointerCast inner
            ATNode ATComma _ _ rhs ->
                hasInvalidFunctionPointerCast rhs
            ATNode (ATConditional cond ATEmpty el) _ _ _ ->
                any hasInvalidFunctionPointerCast [cond, el]
            ATNode (ATConditional _ th el) _ _ _ ->
                any hasInvalidFunctionPointerCast [th, el]
            ATNode (ATStmtExpr stmts) _ _ _ ->
                maybe False hasInvalidFunctionPointerCast (lastMaybe stmts)
            _ ->
                False

        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

evalIntegerConstexprTree :: (Bits i, Integral i) => ATree i -> Either String i
evalIntegerConstexprTree = \case
    ATNode k ty lhs rhs -> case k of
        ATAdd -> binop (+)
        ATSub -> binop (-)
        ATMul -> binop (*)
        ATDiv -> nonZeroBinop quot
        ATMod -> nonZeroBinop rem
        ATAnd -> binop (.&.)
        ATXor -> binop xor
        ATOr -> binop (.|.)
        ATShl -> shiftBinop shiftL
        ATShr -> shiftBinop shiftR
        ATEQ -> binop (fromBool .: (==))
        ATNEQ -> binop (fromBool .: (/=))
        ATLT -> binop (fromBool .: (<))
        ATGT -> binop (fromBool .: (>))
        ATLEQ -> binop (fromBool .: (<=))
        ATGEQ -> binop (fromBool .: (>=))
        ATConditional cond th el ->
            evalIntegerConstexprTree cond >>= \cond' ->
                if cond' == 0
                    then evalIntegerConstexprTree el
                    else evalIntegerConstexprTree $
                        case th of
                            ATEmpty -> cond
                            _       -> th
        ATNot -> fromBool . (== 0) <$> evalIntegerConstexprTree lhs
        ATBitNot -> complement <$> evalIntegerConstexprTree lhs
        ATLAnd -> evalIntegerConstexprTree lhs >>= logicalAnd
        ATLOr -> evalIntegerConstexprTree lhs >>= logicalOr
        ATSizeof -> memOp "sizeof" CT.sizeof lhs
        ATAlignof -> memOp "_Alignof" CT.alignof lhs
        ATCast
            | isConstexprArithmeticCastType ty -> applyConstexprCast ty <$> evalIntegerConstexprTree lhs
            | otherwise -> Left "not an integer constant expression"
        ATNum v -> pure v
        _ -> Left "not an integer constant expression"
      where
        binop f = evalIntegerConstexprTree lhs >>= \lhs' -> f lhs' <$> evalIntegerConstexprTree rhs
        shiftBinop f =
            evalIntegerConstexprTree lhs >>= \lhs' ->
                evalIntegerConstexprTree rhs >>= \rhs' ->
                    case shiftCount rhs' of
                        Nothing     -> Left "not an integer constant expression"
                        Just count' -> pure $ f lhs' count'
        shiftCount n
            | n < 0 = Nothing
            | toInteger n >= shiftWidth = Nothing
            | toInteger n > toInteger (maxBound :: Int) = Nothing
            | otherwise = Just $ fromIntegral n
            where
                shiftWidth = toInteger (CT.sizeof ty) * 8
        logicalAnd lhs'
            | lhs' == 0 = pure 0
            | otherwise = fromBool . (/= 0) <$> evalIntegerConstexprTree rhs
        logicalOr lhs'
            | lhs' /= 0 = pure 1
            | otherwise = fromBool . (/= 0) <$> evalIntegerConstexprTree rhs
        nonZeroBinop f =
            evalIntegerConstexprTree lhs >>= \lhs' ->
                evalIntegerConstexprTree rhs >>= \rhs' ->
                    if rhs' == 0
                        then Left "not an integer constant expression"
                        else pure (f lhs' rhs')
        memOp opName op expr
            | hasIncompleteObjectType (atype expr) =
                Left $ "invalid application of '" <> opName <> "' to incomplete type"
            | otherwise =
                pure $ fromIntegral $ op $ atype expr
        fromBool = fromIntegral . fromEnum
        (.:) f g x y = f (g x y)
    _ -> Left "not an integer constant expression"

containsEscapingStmtExprControlFlow :: Ord i => ATree i -> Bool
containsEscapingStmtExprControlFlow = containsStmtExprEscapingControlFlow True

containsNonReturnEscapingStmtExprControlFlow :: Ord i => ATree i -> Bool
containsNonReturnEscapingStmtExprControlFlow = containsStmtExprEscapingControlFlow False

containsStmtExprEscapingControlFlow :: Ord i => Bool -> ATree i -> Bool
containsStmtExprEscapingControlFlow returnEscapes = exprContains
    where
        exprContains ATEmpty = False
        exprContains (ATNode ATSizeof _ _ _) = False
        exprContains (ATNode ATAlignof _ _ _) = False
        exprContains (ATNode (ATStmtExpr stmts) _ lhs rhs) =
            stmtExprContainsEscape stmts
                || exprContains lhs
                || exprContains rhs
        exprContains (ATNode kind _ lhs rhs) =
            kindExprContains kind
                || exprContains lhs
                || exprContains rhs

        stmtExprContainsEscape stmts =
            any (stmtContainsEscape (localLabels stmts) 0 0) stmts

        stmtContainsEscape _ _ _ ATEmpty = False
        stmtContainsEscape _ _ _ (ATNode ATSizeof _ _ _) = False
        stmtContainsEscape _ _ _ (ATNode ATAlignof _ _ _) = False
        stmtContainsEscape labels breakDepth continueDepth (ATNode ATWhile _ cond body) =
            stmtContainsEscape labels breakDepth continueDepth cond
                || stmtContainsEscape labels (succ breakDepth) (succ continueDepth) body
        stmtContainsEscape labels breakDepth continueDepth (ATNode kind _ lhs rhs) =
            escapingKind labels breakDepth continueDepth kind
                || kindStmtContainsEscape labels breakDepth continueDepth kind
                || stmtContainsEscape labels breakDepth continueDepth lhs
                || stmtContainsEscape labels breakDepth continueDepth rhs

        escapingKind labels breakDepth continueDepth = \case
            ATBreak ->
                breakDepth == 0
            ATContinue ->
                continueDepth == 0
            ATGoto ident ->
                not $ ident `S.member` labels
            ATReturn ->
                returnEscapes
            _ ->
                False

        kindStmtContainsEscape labels breakDepth continueDepth = \case
            ATConditional cond tr fl ->
                any (stmtContainsEscape labels breakDepth continueDepth) [cond, tr, fl]
            ATSwitch cond cases ->
                stmtContainsEscape labels breakDepth continueDepth cond
                    || any (stmtContainsEscape labels (succ breakDepth) continueDepth) cases
            ATFor kinds ->
                any (forContainsEscape labels (succ breakDepth) (succ continueDepth)) kinds
            ATBlock stmts ->
                any (stmtContainsEscape labels breakDepth continueDepth) stmts
            ATStmtExpr stmts ->
                stmtExprContainsEscape stmts
            ATNull at ->
                stmtContainsEscape labels breakDepth continueDepth at
            ATDefFunc _ maybeArgs ->
                maybe False (any exprContains) maybeArgs
            ATCallFunc _ maybeArgs ->
                maybe False (any exprContains) maybeArgs
            ATCallPtr maybeArgs ->
                maybe False (any exprContains) maybeArgs
            _ ->
                False

        forContainsEscape labels breakDepth continueDepth = \case
            ATForkw ->
                False
            ATForInit at ->
                stmtContainsEscape labels breakDepth continueDepth at
            ATForCond at ->
                stmtContainsEscape labels breakDepth continueDepth at
            ATForIncr at ->
                stmtContainsEscape labels breakDepth continueDepth at
            ATForStmt at ->
                stmtContainsEscape labels breakDepth continueDepth at

        kindExprContains = \case
            ATConditional cond tr fl ->
                any exprContains [cond, tr, fl]
            ATSwitch cond cases ->
                exprContains cond || any exprContains cases
            ATFor kinds ->
                any forExprContains kinds
            ATBlock stmts ->
                any exprContains stmts
            ATStmtExpr stmts ->
                stmtExprContainsEscape stmts
            ATNull at ->
                exprContains at
            ATDefFunc _ maybeArgs ->
                maybe False (any exprContains) maybeArgs
            ATCallFunc _ maybeArgs ->
                maybe False (any exprContains) maybeArgs
            ATCallPtr maybeArgs ->
                maybe False (any exprContains) maybeArgs
            _ ->
                False

        forExprContains = \case
            ATForkw ->
                False
            ATForInit at ->
                exprContains at
            ATForCond at ->
                exprContains at
            ATForIncr at ->
                exprContains at
            ATForStmt at ->
                exprContains at

        localLabels =
            S.unions . map labelsInTree

        labelsInTree ATEmpty = S.empty
        labelsInTree (ATNode kind _ lhs rhs) = case kind of
            ATStmtExpr _ ->
                S.empty
            ATCallFunc _ _ ->
                S.empty
            ATCallPtr _ ->
                S.empty
            _ ->
                labelsInKind kind
                    <> labelsInTree lhs
                    <> labelsInTree rhs

        labelsInKind = \case
            ATLabel ident ->
                S.singleton ident
            ATConditional cond tr fl ->
                S.unions $ map labelsInTree [cond, tr, fl]
            ATSwitch cond cases ->
                S.unions $ labelsInTree cond : map labelsInTree cases
            ATFor kinds ->
                S.unions $ map labelsInFor kinds
            ATBlock stmts ->
                S.unions $ map labelsInTree stmts
            ATNull at ->
                labelsInTree at
            _ ->
                S.empty

        labelsInFor = \case
            ATForkw ->
                S.empty
            ATForInit at ->
                labelsInTree at
            ATForCond at ->
                labelsInTree at
            ATForIncr at ->
                labelsInTree at
            ATForStmt at ->
                labelsInTree at

hasInvalidStmtExprControlFlow :: Ord i => ATree i -> Bool
hasInvalidStmtExprControlFlow ast =
    containsStmtExprEscapingControlFlow False ast
        || hasGotoIntoStmtExpr ast
        || hasSwitchLabelIntoStmtExpr ast
    where
        hasGotoIntoStmtExpr tree =
            any jumpsIntoStmtExpr gotos
            where
                (_, labels, gotos) = collect True Nothing 0 tree

                jumpsIntoStmtExpr (ident, currentScope, isEmittedGoto) =
                    isEmittedGoto
                        && any
                            ( \case
                                (label, Just labelScope, _) ->
                                    label == ident && Just labelScope /= currentScope
                                _ ->
                                    False
                            )
                            labels

        collect isEmitted scope nextId ATEmpty = (nextId, [], [])
        collect isEmitted scope nextId (ATNode ATSizeof _ lhs _) =
            collect False scope nextId lhs
        collect isEmitted scope nextId (ATNode ATAlignof _ lhs _) =
            collect False scope nextId lhs
        collect isEmitted scope nextId (ATNode kind _ lhs rhs) =
            let
                (nextId', labels, gotos) = collectKind isEmitted scope nextId kind
                (nextId'', lhsLabels, lhsGotos) = collect isEmitted scope nextId' lhs
                (nextId''', rhsLabels, rhsGotos) = collect isEmitted scope nextId'' rhs
            in
                ( nextId'''
                , labels <> lhsLabels <> rhsLabels
                , gotos <> lhsGotos <> rhsGotos
                )

        collectKind isEmitted scope nextId = \case
            ATLabel ident ->
                (nextId, [(ident, scope, isEmitted)], [])
            ATGoto ident ->
                (nextId, [], [(ident, scope, isEmitted)])
            ATConditional cond th el ->
                collectList isEmitted scope nextId [cond, th, el]
            ATSwitch cond cases ->
                collectList isEmitted scope nextId (cond : cases)
            ATFor clauses ->
                collectList isEmitted scope nextId $ map fromATKindFor clauses
            ATBlock stmts ->
                collectList isEmitted scope nextId stmts
            ATStmtExpr stmts ->
                collectList isEmitted (Just nextId) (succ nextId) stmts
            ATNull at ->
                collect isEmitted scope nextId at
            ATDefFunc _ maybeArgs ->
                maybe (nextId, [], []) (collectList isEmitted scope nextId) maybeArgs
            ATCallFunc _ maybeArgs ->
                maybe (nextId, [], []) (collectList isEmitted scope nextId) maybeArgs
            ATCallPtr maybeArgs ->
                maybe (nextId, [], []) (collectList isEmitted scope nextId) maybeArgs
            _ ->
                (nextId, [], [])

        collectList isEmitted scope nextStart =
            foldl
                ( \(nextId, labels, gotos) expr ->
                    let
                        (nextId', labels', gotos') = collect isEmitted scope nextId expr
                    in
                        (nextId', labels <> labels', gotos <> gotos')
                )
                (nextStart, [], [])

        hasSwitchLabelIntoStmtExpr tree =
            invalid
            where
                (_, invalid) = walk Nothing Nothing 0 tree

        walk _ _ nextId ATEmpty = (nextId, False)
        walk _ _ nextId (ATNode ATSizeof _ _ _) = (nextId, False)
        walk _ _ nextId (ATNode ATAlignof _ _ _) = (nextId, False)
        walk stmtExprScope switchStmtExprScope nextId (ATNode kind _ lhs rhs) =
            let
                (nextId', invalidKind) = walkKind stmtExprScope switchStmtExprScope nextId kind
                (nextId'', invalidLhs) = walk stmtExprScope switchStmtExprScope nextId' lhs
                (nextId''', invalidRhs) = walk stmtExprScope switchStmtExprScope nextId'' rhs
            in
                (nextId''', invalidKind || invalidLhs || invalidRhs)

        walkKind stmtExprScope switchStmtExprScope nextId = \case
            ATCase _ _ ->
                (nextId, stmtExprScope /= switchStmtExprScope)
            ATDefault _ ->
                (nextId, stmtExprScope /= switchStmtExprScope)
            ATConditional cond th el ->
                walkList stmtExprScope switchStmtExprScope nextId [cond, th, el]
            ATSwitch cond cases ->
                let
                    (nextId', invalidCond) = walk stmtExprScope switchStmtExprScope nextId cond
                    (nextId'', invalidCases) = walkList stmtExprScope stmtExprScope nextId' cases
                in
                    (nextId'', invalidCond || invalidCases)
            ATFor clauses ->
                walkList stmtExprScope switchStmtExprScope nextId $ map fromATKindFor clauses
            ATBlock stmts ->
                walkList stmtExprScope switchStmtExprScope nextId stmts
            ATStmtExpr stmts ->
                walkList (Just nextId) switchStmtExprScope (succ nextId) stmts
            ATNull at ->
                walk stmtExprScope switchStmtExprScope nextId at
            ATDefFunc _ maybeArgs ->
                maybe (nextId, False) (walkList stmtExprScope switchStmtExprScope nextId) maybeArgs
            ATCallFunc _ maybeArgs ->
                maybe (nextId, False) (walkList stmtExprScope switchStmtExprScope nextId) maybeArgs
            ATCallPtr maybeArgs ->
                maybe (nextId, False) (walkList stmtExprScope switchStmtExprScope nextId) maybeArgs
            _ ->
                (nextId, False)

        walkList stmtExprScope switchStmtExprScope nextStart =
            foldl
                ( \(nextId, invalid) expr ->
                    let
                        (nextId', invalid') = walk stmtExprScope switchStmtExprScope nextId expr
                    in
                        (nextId', invalid || invalid')
                )
                (nextStart, False)

isConstexprArithmeticCastType :: CT.StorageClass i -> Bool
isConstexprArithmeticCastType = \case
    CT.SCAuto ty     -> go ty
    CT.SCRegister ty -> go ty
    CT.SCStatic ty   -> go ty
    CT.SCUndef ty    -> go ty
    where
        go = \case
            CT.CTInt               -> True
            CT.CTChar              -> True
            CT.CTBool              -> True
            CT.CTEnum _ _          -> True
            CT.CTSigned CT.CTUndef -> True
            CT.CTShort CT.CTUndef  -> True
            CT.CTLong CT.CTUndef   -> True
            CT.CTSigned ty         -> go ty
            CT.CTShort ty          -> go ty
            CT.CTLong ty           -> go ty
            _                      -> False

applyConstexprCast :: (Bits i, Integral i) => CT.StorageClass i -> i -> i
applyConstexprCast ty val
    | CT.toTypeKind ty == CT.CTBool = fromIntegral $ fromEnum $ val /= 0
    | otherwise = truncateToWidth (CT.sizeof ty) val
    where
        truncateToWidth sz x
            | sz == 0 = 0
            | otherwise = fromInteger $ signExtend width $ toInteger x
            where
                width = fromIntegral $ sz * 8

        signExtend width x =
            if width <= 0
                then 0
                else
                    let modulus = bit width :: Integer
                        mask = pred modulus
                        truncated = x .&. mask
                        signBit = bit (pred width) :: Integer
                    in
                        if truncated .&. signBit == 0
                            then truncated
                            else truncated - modulus

isInvalidFunctionPointerInitializer :: (Ord i, Bits i, Integral i) => CT.StorageClass i -> ATree i -> Bool
isInvalidFunctionPointerInitializer = isInvalidFunctionPointerValue

carriesFunctionDesignatorValue :: ATree i -> Bool
carriesFunctionDesignatorValue = \case
    ATNode (ATFuncPtr _) _ _ _ ->
        True
    ATNode ATAddr _ inner _ ->
        carriesFunctionDesignatorValue inner
    ATNode ATCast _ inner _ ->
        carriesFunctionDesignatorValue inner
    ATNode (ATNull inner) _ _ _ ->
        carriesFunctionDesignatorValue inner
    ATNode ATExprStmt _ inner _ ->
        carriesFunctionDesignatorValue inner
    ATNode ATComma _ _ rhs ->
        carriesFunctionDesignatorValue rhs
    ATNode (ATConditional cond ATEmpty el) _ _ _ ->
        any carriesFunctionDesignatorValue [cond, el]
    ATNode (ATConditional _ th el) _ _ _ ->
        any carriesFunctionDesignatorValue [th, el]
    ATNode (ATStmtExpr stmts) _ _ _ ->
        maybe False carriesFunctionDesignatorValue (lastMaybe stmts)
    _ ->
        False
    where
        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

requiresUnsupportedNonAddressableArrayDecay :: Ord i => ATree i -> Bool
requiresUnsupportedNonAddressableArrayDecay = requiresUnsupportedValueRead
    where
        requiresUnsupportedValueRead = \case
            n@(ATNode (ATMemberAcc _) ty lhs _)
                | isNonAddressableAggregateArrayElementAccess lhs ->
                    True
                | Just (base, offset) <- rvalueSubobjectBaseOffset n ->
                    not $
                        isSupportedSmallRvalueBase base
                            && isSupportedRvalueScalarType ty
                            && offset + CT.sizeof ty <= CT.sizeof (atype base)
                            && not (requiresUnsupportedValueRead base)
                | otherwise ->
                    isUnsupportedStructValueType ty
                        || requiresUnsupportedAddressComputation lhs
            ATNode ATDeref ty ptr _
                | Just (arrayExpr, idx) <- pointerIndexOperands ptr
                , Just (base, _) <- rvalueSubobjectBaseOffset arrayExpr ->
                    requiresUnsupportedRvalueArrayElementRead ty base [idx]
            ATNode ATDeref ty ptr _
                | Just (arrayExpr, idxs) <- nonAddressableArrayMemberPointerIndexes ptr
                , Just (base, _) <- rvalueSubobjectBaseOffset arrayExpr ->
                    requiresUnsupportedRvalueArrayElementRead ty base idxs
            ATNode ATDeref ty lhs _ ->
                isUnsupportedStructValueType ty
                    || requiresUnsupportedValueRead lhs
            ATNode ATSizeof _ _ _ ->
                False
            ATNode ATAlignof _ _ _ ->
                False
            ATNode ATAddr _ lhs _ ->
                requiresUnsupportedAddressComputation lhs
            ATNode (ATLVar _ _) ty _ _ ->
                isUnsupportedStructValueType ty
            ATNode (ATGVar _ _) ty _ _ ->
                isUnsupportedStructValueType ty
            ATNode (ATCallFunc _ args) ty _ _ ->
                isUnsupportedStructValueType ty
                    || maybe False (any requiresUnsupportedValueRead) args
            ATNode (ATCallPtr args) ty callee _ ->
                isUnsupportedStructValueType ty
                    || requiresUnsupportedValueRead callee
                    || maybe False (any requiresUnsupportedValueRead) args
            ATNode ATAssign ty lhs rhs ->
                isUnsupportedStructValueType ty
                    || requiresUnsupportedAddressComputation lhs
                    || requiresUnsupportedValueRead rhs
            ATNode ATComma _ lhs rhs ->
                requiresUnsupportedValueRead lhs
                    || requiresUnsupportedValueRead rhs
            ATNode ATCast ty lhs _ ->
                isUnsupportedStructValueType ty
                    || requiresUnsupportedValueRead lhs
            ATNode (ATConditional cond ATEmpty el) _ _ _ ->
                requiresUnsupportedValueRead cond
                    || requiresUnsupportedValueRead el
            ATNode (ATConditional cond th el) _ _ _ ->
                requiresUnsupportedValueRead cond
                    || requiresUnsupportedValueRead th
                    || requiresUnsupportedValueRead el
            ATNode (ATNull _) _ _ _ ->
                False
            ATNode ATExprStmt _ inner _ ->
                requiresUnsupportedValueRead inner
            ATNode (ATStmtExpr stmts) _ _ _ ->
                maybe False requiresUnsupportedValueRead (lastMaybe stmts)
            ATNode kind _ lhs rhs ->
                requiresUnsupportedNonAddressableArrayDecayKind kind
                    || requiresUnsupportedValueRead lhs
                    || requiresUnsupportedValueRead rhs
            _ ->
                False

        requiresUnsupportedAddressComputation = \case
            ATNode (ATMemberAcc _) _ lhs _ ->
                requiresUnsupportedAddressComputation lhs
            ATNode ATDeref _ lhs _ ->
                requiresUnsupportedValueRead lhs
            ATNode ATExprStmt _ inner _ ->
                requiresUnsupportedAddressComputation inner
            _ ->
                False

        requiresUnsupportedNonAddressableArrayDecayKind = \case
            ATConditional cond th el ->
                any requiresUnsupportedValueRead [cond, th, el]
            ATSwitch cond cases ->
                requiresUnsupportedValueRead cond
                    || any requiresUnsupportedValueRead cases
            ATFor clauses ->
                any requiresUnsupportedNonAddressableArrayDecayFor clauses
            ATBlock stmts ->
                any requiresUnsupportedValueRead stmts
            ATStmtExpr stmts ->
                any requiresUnsupportedValueRead stmts
            ATDefFunc _ args ->
                maybe False (any requiresUnsupportedValueRead) args
            ATCallFunc _ args ->
                maybe False (any requiresUnsupportedValueRead) args
            ATCallPtr args ->
                maybe False (any requiresUnsupportedValueRead) args
            ATNull _ ->
                False
            _ ->
                False

        pointerIndexOperands (ATNode ATAddPtr _ arrayExpr idx) = Just (arrayExpr, idx)
        pointerIndexOperands (ATNode ATSubPtr _ arrayExpr idx) = Just (arrayExpr, idx)
        pointerIndexOperands _ = Nothing

        requiresUnsupportedRvalueArrayElementRead ty base idxs =
            containsNonReturnEscapingStmtExprControlFlow base
                || not (isSupportedSmallRvalueBase base)
                || not (isSupportedRvalueScalarType ty)
                || requiresUnsupportedValueRead base
                || any requiresUnsupportedValueRead idxs

        nonAddressableArrayMemberPointerIndexes (ATNode ATAddPtr _ lhs idx)
            | isNonAddressableArrayMemberExpr lhs = Just (lhs, [idx])
            | otherwise = do
                (arrayExpr, idxs) <- nonAddressableArrayMemberPointerIndexes lhs
                pure (arrayExpr, idxs <> [idx])
        nonAddressableArrayMemberPointerIndexes (ATNode ATSubPtr _ lhs idx)
            | isNonAddressableArrayMemberExpr lhs = Just (lhs, [idx])
            | otherwise = do
                (arrayExpr, idxs) <- nonAddressableArrayMemberPointerIndexes lhs
                pure (arrayExpr, idxs <> [idx])
        nonAddressableArrayMemberPointerIndexes expr
            | isNonAddressableArrayMemberExpr expr = Just (expr, [])
            | otherwise = Nothing

        isNonAddressableArrayMemberExpr (ATNode (ATMemberAcc _) ty lhs _) =
            CT.isArray ty && not (isAddressableExpr lhs)
        isNonAddressableArrayMemberExpr _ = False

        isNonAddressableAggregateArrayElementAccess (ATNode ATDeref ty ptr _)
            | Just (arrayExpr, _) <- pointerIndexOperands ptr =
                CT.isCTStruct ty && isNonAddressableArrayMemberExpr arrayExpr
        isNonAddressableAggregateArrayElementAccess _ = False

        rvalueSubobjectBaseOffset (ATNode (ATMemberAcc member) _ lhs _)
            | isAddressableExpr lhs = Nothing
            | otherwise = case rvalueSubobjectBaseOffset lhs of
                Just (base, offset) -> Just (base, offset + CT.smOffset member)
                Nothing             -> Just (lhs, CT.smOffset member)
        rvalueSubobjectBaseOffset _ = Nothing

        isSupportedSmallRvalueBase base = CT.sizeof (atype base) <= 8

        isUnsupportedStructValueType ty =
            CT.isIncompleteStruct ty
                || (CT.isCTStruct ty && CT.sizeof ty > 8)

        isSupportedRvalueScalarType ty =
            not (CT.isArray ty)
                && not (CT.isCTStruct ty)
                && case CT.toTypeKind ty of
                    CT.CTFunc _ _ -> False
                    _             -> True

        requiresUnsupportedNonAddressableArrayDecayFor = \case
            ATForInit expr -> requiresUnsupportedValueRead expr
            ATForCond expr -> requiresUnsupportedValueRead expr
            ATForIncr expr -> requiresUnsupportedValueRead expr
            ATForStmt expr -> requiresUnsupportedValueRead expr
            ATForkw        -> False

        isAddressableExpr (ATNode kind _ lhs _) = case kind of
            ATLVar _ _    -> True
            ATGVar _ _    -> True
            ATMemberAcc _ -> isAddressableExpr lhs
            ATDeref       -> isAddressableDerefOperand lhs
            _             -> False
        isAddressableExpr _ = False

        isAddressableDerefOperand ptr
            | Just (arrayExpr, _) <- pointerIndexOperands ptr
            , CT.isArray (atype arrayExpr) =
                isAddressableExpr arrayExpr
        isAddressableDerefOperand _ = True

        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

bracket :: Parser i a -> (a -> Parser i b) -> (a -> Parser i c) -> Parser i c
bracket beg end m = do
    b <- beg
    M.withRecovery (\err -> end b *> M.parseError err) (m b) <* end b

tmpTKIdent :: Num i => T.Text -> HT.TokenLC i
tmpTKIdent ident = (HT.TokenLCNums 1 1, HT.TKIdent ident)

getPosState :: Parser i (M.PosState T.Text)
getPosState = do
    statePos <- M.statePosState <$> M.getParserState
    srcPos <- M.getSourcePos
    pure $ statePos { M.pstateSourcePos = srcPos }
