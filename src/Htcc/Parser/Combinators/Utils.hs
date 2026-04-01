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
  , decayExprType
  , conditionalResultType
  , isNullPointerConstant
  , functionDesignatorSourcePointerType
  , isInvalidObjectPointerValue
  , isInvalidFunctionPointerValue
  , isInvalidFunctionPointerInitializer
  , bracket
  , getPosState
) where
import           Control.Applicative                             ((<|>))
import           Control.Monad.State                             (gets, put)
import           Control.Natural                                 (type (~>))
import           Data.Bits                                       (Bits (..))
import qualified Data.ByteString.UTF8                            as BSU
import           Data.Maybe                                      (isJust)
import qualified Data.Text                                       as T
import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (ATKind (..),
                                                                  ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.ConstructionData.Core               (ConstructionData,
                                                                  addFunction,
                                                                  addGVar,
                                                                  addGVarWith,
                                                                  addLVar,
                                                                  addLiteral)
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
    => String
    -> Parser i (ATree i)
registerStringLiteral s = gets (addLiteral ty (HT.TokenLCNums 1 1, HT.TKString $ BSU.fromString s))
    >>= \case
        Right (n, scp) -> n <$ put scp
        Left err -> fail $ T.unpack $ fst err
    where
        ty = CT.SCAuto $ CT.CTArray (fromIntegral $ length s) CT.CTChar

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

decayExprType :: Ord i => CT.StorageClass i -> CT.StorageClass i
decayExprType ty = case CT.toTypeKind ty of
    CT.CTArray _ _                     -> decayArrayType ty
    CT.CTIncomplete (CT.IncompleteArray elemTy) -> CT.mapTypeKind (const $ CT.CTPtr elemTy) ty
    CT.CTFunc _ _                      -> CT.mapTypeKind CT.CTPtr ty
    _                                  -> ty
    where
        decayArrayType arrTy = maybe arrTy (CT.mapTypeKind CT.CTPtr) $ CT.deref arrTy

conditionalResultType :: (Ord i, Bits i, Integral i) => ATree i -> ATree i -> Maybe (CT.StorageClass i)
conditionalResultType lhs rhs =
    if isPointerType lhsTy || isPointerType rhsTy
        then
            nullPointerConditionalType lhsTy rhs
                <|> nullPointerConditionalType rhsTy lhs
                <|> mergePointerConditionalType lhsTy rhsTy
        else
            Just $ CT.conversion lhsTy rhsTy
    where
        lhsTy = decayExprType $ atype lhs
        rhsTy = decayExprType $ atype rhs

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
    | otherwise = case CT.toTypeKind sourceTy of
        CT.CTPtr (CT.CTFunc _ _) ->
            True
        CT.CTPtr _ ->
            not $ objectPointerTypesCompatible targetTy sourceTy
        _ ->
            True
    where
        sourceTy = decayExprType $ atype expr

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
                            maybe Nothing inferredFunctionPointerValueType (lastMaybe stmts)
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
        ATShl -> binop (\l r -> shiftL l (fromIntegral r))
        ATShr -> binop (\l r -> shiftR l (fromIntegral r))
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
            | CT.isCTIncomplete (atype expr) =
                Left $ "invalid application of '" <> opName <> "' to incomplete type"
            | otherwise =
                pure $ fromIntegral $ op $ atype expr
        fromBool = fromIntegral . fromEnum
        (.:) f g x y = f (g x y)
    _ -> Left "not an integer constant expression"

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
