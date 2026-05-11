{-|
Module      : Htcc.Parser.Combinators.Program
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language Program parser
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}
module Htcc.Parser.Combinators.Program (
    parser
  , assign
  , conditional
  , compoundStmt
  , convertCallArgs
  , convertCallArgsWith
  , foldGlobalInitWith
  , isInvalidAggregateValueConversion
) where

import           Control.Monad                               (unless, void,
                                                              when, zipWithM,
                                                              (>=>))
import           Control.Monad.Combinators                   (choice)
import           Control.Monad.Extra                         (ifM)
import           Control.Monad.State                         (get, gets, modify)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Data.Bits                                   (Bits, bit,
                                                              complement,
                                                              shiftL, shiftR,
                                                              xor, (.&.), (.|.))
import           Data.Functor                                (($>), (<&>))
import           Data.List                                   (find, sortBy)
import           Data.Maybe                                  (fromJust,
                                                              fromMaybe, isJust,
                                                              listToMaybe)
import           Data.Ord                                    (comparing)
import qualified Data.Text                                   as T
import           Data.Tuple.Extra                            (dupe, first,
                                                              second)
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser.AST                             (Treealizable (..),
                                                              addKind,
                                                              isEmptyReturn,
                                                              isNonEmptyReturn,
                                                              subKind)
import           Htcc.Parser.AST.Core                        (ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              atBlock, atBreak,
                                                              atCase, atCast,
                                                              atConditional,
                                                              atContinue,
                                                              atDefFunc,
                                                              atDefault, atElse,
                                                              atExprStmt, atFor,
                                                              atGVar, atGoto,
                                                              atIf, atLVar,
                                                              atLabel,
                                                              atMemberAcc,
                                                              atNoLeaf, atNull,
                                                              atNumLit,
                                                              atReturn,
                                                              atSwitch, atUnary,
                                                              atWhile,
                                                              fromATKindFor)
import           Htcc.Parser.AST.Type                        (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.ConstExpr           (evalConstexpr)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Decl                (DeclStorage (..),
                                                              absDeclarator,
                                                              declarationSpec,
                                                              declarator,
                                                              declspec)
import qualified Htcc.Parser.Combinators.GNUExtensions       as GNU
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.Combinators.Utils               (bracket,
                                                              captureFunctionParamScopes,
                                                              conditionalResultType,
                                                              containsEscapingStmtExprControlFlow,
                                                              decayExprType,
                                                              getPosState,
                                                              hasInvalidStmtExprControlFlow,
                                                              isInvalidAggregateValueConversion,
                                                              isInvalidFunctionPointerInitializer,
                                                              isInvalidFunctionPointerValue,
                                                              isInvalidObjectPointerValue,
                                                              maybeToParser,
                                                              registerFunc,
                                                              registerGVar,
                                                              registerGVarWith,
                                                              registerLVar,
                                                              registerStringLiteral,
                                                              registerTypedef,
                                                              requiresUnsupportedNonAddressableArrayDecay)
import           Htcc.Parser.Combinators.Var                 (varInit)
import           Htcc.Parser.ConstructionData.Core           (ConstructionData (scope, suppressUnsupportedValueChecks),
                                                              FunctionParamScope (..),
                                                              fallBack,
                                                              hasIncompleteObjectType,
                                                              incomplete,
                                                              isSwitchStmt,
                                                              lookupFunction,
                                                              lookupGVar,
                                                              lookupLVar,
                                                              lookupVar,
                                                              normalizeCompletedStorageClass,
                                                              pushWarn,
                                                              resetLocal,
                                                              succNest)
import           Htcc.Parser.ConstructionData.Scope          (LookupVarResult (..),
                                                              Scoped (curNestDepth, curScopeId, enumerators, functions, structs))
import qualified Htcc.Parser.ConstructionData.Scope.Function as PSF
import qualified Htcc.Parser.ConstructionData.Scope.Var      as PV
import           Numeric.Natural                             (Natural)
import qualified Text.Megaparsec                             as M
import qualified Text.Megaparsec.Char                        as MC

import           Text.Megaparsec.Debug                       (dbg)

parser, program :: (Ord i, Integral i, Bits i, Read i, Show i) => Parser i (ASTs i)
parser = do
    asts <- spaceConsumer *> program <* M.eof
    rejectUnsupportedCompletedFunctionReturnTypes
    pure asts
program = M.many global

requireCompleteObjectType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
requireCompleteObjectType err ty = do
    resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
    if hasIncompleteObjectType resolvedTy
        then fail err
        else pure resolvedTy

isVoidObjectType :: CT.StorageClass i -> Bool
isVoidObjectType = go . CT.toTypeKind
    where
        go = \case
            CT.CTLong innerTy   -> go innerTy
            CT.CTShort innerTy  -> go innerTy
            CT.CTSigned innerTy -> go innerTy
            CT.CTArray _ innerTy -> go innerTy
            CT.CTIncomplete (CT.IncompleteArray innerTy) -> go innerTy
            CT.CTVoid           -> True
            _                   -> False

requireNonVoidObjectType :: String -> CT.StorageClass i -> Parser i (CT.StorageClass i)
requireNonVoidObjectType err ty
    | isVoidObjectType ty = fail err
    | otherwise = pure ty

requireSupportedByValueType :: Ord i => String -> CT.StorageClass i -> Parser i (CT.StorageClass i)
requireSupportedByValueType err ty
    | isUnsupportedByValueAggregateType ty = fail err
    | otherwise = pure ty

isUnsupportedByValueAggregateType :: Ord i => CT.StorageClass i -> Bool
isUnsupportedByValueAggregateType ty =
    CT.isCTStruct ty && CT.sizeof ty > 8

rejectUnsupportedCompletedFunctionReturnTypes :: Ord i => Parser i ()
rejectUnsupportedCompletedFunctionReturnTypes = do
    hasUnsupportedReturn <- gets $ \cd ->
        any (definedFunctionReturnsUnsupported cd) (functions $ scope cd)
    when hasUnsupportedReturn $
        fail "unsupported by-value function return type"
    where
        definedFunctionReturnsUnsupported cd fn =
            PSF.fnDefined fn
                && functionReturnsUnsupported
                    (normalizeCompletedStorageClass cd $ PSF.fntype fn)

        functionReturnsUnsupported fnTy = case CT.toTypeKind fnTy of
            CT.CTFunc retTy _ ->
                isUnsupportedByValueAggregateType $ CT.SCAuto retTy
            _ ->
                False

functionParamDecls :: Eq i => CT.StorageClass i -> Parser i [(CT.StorageClass i, Maybe T.Text)]
functionParamDecls ty = case CT.toTypeKind ty of
    CT.CTFunc _ params -> pure
        [ (CT.SCAuto $ canonicalizeFunctionParamType paramTy, ident)
        | (paramTy, ident) <- params
        , paramTy /= CT.CTVoid
        ]
    _ -> fail "expected function parameters"

unnamedFunctionParamIdent :: Int -> T.Text
unnamedFunctionParamIdent idx =
    "$htcc_unnamed_param_" <> T.pack (show idx)

requireInitializedObjectType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
requireInitializedObjectType err baseTy ty = do
    resolvedBaseTy <- gets (`normalizeCompletedStorageClass` baseTy)
    resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
    if hasInitializerCompletableArrayType resolvedTy
        && not (addsOuterArrayLayerBeforeExistingOmittedBound resolvedBaseTy resolvedTy)
        then pure resolvedTy
        else
            if hasIncompleteObjectType resolvedTy
                then fail err
                else pure resolvedTy

requireExternDeclObjectType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
requireExternDeclObjectType err ty = do
    resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
    if hasInvalidExternArrayElementType resolvedTy
        then fail err
        else pure resolvedTy
    where
        hasInvalidExternArrayElementType = goTop . CT.toTypeKind

        goTop = \case
            CT.CTArray _ innerTy ->
                hasIncompleteArrayElementType innerTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) ->
                hasIncompleteArrayElementType elemTy
            _ ->
                False

        hasIncompleteArrayElementType = \case
            CT.CTArray _ innerTy ->
                hasIncompleteArrayElementType innerTy
            CT.CTIncomplete _ ->
                True
            _ ->
                False

requireTypedefDeclType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
requireTypedefDeclType err ty = do
    resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
    if hasInvalidTypedefArrayElementType resolvedTy
        then fail err
        else pure resolvedTy
    where
        hasInvalidTypedefArrayElementType = goTop . CT.toTypeKind

        goTop = \case
            CT.CTArray _ innerTy ->
                hasInvalidArrayElementType innerTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) ->
                hasInvalidArrayElementType elemTy
            _ ->
                False

        hasInvalidArrayElementType = \case
            CT.CTArray _ innerTy ->
                hasInvalidArrayElementType innerTy
            CT.CTIncomplete _ ->
                True
            innerTy ->
                isVoidTypeKind innerTy

        isVoidTypeKind = \case
            CT.CTLong innerTy ->
                isVoidTypeKind innerTy
            CT.CTShort innerTy ->
                isVoidTypeKind innerTy
            CT.CTSigned innerTy ->
                isVoidTypeKind innerTy
            CT.CTVoid ->
                True
            _ ->
                False

resolveDerefObjectType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
resolveDerefObjectType err ty
    | isTopLevelOmittedBoundArrayType ty = pure ty
    | otherwise = gets (incomplete ty) >>= maybeToParser err

decayIncompleteArrayExpr :: Ord i => ATree i -> ATree i
decayIncompleteArrayExpr expr
    | isTopLevelOmittedBoundArrayType (atype expr) = atCast (decayExprType $ atype expr) expr
    | otherwise = expr

isTopLevelOmittedBoundArrayType :: CT.StorageClass i -> Bool
isTopLevelOmittedBoundArrayType ty = case CT.toTypeKind ty of
    CT.CTIncomplete (CT.IncompleteArray _) -> True
    _                                      -> False

hasInitializerCompletableArrayType :: CT.StorageClass i -> Bool
hasInitializerCompletableArrayType = go . CT.toTypeKind
    where
        go = \case
            CT.CTLong innerTy   -> go innerTy
            CT.CTShort innerTy  -> go innerTy
            CT.CTSigned innerTy -> go innerTy
            CT.CTEnum baseTy _  -> go baseTy
            CT.CTArray _ innerTy ->
                go innerTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) ->
                isCompleteObjectTypeKind elemTy
            _ ->
                False

        isCompleteObjectTypeKind = \case
            CT.CTLong innerTy   -> isCompleteObjectTypeKind innerTy
            CT.CTShort innerTy  -> isCompleteObjectTypeKind innerTy
            CT.CTSigned innerTy -> isCompleteObjectTypeKind innerTy
            CT.CTEnum baseTy _  -> isCompleteObjectTypeKind baseTy
            CT.CTArray _ innerTy ->
                isCompleteObjectTypeKind innerTy
            CT.CTIncomplete _ ->
                False
            CT.CTVoid ->
                False
            CT.CTFunc _ _ ->
                False
            tyKind ->
                not $ hasIncompleteObjectType $ CT.SCAuto tyKind

addsOuterArrayLayerBeforeExistingOmittedBound :: CT.StorageClass i -> CT.StorageClass i -> Bool
addsOuterArrayLayerBeforeExistingOmittedBound baseTy declaredTy
    | not (hasInitializerCompletableArrayType baseTy) =
        False
    | otherwise =
        case
            ( outerArrayDepthBeforeFirstIncomplete baseTy
            , outerArrayDepthBeforeFirstIncomplete declaredTy
            ) of
            (Just baseDepth, Just declaredDepth) ->
                declaredDepth > baseDepth
            _ ->
                False

outerArrayDepthBeforeFirstIncomplete :: CT.StorageClass i -> Maybe Int
outerArrayDepthBeforeFirstIncomplete = go 0 . CT.toTypeKind
    where
        go depth = \case
            CT.CTLong innerTy   -> go depth innerTy
            CT.CTShort innerTy  -> go depth innerTy
            CT.CTSigned innerTy -> go depth innerTy
            CT.CTEnum baseTy _  -> go depth baseTy
            CT.CTArray _ innerTy ->
                go (succ depth) innerTy
            CT.CTIncomplete (CT.IncompleteArray _) ->
                Just depth
            _ ->
                Nothing

canonicalizeCompletableOmittedArrayType :: CT.StorageClass i -> CT.StorageClass i
canonicalizeCompletableOmittedArrayType ty = case CT.toTypeKind ty of
    CT.CTIncomplete (CT.IncompleteArray elemTy) ->
        let (baseTy, rebuild) = peelArrays elemTy
         in CT.mapTypeKind (const $ rebuild $ CT.CTIncomplete $ CT.IncompleteArray baseTy) ty
    _ ->
        ty
    where
        peelArrays (CT.CTArray n innerTy) =
            let (baseTy, rebuild) = peelArrays innerTy
             in (baseTy, CT.CTArray n . rebuild)
        peelArrays baseTy = (baseTy, id)

normalizeGlobalDeclType :: CT.StorageClass i -> CT.StorageClass i
normalizeGlobalDeclType ty
    | hasInitializerCompletableArrayType ty =
        canonicalizeCompletableOmittedArrayType ty
    | otherwise =
        ty

isValidTentativeFileScopeArrayType :: CT.StorageClass i -> Bool
isValidTentativeFileScopeArrayType = go . CT.toTypeKind
    where
        go = \case
            CT.CTArray _ innerTy ->
                go innerTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) ->
                isCompleteArrayElementType elemTy
            _ ->
                False

        isCompleteArrayElementType = \case
            CT.CTIncomplete _ -> False
            CT.CTArray _ innerTy -> isCompleteArrayElementType innerTy
            _ -> True

derefObjectType :: Ord i => CT.StorageClass i -> Maybe (CT.StorageClass i)
derefObjectType ty = case CT.toTypeKind ty of
    CT.CTArray n (CT.CTIncomplete (CT.IncompleteArray elemTy)) ->
        Just $ CT.mapTypeKind (const $ CT.CTArray n elemTy) ty
    _ ->
        CT.deref ty

callableSignature :: CT.StorageClass i -> Maybe (CT.StorageClass i, Maybe [CT.StorageClass i])
callableSignature ty = case CT.toTypeKind ty of
    CT.CTFunc retTy params ->
        Just (CT.SCAuto retTy, explicitFunctionParamTypes params)
    CT.CTPtr (CT.CTFunc retTy params) ->
        Just (CT.SCAuto retTy, explicitFunctionParamTypes params)
    _ ->
        Nothing

explicitFunctionParamTypes :: [(CT.TypeKind i, Maybe T.Text)] -> Maybe [CT.StorageClass i]
explicitFunctionParamTypes [] = Nothing
explicitFunctionParamTypes [(CT.CTVoid, Nothing)] = Just []
explicitFunctionParamTypes params =
    Just $ map (CT.SCAuto . canonicalizeFunctionParamType . fst) params

canonicalizeFunctionParamType :: CT.TypeKind i -> CT.TypeKind i
canonicalizeFunctionParamType (CT.CTArray _ elemTy) = CT.CTPtr elemTy
canonicalizeFunctionParamType (CT.CTIncomplete (CT.IncompleteArray elemTy)) = CT.CTPtr elemTy
canonicalizeFunctionParamType (CT.CTFunc retTy params) = CT.CTPtr $ CT.CTFunc retTy params
canonicalizeFunctionParamType ty = ty

applyCallArgConversions :: (Ord i, Bits i, Integral i) => Maybe [CT.StorageClass i] -> [ATree i] -> Parser i [ATree i]
applyCallArgConversions paramTys args = do
    shouldValidateUnsupported <- gets (not . suppressUnsupportedValueChecks)
    either fail pure $ convertCallArgsWith shouldValidateUnsupported paramTys args

convertCallArgs :: (Ord i, Bits i, Integral i) => Maybe [CT.StorageClass i] -> [ATree i] -> Either String [ATree i]
convertCallArgs = convertCallArgsWith True

convertCallArgsWith :: (Ord i, Bits i, Integral i) => Bool -> Maybe [CT.StorageClass i] -> [ATree i] -> Either String [ATree i]
convertCallArgsWith validateUnsupported Nothing args = do
    mapM_ (validateDeferredCallArg validateUnsupported) args
    Right $ map defaultPromotedCallArg args
convertCallArgsWith validateUnsupported (Just paramTys) args
    | actualArgCount < expectedArgCount = Left "too few arguments to function call"
    | actualArgCount > expectedArgCount = Left "too many arguments to function call"
    | otherwise = zipWithM convertTypedCallArg paramTys args
    where
        actualArgCount = length args
        expectedArgCount = length paramTys

        convertTypedCallArg paramTy arg = do
            validateDeferredCallArg validateUnsupported arg
            if isInvalidFunctionPointerValue paramTy arg
                || isInvalidObjectPointerValue paramTy arg
                || isInvalidAggregateValueArgument paramTy arg
                then
                    Left "invalid argument type to function call"
                else Right $ atCast paramTy arg

isInvalidAggregateValueArgument :: Eq i => CT.StorageClass i -> ATree i -> Bool
isInvalidAggregateValueArgument = isInvalidAggregateValueConversion

validateDeferredCallArg :: (Ord i, Bits i, Integral i) => Bool -> ATree i -> Either String ()
validateDeferredCallArg shouldValidate arg
    | not shouldValidate =
        Right ()
    | containsEscapingStmtExprControlFlow arg =
        Left "unsupported control flow in function call argument"
    | requiresUnsupportedNonAddressableArrayDecay arg =
        Left "unsupported non-addressable array member decay"
    | otherwise =
        Right ()

defaultPromotedCallArg :: Ord i => ATree i -> ATree i
defaultPromotedCallArg = castExprType defaultPromotedCallArgType

defaultPromotedCallArgType :: Ord i => CT.StorageClass i -> CT.StorageClass i
defaultPromotedCallArgType ty = CT.mapTypeKind (const promotedTy) decayedTy
    where
        decayedTy = decayExprType ty
        promotedTy = CT.integerPromotedTypeKind $ CT.toTypeKind decayedTy

integerPromotedExpr :: Eq i => ATree i -> ATree i
integerPromotedExpr = castExprType integerPromotedExprType

integerPromotedExprType :: CT.StorageClass i -> CT.StorageClass i
integerPromotedExprType ty = CT.mapTypeKind (const promotedTy) ty
    where
        promotedTy = CT.integerPromotedTypeKind $ CT.toTypeKind ty

castExprType :: Eq i => (CT.StorageClass i -> CT.StorageClass i) -> ATree i -> ATree i
castExprType f expr
    | promotedTy == atype expr = expr
    | otherwise = atCast promotedTy expr
    where
        promotedTy = f $ atype expr

isFunctionType :: CT.StorageClass i -> Bool
isFunctionType ty = case CT.toTypeKind ty of
    CT.CTFunc _ _ -> True
    _             -> False

isPointerType :: CT.StorageClass i -> Bool
isPointerType ty = case CT.toTypeKind ty of
    CT.CTPtr _ -> True
    _          -> False

isScalarOperandType :: Ord i => CT.StorageClass i -> Bool
isScalarOperandType ty =
    CT.isIntegral decayedTy || isPointerType decayedTy
    where
        decayedTy = decayExprType ty

isIntegerOperandType :: Ord i => CT.StorageClass i -> Bool
isIntegerOperandType =
    CT.isIntegral . decayExprType

isModifiableLvalueType :: CT.StorageClass i -> Bool
isModifiableLvalueType ty =
    not (CT.isCTArray ty) && not (isFunctionType ty)

requireNonFunctionOperand
    :: String
    -> ATree i
    -> Parser i (ATree i)
requireNonFunctionOperand opName expr
    | isFunctionType (atype expr) = fail $ "invalid application of '" <> opName <> "' to function type"
    | otherwise = pure expr

requireScalarOperand
    :: Ord i
    => String
    -> ATree i
    -> Parser i (ATree i)
requireScalarOperand err expr
    | isScalarOperandType (atype expr) = pure expr
    | otherwise = fail err

requireIntegerOperand
    :: Ord i
    => String
    -> ATree i
    -> Parser i (ATree i)
requireIntegerOperand err expr
    | isIntegerOperandType (atype expr) = pure expr
    | otherwise = fail err

requirePointerArithmeticTarget
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => ATree i
    -> Parser i ()
requirePointerArithmeticTarget expr = case CT.deref (atype expr) of
    Just ty
        | isFunctionType ty ->
            fail "invalid operands"
        | otherwise ->
            void $ requireCompleteObjectType "invalid use of pointer to incomplete type" ty
    Nothing ->
        pure ()

requirePointerArithmeticTargetAllowDeferred
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => ATree i
    -> Parser i ()
requirePointerArithmeticTargetAllowDeferred expr = case CT.deref (atype expr) of
    Just ty
        | isFunctionType ty ->
            requirePointerArithmeticTarget expr
        | CT.isCTIncomplete ty && isDeferredIncompletePointerArithmeticExpr expr ->
            pure ()
        | otherwise ->
            requirePointerArithmeticTarget expr
    Nothing ->
        requirePointerArithmeticTarget expr

isDeferredIncompleteObjectExpr :: ATree i -> Bool
isDeferredIncompleteObjectExpr = \case
    ATNode (ATLVar _ _) ty _ _ ->
        not $ CT.isIncompleteArray ty
    ATNode (ATGVar _ _) ty _ _ ->
        not $ CT.isIncompleteArray ty
    ATNode (ATMemberAcc _) ty _ _ ->
        not $ CT.isIncompleteArray ty
    _ ->
        True

isDeferredIncompletePointerArithmeticExpr :: ATree i -> Bool
isDeferredIncompletePointerArithmeticExpr = \case
    ATNode ATAddr _ _ _ -> False
    _                   -> True

resolveMemOperandType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> ATree i
    -> Parser i (CT.StorageClass i)
resolveMemOperandType err expr = do
    resolvedTy <- gets (incomplete $ atype expr)
    case resolvedTy of
        Just ty
            | CT.isIncompleteArray ty ->
                if isDeferredIncompleteObjectExpr expr then pure (atype expr) else fail err
            | otherwise ->
                pure ty
        Nothing ->
            if isDeferredIncompleteObjectExpr expr then pure (atype expr) else fail err

isModifiableLvalueExpr :: ATree i -> Bool
isModifiableLvalueExpr (ATNode kind ty lhs _)
    | not $ isModifiableLvalueType ty = False
    | otherwise = case kind of
        ATLVar _ _    -> True
        ATGVar _ _    -> True
        ATMemberAcc _ -> isModifiableLvalueExpr lhs
        ATDeref       -> isAddressableDerefOperand lhs
        _             -> False
isModifiableLvalueExpr _ = False

isAddressableLvalueExpr :: ATree i -> Bool
isAddressableLvalueExpr (ATNode kind _ lhs _) = case kind of
    ATLVar _ _    -> True
    ATGVar _ _    -> True
    ATMemberAcc _ -> isAddressableLvalueExpr lhs
    ATDeref       -> isAddressableDerefOperand lhs
    _             -> False
isAddressableLvalueExpr _ = False

isAddressableDerefOperand :: ATree i -> Bool
isAddressableDerefOperand ptr
    | Just arrayExpr <- pointerIndexRootArrayOperand ptr =
        isAddressableLvalueExpr arrayExpr
    | CT.isArray (atype ptr) =
        isAddressableLvalueExpr ptr
isAddressableDerefOperand _ = True

pointerIndexOperands :: ATree i -> Maybe (ATree i, ATree i)
pointerIndexOperands (ATNode ATAddPtr _ arrayExpr idx) = Just (arrayExpr, idx)
pointerIndexOperands (ATNode ATSubPtr _ arrayExpr idx) = Just (arrayExpr, idx)
pointerIndexOperands _                                 = Nothing

pointerIndexRootArrayOperand :: ATree i -> Maybe (ATree i)
pointerIndexRootArrayOperand ptr = do
    root <- pointerIndexRootOperand ptr
    if CT.isArray (atype root) then Just root else Nothing
    where
        pointerIndexRootOperand expr = do
            (arrayExpr, _) <- pointerIndexOperands expr
            case pointerIndexRootOperand arrayExpr of
                Just root -> Just root
                Nothing   -> Just arrayExpr

isAddressableUnaryOperand :: ATree i -> Bool
isAddressableUnaryOperand (ATNode kind _ lhs _) = case kind of
    ATLVar _ _    -> True
    ATGVar _ _    -> True
    ATFuncPtr _   -> True
    ATMemberAcc _ -> isAddressableLvalueExpr lhs
    ATDeref       -> isAddressableDerefOperand lhs
    _             -> False
isAddressableUnaryOperand _ = False

isUnevaluatedRvalueArrayElementLvalue :: ATree i -> Bool
isUnevaluatedRvalueArrayElementLvalue (ATNode ATDeref _ ptr _)
    | Just arrayExpr <- pointerIndexRootArrayOperand ptr =
        not $ isAddressableLvalueExpr arrayExpr
    | CT.isArray (atype ptr) =
        not $ isAddressableLvalueExpr ptr
isUnevaluatedRvalueArrayElementLvalue (ATNode (ATMemberAcc _) _ lhs _) =
    isUnevaluatedRvalueArrayElementLvalue lhs
isUnevaluatedRvalueArrayElementLvalue _ = False

rejectUnsupportedNonAddressableArrayDecay :: Ord i => ATree i -> Parser i ()
rejectUnsupportedNonAddressableArrayDecay expr = do
    shouldValidateUnsupported <- gets (not . suppressUnsupportedValueChecks)
    when (shouldValidateUnsupported && requiresUnsupportedNonAddressableArrayDecay expr) $
        fail "unsupported non-addressable array member decay"

rejectNonScalarCondition :: Ord i => ATree i -> Parser i ()
rejectNonScalarCondition expr
    | isScalarConditionType (atype expr) = pure ()
    | otherwise = fail "invalid condition type"

isScalarConditionType :: Ord i => CT.StorageClass i -> Bool
isScalarConditionType ty =
    CT.isIntegral decayedTy || case CT.toTypeKind decayedTy of
        CT.CTPtr _ -> True
        _          -> False
    where
        decayedTy = decayExprType ty

withSuppressedUnsupportedValueChecks :: Parser i a -> Parser i a
withSuppressedUnsupportedValueChecks =
    bracket
        (gets suppressUnsupportedValueChecks <* modify (\cd -> cd { suppressUnsupportedValueChecks = True }))
        (\restore -> modify (\cd -> cd { suppressUnsupportedValueChecks = restore }))
        . const

rejectingBinOp
    :: Ord i
    => (ATree i -> ATree i -> Parser i (ATree i))
    -> ATree i
    -> ATree i
    -> Parser i (ATree i)
rejectingBinOp op lhs rhs = do
    rejectUnsupportedNonAddressableArrayDecay lhs
    rejectUnsupportedNonAddressableArrayDecay rhs
    op lhs rhs

rejectingScalarBinOp
    :: Ord i
    => (ATree i -> ATree i -> Parser i (ATree i))
    -> ATree i
    -> ATree i
    -> Parser i (ATree i)
rejectingScalarBinOp op lhs rhs = do
    void $ requireScalarOperand "invalid operands" lhs
    void $ requireScalarOperand "invalid operands" rhs
    rejectingBinOp op lhs rhs

requireModifiableLvalue
    :: String
    -> ATree i
    -> Parser i (ATree i)
requireModifiableLvalue err expr
    | isModifiableLvalueExpr expr = pure expr
    | otherwise = do
        unsupportedChecksSuppressed <- gets suppressUnsupportedValueChecks
        if unsupportedChecksSuppressed
            && isModifiableLvalueType (atype expr)
            && isUnevaluatedRvalueArrayElementLvalue expr
            then pure expr
            else fail err

global,
    stmt,
    expr,
    assign,
    conditional,
    logicalOr,
    logicalAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseAnd,
    equality,
    relational,
    shift,
    add,
    term,
    cast,
    unary,
    factor :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)

global = do
    pos <- getPosState
    rejectInvalidFileScopeStorageClass
    (declStorage, ty) <- declarationSpec
    case declStorage of
        TypedefDecl ->
            globalDecl declStorage ty pos
        _ ->
            choice
                [ ATEmpty <$ semi
                , globalDecl declStorage ty pos
                ]
    where
        rejectInvalidFileScopeStorageClass =
            M.lookAhead $
                choice
                    [ kAuto *> fail "storage-class specifier is not allowed at file scope"
                    , kRegister *> fail "storage-class specifier is not allowed at file scope"
                    , pure ()
                    ]

        globalDecl declStorage ty pos = captureFunctionParamScopes (declarator ty) >>= \case
            ((_, Nothing), _) -> fail $
                if declStorage == TypedefDecl
                    then "typedef name omitted, expected unqualified-id"
                    else "variable name omitted, expected unqualified-id"
            ((ty', Just ident), paramScopes)
                | declStorage == TypedefDecl ->
                    typedefDecl ty' ident
                | isFunctionType ty' -> modify resetLocal
                *> choice
                    [ declaration ty' ident
                    , definition ty' ident pos paramScopes
                    ]
            ((ty', Just ident), _) ->
                requireNonVoidObjectType "variable declared void" ty'
                    *> gvarDecl declStorage ty ty' ident

        isFunctionType ty' = case CT.toTypeKind ty' of
            CT.CTFunc _ _ -> True
            _             -> False

        declaration ty ident = do
            resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
            semi *> registerFunc False False resolvedTy ident $> ATEmpty

        typedefDecl ty ident = do
            resolvedTy <- requireTypedefDeclType "typedef declaration has invalid array element type" ty
            semi *> registerTypedef resolvedTy ident $> ATEmpty

        definition ty ident pos paramScopes = do
            resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
            registerFunc True False resolvedTy ident
            bracket get (modify . fallBack) $ const $ do
                paramScope <- maybe
                    (fail "internal compiler error: missing function parameter scope")
                    pure
                    -- The function body's scope is the earliest prototype scope created
                    -- while parsing the declarator. Nested parameter prototypes and
                    -- trailing function suffixes are created later and must not leak.
                    (listToMaybe $ sortBy (comparing fpsScopeId) paramScopes)
                params <- registerFunctionParams paramScope resolvedTy
                functionBody >>= fromValidFunc resolvedTy params
            where
                registerFunctionParams paramScope fnTy =
                    enterFunctionScope paramScope
                        *> (mapM registerParam . zip [0 :: Int ..] =<< functionParamDecls fnTy)
                    where
                        registerParam (idx, (paramTy, mIdent)) = do
                            resolvedParamTy <-
                                requireCompleteObjectType
                                    "declaration of variable with incomplete type"
                                    paramTy
                                    >>= requireSupportedByValueType
                                        "unsupported by-value function parameter type"
                            registerLVar
                                resolvedParamTy
                                (fromMaybe (unnamedFunctionParamIdent idx) mIdent)

                enterFunctionScope paramScope =
                    modify $ \cd ->
                        cd
                            { scope =
                                (scope cd)
                                    { curNestDepth = succ $ curNestDepth $ scope cd
                                    , curScopeId = fpsScopeId paramScope
                                    , structs = fpsTags paramScope
                                    , enumerators = fpsEnumerators paramScope
                                    }
                            }

                functionBody = atBlock <$> braces (M.many stmt)

                fromValidFunc fnTy params' st@(ATNode (ATBlock block) _ _ _) = do
                    when (hasInvalidStmtExprControlFlow st) $
                        fail "unsupported control flow in statement expression"
                    case CT.toTypeKind fnTy of
                        CT.CTFunc retTy _ -> do
                            when (isUnsupportedByValueAggregateType (CT.SCAuto retTy)) $
                                fail "unsupported by-value function return type"
                            if retTy == CT.CTVoid then
                                if isJust (find isNonEmptyReturn block) then
                                    fail $ mconcat
                                        [ "the return type of function '"
                                        , T.unpack ident
                                        , "' is void, but the statement returns a value"
                                        ]
                                else do
                                    when (hasInvalidAggregateReturnValue (CT.SCAuto retTy) st) $
                                        fail "invalid return type"
                                    pure $ atDefFunc ident (if null params' then Nothing else Just params') fnTy st
                            else do
                                when (hasInvalidAggregateReturnValue (CT.SCAuto retTy) st) $
                                    fail "invalid return type"
                                when (isJust (find isEmptyReturn block)) $
                                    pushWarn pos $ mconcat
                                        [ "the return type of function '"
                                        , T.unpack ident
                                        , "' is "
                                        , show retTy
                                        , ", but the statement returns no value"
                                        ]
                                pure $ atDefFunc ident (if null params' then Nothing else Just params') fnTy st
                        _ ->
                            fail "internal compiler error"

        gvarDecl declStorage baseTy ty ident = choice
            [ nonInit declStorage ty ident
            , withInit baseTy ty ident
            ]
        nonInit declStorage ty ident
            | declStorage == ExternDecl =
                semi
                    *> (requireExternDeclObjectType "declaration of variable with incomplete type" ty
                            >>= \resolvedTy -> registerGVarWith (normalizeGlobalDeclType resolvedTy) ident PV.GVarInitWithExternDecl
                       )
                    $> ATEmpty
            | CT.isIncompleteArray ty && isValidTentativeFileScopeArrayType ty =
                semi *> registerGVar (normalizeGlobalDeclType ty) ident $> ATEmpty
            | CT.isIncompleteArray ty =
                fail "defining global variables with a incomplete type"
            | otherwise =
                semi
                    >> requireCompleteObjectType "defining global variables with a incomplete type" ty
                    >>= flip (registerGVar . normalizeGlobalDeclType) ident
                    >> pure ATEmpty

        withInit baseTy ty ident = do
            resolvedTy <- requireInitializedObjectType "defining global variables with a incomplete type" baseTy ty
            void equal
            (ty', initWith) <- parseGlobalVarInit resolvedTy ident
            registerGVarWith (normalizeGlobalDeclType ty') ident initWith <* semi

parseGlobalVarInit :: (Ord i, Bits i, Read i, Show i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i (CT.StorageClass i, PV.GVarInitWith i)
parseGlobalVarInit ty ident =
    bracket get (modify . fallBack) $ const $ do
        ensureTargetGlobalVisible ty ident
        let tempIdent = ".L.global.init." <> ident
        ast <- varInit assign ty tempIdent
        rejectIncompleteGlobalSelfReference ty ident ast
        ty' <- maybeToParser "defining global variables with a incomplete type"
            =<< gets (fmap PV.lvtype . lookupLVar tempIdent)
        void $ either fail pure $ foldGlobalInitWith ty' ast
        pure (ty', PV.GVarInitWithAST ast)
    where
        ensureTargetGlobalVisible declaredTy name =
            void $ registerGVar (normalizeGlobalDeclType declaredTy) name

        rejectIncompleteGlobalSelfReference declaredTy name ast
            | CT.isIncompleteArray declaredTy && containsGlobalRef name ast =
                fail "invalid initializer in global variable"
            | otherwise =
                pure ()

containsGlobalRef :: T.Text -> ATree i -> Bool
containsGlobalRef name = go
    where
        go ATEmpty = False
        go (ATNode kind _ lhs rhs) =
            goKind kind || go lhs || go rhs

        goKind = \case
            ATConditional cond tr fl ->
                any go [cond, tr, fl]
            ATSwitch cond cases ->
                go cond || any go cases
            ATFor kinds ->
                any (go . fromATKindFor) kinds
            ATBlock ats ->
                any go ats
            ATStmtExpr ats ->
                any go ats
            ATNull at ->
                go at
            ATDefFunc _ args ->
                maybe False (any go) args
            ATCallFunc _ args ->
                maybe False (any go) args
            ATCallPtr args ->
                maybe False (any go) args
            ATGVar _ ref ->
                ref == name
            _ ->
                False

hasInvalidAggregateReturnValue :: Eq i => CT.StorageClass i -> ATree i -> Bool
hasInvalidAggregateReturnValue returnTy = go
    where
        go ATEmpty = False
        go (ATNode ATReturn _ ATEmpty _) = False
        go (ATNode ATReturn _ returnedExpr _) =
            isInvalidAggregateValueConversion returnTy returnedExpr
                || go returnedExpr
        go (ATNode ATSizeof _ _ _) = False
        go (ATNode ATAlignof _ _ _) = False
        go (ATNode kind _ lhs rhs) =
            goKind kind || go lhs || go rhs

        goKind = \case
            ATConditional cond tr fl ->
                any go [cond, tr, fl]
            ATSwitch cond cases ->
                go cond || any go cases
            ATFor kinds ->
                any (go . fromATKindFor) kinds
            ATBlock ats ->
                any go ats
            ATStmtExpr ats ->
                any go ats
            ATNull at ->
                go at
            ATDefFunc _ args ->
                maybe False (any go) args
            ATCallFunc _ args ->
                maybe False (any go) args
            ATCallPtr args ->
                maybe False (any go) args
            _ ->
                False

foldGlobalInitWith :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> ATree i
    -> Either String (PV.GVarInitWith i)
foldGlobalInitWith ty ast = do
    entries <- globalInitEntries ast
    PV.GVarInitWithData <$> finalizeGlobalInitData (CT.sizeof ty) entries

globalInitEntries :: (Integral i, Bits i, Read i, Show i, Ord i)
    => ATree i
    -> Either String [(Natural, PV.GVarInitData i)]
globalInitEntries ATEmpty = Right []
globalInitEntries (ATNode (ATBlock stmts) _ _ _) = concat <$> mapM globalInitEntries stmts
globalInitEntries (ATNode ATExprStmt _ expr _) = globalInitEntries expr
globalInitEntries (ATNode ATAssign _ lhs rhs) = do
    offset <- maybe (Left "invalid initializer in global variable") Right $ globalInitByteOffset lhs
    dat <- globalInitDatum (atype lhs) rhs
    Right [(offset, dat)]
globalInitEntries _ = Left "invalid initializer in global variable"

globalInitDatum :: (Integral i, Bits i, Read i, Show i, Ord i)
    => CT.StorageClass i
    -> ATree i
    -> Either String (PV.GVarInitData i)
globalInitDatum ty rhs
    | rejectsIncompatibleScalarInitializer =
        Left "invalid initializer for scalar object"
    | otherwise = globalInitReloc ty rhs >>= \case
    Just (ref, addend)
        | isRelocInitializerType ty -> pure $ PV.GVarInitReloc size ref addend
        | otherwise -> Left "invalid initializer for scalar object"
    Nothing -> do
        val <- evalGlobalInitConstexpr ty rhs
        Right $
            if val == 0
                then PV.GVarInitZeroBytes size
                else PV.GVarInitBytes size val
    where
        size = CT.sizeof ty
        rejectsIncompatibleScalarInitializer =
            isInvalidFunctionPointerInitializer ty rhs
                || isInvalidObjectPointerValue ty rhs
        isRelocInitializerType = \case
            CT.SCAuto (CT.CTPtr _)   -> True
            CT.SCRegister (CT.CTPtr _) -> True
            CT.SCStatic (CT.CTPtr _) -> True
            _                        -> False
        isFunctionPointerType sc = case CT.toTypeKind sc of
            CT.CTPtr (CT.CTFunc _ _) -> True
            _                        -> False

globalInitReloc :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> ATree i
    -> Either String (Maybe (T.Text, Integer))
globalInitReloc targetTy = \case
    ATNode ATCast ty inner _
        | isPointerType ty
            && (not (isFunctionPointerType targetTy) || isFunctionPointerType ty) ->
            globalInitReloc targetTy inner
        | otherwise -> Right Nothing
    ATNode (ATNull inner) _ _ _ ->
        globalInitReloc targetTy inner
    ATNode ATExprStmt _ inner _ ->
        globalInitReloc targetTy inner
    ATNode (ATConditional cond ATEmpty el) _ _ _ ->
        globalInitConditionalReloc cond cond el
    ATNode (ATConditional cond th el) _ _ _ ->
        globalInitConditionalReloc cond th el
    ATNode (ATFuncPtr name) _ _ _
        | isFunctionPointerType targetTy -> Right $ Just (name, 0)
        | otherwise -> Right Nothing
    ast@(ATNode (ATGVar ty _) _ _ _)
        | CT.isArray ty -> globalInitLvalueReloc targetTy ast
    ast@(ATNode ATDeref ty _ _)
        | CT.isArray ty -> globalInitLvalueReloc targetTy ast
    ast@(ATNode (ATMemberAcc mem) _ _ _)
        | CT.isArray (CT.smType mem) -> globalInitLvalueReloc targetTy ast
    ATNode ATAddr _ inner _ -> globalInitLvalueReloc targetTy inner
    ATNode ATAddPtr _ lhs rhs -> globalInitRelocWithAddend (+) lhs rhs
    ATNode ATSubPtr _ lhs rhs -> globalInitRelocWithAddend (-) lhs rhs
    _ -> Right Nothing
    where
        isPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr _ -> True
            _          -> False

        isFunctionPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr (CT.CTFunc _ _) -> True
            _                        -> False

        offsetFromIndex lhs idx =
            fromIntegral idx * maybe 0 (fromIntegral . CT.sizeof) (CT.deref $ atype lhs)

        globalInitConditionalReloc cond th el = case evalGlobalInitConditionMaybe cond of
            Just True -> globalInitReloc targetTy th
            Just False ->
                globalInitReloc targetTy el
            Nothing ->
                Right Nothing

        globalInitRelocWithAddend op lhs rhs =
            globalInitReloc targetTy lhs >>= \case
                Just (name, addend) ->
                    Right $ (\idx -> Just (name, op addend (offsetFromIndex lhs idx))) =<< evalConstexprMaybe rhs
                Nothing -> Right Nothing

        evalConstexprMaybe expr = either (const Nothing) Just $ evalConstexprTree expr

globalInitLvalueReloc :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> ATree i
    -> Either String (Maybe (T.Text, Integer))
globalInitLvalueReloc targetTy = \case
    ATNode (ATFuncPtr name) _ _ _
        | isFunctionPointerType targetTy -> Right $ Just (name, 0)
        | otherwise -> Right Nothing
    ATNode (ATGVar _ name) _ _ _
        | isFunctionPointerType targetTy -> Right Nothing
        | otherwise -> Right $ Just (name, 0)
    ATNode (ATMemberAcc mem) _ lhs _ -> do
        globalInitLvalueReloc targetTy lhs <&> fmap (second (+ fromIntegral (CT.smOffset mem)))
    ATNode ATDeref _ ptr _ -> globalInitReloc targetTy ptr
    ATNode ATCast _ lhs _ -> globalInitLvalueReloc targetTy lhs
    _ -> Right Nothing
    where
        isFunctionPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr (CT.CTFunc _ _) -> True
            _                        -> False

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

evalGlobalInitConstexpr :: (Bits i, Integral i, Show i, Read i) => CT.StorageClass i -> ATree i -> Either String i
evalGlobalInitConstexpr ty rhs =
    case CT.toTypeKind ty of
        CT.CTPtr _ -> evalPointerNullGlobalInit rhs
        _          -> applyConstexprCast ty <$> evalConstexprTree rhs
    where
        evalPointerNullGlobalInit ast =
            evalPointerNullConstexpr ast >>= rejectUnlessZero

        rejectUnlessZero val
            | val == 0 = pure 0
            | otherwise = Left "initializer element is not constant"

evalGlobalInitConditionMaybe :: (Bits i, Integral i, Show i, Read i) => ATree i -> Maybe Bool
evalGlobalInitConditionMaybe = either (const Nothing) Just . evalGlobalInitCondition

evalGlobalInitCondition :: (Bits i, Integral i, Show i, Read i) => ATree i -> Either String Bool
evalGlobalInitCondition ast =
    case either (const Nothing) (Just . (/= 0)) $ evalConstexprTree ast of
        Just truthy ->
            pure truthy
        Nothing -> case evalPointerNullConstexpr ast of
            Right 0 ->
                pure False
            _ ->
                maybe (Left "initializer element is not constant") pure $
                    evalGlobalInitAddressCondition ast

evalGlobalInitAddressCondition :: (Bits i, Integral i, Show i, Read i) => ATree i -> Maybe Bool
evalGlobalInitAddressCondition = \case
    ATNode ATCast ty inner _
        | isPointerType ty ->
            evalGlobalInitAddressCondition inner
    ATNode (ATNull inner) _ _ _ ->
        evalGlobalInitAddressCondition inner
    ATNode ATExprStmt _ inner _ ->
        evalGlobalInitAddressCondition inner
    ATNode (ATConditional cond ATEmpty el) _ _ _ ->
        evalGlobalInitConditionMaybe cond >>= \case
            True ->
                pure True
            False ->
                evalGlobalInitConditionMaybe el
    ATNode (ATConditional cond th el) _ _ _ ->
        evalGlobalInitConditionMaybe cond >>= \case
            True ->
                evalGlobalInitConditionMaybe th
            False ->
                evalGlobalInitConditionMaybe el
    ATNode (ATFuncPtr _) _ _ _ ->
        Just True
    ast@(ATNode (ATGVar ty _) _ _ _)
        | CT.isArray ty ->
            globalInitLvalueCondition ast
    ast@(ATNode ATDeref ty _ _)
        | CT.isArray ty ->
            globalInitLvalueCondition ast
    ast@(ATNode (ATMemberAcc mem) _ _ _)
        | CT.isArray (CT.smType mem) ->
            globalInitLvalueCondition ast
    ATNode ATAddr _ inner _ ->
        globalInitLvalueCondition inner
    ATNode ATAddPtr _ lhs rhs ->
        globalInitAddressAdditiveCondition lhs rhs
    ATNode ATSubPtr _ lhs rhs ->
        globalInitAddressAdditiveCondition lhs rhs
    _ ->
        Nothing
    where
        isPointerType ty = case CT.toTypeKind ty of
            CT.CTPtr _ -> True
            _          -> False

        globalInitAddressAdditiveCondition lhs rhs =
            globalInitAddressConditionBase lhs >> evalConstexprMaybe rhs >> pure True

        globalInitAddressConditionBase = \case
            ATNode ATCast ty inner _
                | isPointerType ty ->
                    globalInitAddressConditionBase inner
            ATNode (ATNull inner) _ _ _ ->
                globalInitAddressConditionBase inner
            ATNode ATExprStmt _ inner _ ->
                globalInitAddressConditionBase inner
            ATNode (ATConditional cond ATEmpty el) _ _ _ ->
                evalGlobalInitConditionMaybe cond >>= \case
                    True ->
                        globalInitAddressConditionBase cond
                    False ->
                        globalInitAddressConditionBase el
            ATNode (ATConditional cond th el) _ _ _ ->
                evalGlobalInitConditionMaybe cond >>= \case
                    True ->
                        globalInitAddressConditionBase th
                    False ->
                        globalInitAddressConditionBase el
            ATNode (ATFuncPtr _) _ _ _ ->
                Just ()
            ast@(ATNode (ATGVar ty _) _ _ _)
                | CT.isArray ty ->
                    globalInitLvalueConditionBase ast
            ast@(ATNode ATDeref ty _ _)
                | CT.isArray ty ->
                    globalInitLvalueConditionBase ast
            ast@(ATNode (ATMemberAcc mem) _ _ _)
                | CT.isArray (CT.smType mem) ->
                    globalInitLvalueConditionBase ast
            ATNode ATAddr _ inner _ ->
                globalInitLvalueConditionBase inner
            ATNode ATAddPtr _ lhs rhs ->
                globalInitAddressConditionBase lhs >> evalConstexprMaybe rhs >> pure ()
            ATNode ATSubPtr _ lhs rhs ->
                globalInitAddressConditionBase lhs >> evalConstexprMaybe rhs >> pure ()
            _ ->
                Nothing

        globalInitLvalueCondition = fmap (const True) . globalInitLvalueConditionBase

        globalInitLvalueConditionBase = \case
            ATNode (ATFuncPtr _) _ _ _ ->
                Just ()
            ATNode (ATGVar _ _) _ _ _ ->
                Just ()
            ATNode (ATMemberAcc _) _ lhs _ ->
                globalInitLvalueConditionBase lhs
            ATNode ATDeref _ ptr _ ->
                globalInitAddressConditionBase ptr
            ATNode ATCast _ lhs _ ->
                globalInitLvalueConditionBase lhs
            _ ->
                Nothing

        evalConstexprMaybe expr = either (const Nothing) Just $ evalConstexprTree expr

evalPointerNullConstexpr :: (Bits i, Integral i, Show i, Read i) => ATree i -> Either String i
evalPointerNullConstexpr (ATNode ATCast castTy inner _)
    | isPointerStorageClass castTy =
        applyConstexprCast castTy <$> evalPointerNullConstexpr inner
evalPointerNullConstexpr ast =
    evalConstexprTree ast

isPointerStorageClass :: CT.StorageClass i -> Bool
isPointerStorageClass ty = case CT.toTypeKind ty of
    CT.CTPtr _ -> True
    _          -> False

evalConstexprTree :: (Bits i, Integral i, Show i, Read i) => ATree i -> Either String i
evalConstexprTree = \case
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
        ATConditional cn th el ->
            evalGlobalInitCondition cn >>= \cond ->
                if cond
                    then evalConstexprTree $
                        case th of
                            ATEmpty -> cn
                            _       -> th
                    else evalConstexprTree el
        ATNot -> fromBool . not <$> evalGlobalInitCondition lhs
        ATBitNot -> complement <$> evalConstexprTree lhs
        ATLAnd -> evalGlobalInitCondition lhs >>= logicalAnd
        ATLOr -> evalGlobalInitCondition lhs >>= logicalOr
        ATSizeof -> memOp "sizeof" CT.sizeof lhs
        ATAlignof -> memOp "_Alignof" CT.alignof lhs
        ATCast
            | isConstexprArithmeticCastType ty -> applyConstexprCast ty <$> evalConstexprTree lhs
            | otherwise -> Left "initializer element is not constant"
        ATNum v -> pure v
        _ -> Left "initializer element is not constant"
      where
        binop f = evalConstexprTree lhs >>= \lhs' -> f lhs' <$> evalConstexprTree rhs
        logicalAnd lhs'
            | not lhs' = pure $ fromBool False
            | otherwise = fromBool <$> evalGlobalInitCondition rhs
        logicalOr lhs'
            | lhs' = pure $ fromBool True
            | otherwise = fromBool <$> evalGlobalInitCondition rhs
        nonZeroBinop f =
            evalConstexprTree lhs >>= \lhs' ->
                evalConstexprTree rhs >>= \rhs' ->
                    if rhs' == 0
                        then Left "initializer element is not constant"
                        else pure (f lhs' rhs')
        memOp opName op expr
            | hasIncompleteObjectType (atype expr) =
                Left $ "invalid application of '" <> opName <> "' to incomplete type"
            | otherwise =
                pure $ fromIntegral $ op $ atype expr
        fromBool = fromIntegral . fromEnum
        (.:) f g x y = f (g x y)
    _ -> Left "initializer element is not constant"

isConstexprArithmeticCastType :: CT.StorageClass i -> Bool
isConstexprArithmeticCastType = \case
    CT.SCAuto ty     -> go ty
    CT.SCRegister ty -> go ty
    CT.SCStatic ty   -> go ty
    CT.SCUndef ty    -> go ty
    where
        go = \case
            CT.CTInt              -> True
            CT.CTChar             -> True
            CT.CTBool             -> True
            CT.CTEnum _ _         -> True
            CT.CTSigned CT.CTUndef -> True
            CT.CTShort CT.CTUndef  -> True
            CT.CTLong CT.CTUndef   -> True
            CT.CTSigned ty        -> go ty
            CT.CTShort ty         -> go ty
            CT.CTLong ty          -> go ty
            _                     -> False

finalizeGlobalInitData :: Natural -> [(Natural, PV.GVarInitData i)] -> Either String [PV.GVarInitData i]
finalizeGlobalInitData totalBytes entries = mergeGlobalInitData <$> go 0 sorted
    where
        sorted = sortBy (comparing fst) entries

        go offset [] =
            pure [PV.GVarInitZeroBytes $ totalBytes - offset | offset < totalBytes]
        go offset ((nextOffset, dat):rest)
            | nextOffset < offset = Left "internal compiler error: overlapping global initializer"
            | otherwise = do
                suffix <- go (nextOffset + globalInitDataSize dat) rest
                pure $
                    zeroGap offset nextOffset <> [dat] <> suffix

        zeroGap cur nxt
            | cur < nxt = [PV.GVarInitZeroBytes $ nxt - cur]
            | otherwise = []

globalInitDataSize :: PV.GVarInitData i -> Natural
globalInitDataSize = \case
    PV.GVarInitZeroBytes sz -> sz
    PV.GVarInitBytes sz _   -> sz
    PV.GVarInitReloc sz _ _ -> sz

mergeGlobalInitData :: [PV.GVarInitData i] -> [PV.GVarInitData i]
mergeGlobalInitData = foldr step []
    where
        step (PV.GVarInitZeroBytes sz) (PV.GVarInitZeroBytes sz' : rest) =
            PV.GVarInitZeroBytes (sz + sz') : rest
        step dat acc = dat : acc

globalInitByteOffset :: Integral i => ATree i -> Maybe Natural
globalInitByteOffset = \case
    ATNode (ATLVar _ _) _ _ _ -> Just 0
    ATNode (ATMemberAcc mem) _ lhs _ -> (+ CT.smOffset mem) <$> globalInitByteOffset lhs
    ATNode ATDeref _ ptr _ -> globalInitByteOffset ptr
    ATNode ATAddr _ lhs _ -> globalInitByteOffset lhs
    ATNode ATCast _ lhs _ -> globalInitByteOffset lhs
    ATNode ATAddPtr _ lhs (ATNode (ATNum idx) _ _ _) ->
        (+ offsetFromIndex lhs idx) <$> globalInitByteOffset lhs
    _ -> Nothing
    where
        offsetFromIndex lhs idx = fromIntegral idx * maybe 0 CT.sizeof (CT.deref $ atype lhs)

compoundStmt :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i [ATree i]
compoundStmt = bracket get (modify . fallBack) $ const $
    braces (modify succNest *> M.many stmt)

stmt = choice
    [ returnStmt
    , ifStmt
    , whileStmt
    , forStmt
    , breakStmt
    , continueStmt
    , switchStmt
    , caseStmt
    , defaultStmt
    , gotoStmt
    , labelStmt
    , atBlock <$> compoundStmt
    , lvarStmt
    , exprStmt
    , ATEmpty <$ semi
    ]
    where
        returnStmt = choice
            [ atReturn (CT.SCUndef CT.CTUndef) ATEmpty <$ M.try (kReturn *> semi)
            , do
                ret <- M.try kReturn *> expr
                rejectUnsupportedNonAddressableArrayDecay ret
                atReturn (CT.SCUndef CT.CTUndef) ret <$ semi
            ]

        exprStmt = do
            nd <- expr
            rejectUnsupportedNonAddressableArrayDecay nd
            atExprStmt nd <$ semi

        ifStmt = do
            cond <- M.try kIf >> parens expr
            rejectNonScalarCondition cond
            rejectUnsupportedNonAddressableArrayDecay cond
            r <- atIf cond <$> stmt
            M.option ATEmpty (M.try kElse >> stmt) <&> \case
                ATEmpty -> r
                nd -> atElse r nd

        whileStmt = do
            cond <- M.try kWhile >> parens expr
            rejectNonScalarCondition cond
            rejectUnsupportedNonAddressableArrayDecay cond
            atWhile cond <$> stmt

        forStmt = (>>) (M.try kFor) $ bracket get (modify . fallBack) $ const $ do
            es <- parens $ do
                modify succNest
                initSect <- ATForInit
                    <$> choice [ATEmpty <$ semi, M.try exprStmt, lvarStmt]
                condSect <- ATForCond
                    <$> choice [ATEmpty <$ semi, checkedCondition <* semi]
                incrSect <- ATForIncr
                    <$> M.option ATEmpty exprStmtNoSemi
                pure [initSect, condSect, incrSect]
            atFor (es <> [ATForStmt ATEmpty]) <$ semi
                M.<|> atFor . (es <>) . (:[]) . ATForStmt <$> stmt
            where
                checkedCondition = do
                    nd <- expr
                    rejectNonScalarCondition nd
                    rejectUnsupportedNonAddressableArrayDecay nd
                    pure nd

                checkedExpr = do
                    nd <- expr
                    rejectUnsupportedNonAddressableArrayDecay nd
                    pure nd

                exprStmtNoSemi =
                    atExprStmt <$> checkedExpr

        breakStmt = atBreak <$ (M.try kBreak *> semi)

        continueStmt = atContinue <$ (M.try kContinue *> semi)

        switchStmt = do
            cond <- M.try kSwitch *> parens expr
            rejectNonScalarCondition cond
            rejectUnsupportedNonAddressableArrayDecay cond
            bracket (putSwitchState True) (const $ putSwitchState False) (const stmt)
                >>= \case
                    ATNode (ATBlock ats) ty _ _ -> pure $ atSwitch cond ats ty
                    _ -> fail "expected compound statement after the token ')'"
            where
                putSwitchState b = modify $ \scp -> scp { isSwitchStmt = b }

        caseStmt = M.try kCase
            *> ifM (gets isSwitchStmt)
                ((atCase 0 <$> evalConstexpr <* colon) <*> stmt)
                (fail "stray 'case'")

        defaultStmt = (M.try kDefault <* colon)
            *> ifM (gets isSwitchStmt)
                (atDefault 0 <$> stmt)
                (fail "stray 'default'")

        gotoStmt = atGoto <$> (M.try kGoto *> identifier <* semi)

        labelStmt = atLabel <$> M.try (identifier <* colon)

        lvarStmt = do
            (declStorage, ty) <- M.try declarationSpec
            case declStorage of
                TypedefDecl ->
                    declLVar declStorage ty
                _ ->
                    M.choice
                        [ standaloneDecl
                        , declLVar declStorage ty
                        ]
            where
                standaloneDecl = ATEmpty <$ semi

                declLVar declStorage ty = captureFunctionParamScopes (declarator ty) >>= \case
                    ((_, Nothing), _) -> fail $
                        if declStorage == TypedefDecl
                            then "typedef name omitted, expected unqualified-id"
                            else "variable name omitted, expected unqualified-id"
                    ((ty', Just ident), _) ->
                        case declStorage of
                            TypedefDecl ->
                                typedefDecl ty' ident
                            ExternDecl
                                | isFunctionType ty' ->
                                    do
                                        resolvedTy <- gets (`normalizeCompletedStorageClass` ty')
                                        semi *> registerFunc False False resolvedTy ident $> ATEmpty
                                | otherwise ->
                                    requireNonVoidObjectType "variable declared void" ty'
                                        *> externDecl ty' ident
                            OrdinaryDecl ->
                                if isFunctionType ty'
                                    then blockScopeFunctionDecl ty' ident
                                    else
                                        ordinaryObjectDecl ty ty' ident
                            AutoDecl ->
                                if isFunctionType ty'
                                    then fail "invalid storage-class specifier for block-scope function declaration"
                                    else ordinaryObjectDecl ty ty' ident

                nonInit ty ident =
                    requireCompleteObjectType "declaration of variable with incomplete type" ty
                        >>= \resolvedTy ->
                            semi *> registerLVar resolvedTy ident <&> atNull
                withInit baseTy ty ident = do
                    resolvedTy <-
                        requireInitializedObjectType "declaration of variable with incomplete type" baseTy ty
                    equal *> varInit assign resolvedTy ident <* semi
                ordinaryObjectDecl baseTy ty ident =
                    requireNonVoidObjectType "variable declared void" ty
                        *> M.choice
                            [ nonInit ty ident
                            , withInit baseTy ty ident
                            ]
                blockScopeFunctionDecl ty ident = do
                    rejectInvalidBlockScopeFunctionStorage ty
                    resolvedTy <- gets (`normalizeCompletedStorageClass` ty)
                    semi *> registerFunc False False resolvedTy ident $> ATEmpty
                rejectInvalidBlockScopeFunctionStorage = \case
                    CT.SCStatic _ ->
                        fail "invalid storage-class specifier for block-scope function declaration"
                    CT.SCRegister _ ->
                        fail "invalid storage-class specifier for block-scope function declaration"
                    _ ->
                        pure ()
                typedefDecl ty ident = do
                    resolvedTy <- requireTypedefDeclType "typedef declaration has invalid array element type" ty
                    semi *> registerTypedef resolvedTy ident $> ATEmpty
                externDecl ty ident =
                    M.choice
                        [ equal *> fail "initializer is not allowed in block scope extern declaration"
                        , semi
                            *> ( requireExternDeclObjectType
                                    "declaration of variable with incomplete type"
                                    ty
                                    >>= \resolvedTy ->
                                        registerGVarWith
                                            (normalizeGlobalDeclType resolvedTy)
                                            ident
                                            PV.GVarInitWithExternDecl
                               )
                            $> ATEmpty
                        ]

expr = assign >>= go
    where
        go lhs = M.option lhs $ do
            void comma
            rhs <- assign
            rejectUnsupportedNonAddressableArrayDecay lhs
            rejectUnsupportedNonAddressableArrayDecay rhs
            go $ ATNode ATComma (decayExprType $ atype rhs) lhs rhs

assign = do
    nd <- conditional
    M.option nd $ choice $ map (`id` nd)
        [ assignOp ATAssign "="
        , assignOp ATMulAssign "*="
        , assignOp ATDivAssign "/="
        , assignOp ATAndAssign "&="
        , assignOp ATOrAssign "|="
        , assignOp ATXorAssign "^="
        , assignOp ATShlAssign "<<="
        , assignOp ATShrAssign ">>="
        , assignOp (maybe ATAddAssign (const ATAddPtrAssign) $ CT.deref (atype nd)) "+="
        , assignOp (maybe ATSubAssign (const ATSubPtrAssign) $ CT.deref (atype nd)) "-="
        ]
    where
        assignOp k s nd = symbol s *> do
            lhs <- requireModifiableLvalue "lvalue required as left operand of assignment" nd
            requireCompletePointerArithmetic k lhs
            rhs <- assign
            requireCompatibleAssignmentOperands k lhs rhs
            rejectUnsupportedNonAddressableArrayDecay rhs
            pure $ ATNode k (atype lhs) lhs rhs

        requireCompletePointerArithmetic kind expr = case kind of
            ATAddPtrAssign ->
                requirePointerArithmeticTargetAllowDeferred expr
            ATSubPtrAssign ->
                requirePointerArithmeticTargetAllowDeferred expr
            _ ->
                pure ()

        requireCompatibleAssignmentOperands kind lhs rhs
            | isInvalidFunctionPointerValue (atype lhs) rhs =
                fail "invalid operands to assignment"
            | kind == ATAssign && isInvalidObjectPointerValue (atype lhs) rhs =
                fail "invalid operands to assignment"
            | kind == ATAssign && isInvalidAggregateValueConversion (atype lhs) rhs =
                fail "invalid operands to assignment"
            | kind /= ATAssign && isInvalidCompoundAssignmentOperands kind lhs rhs =
                fail "invalid operands to assignment"
            | otherwise =
                pure ()

        isInvalidCompoundAssignmentOperands kind lhs rhs =
            not $ case kind of
                ATAddPtrAssign ->
                    isPointerType (decayExprType $ atype lhs)
                        && isIntegerOperandType (atype rhs)
                ATSubPtrAssign ->
                    isPointerType (decayExprType $ atype lhs)
                        && isIntegerOperandType (atype rhs)
                ATAddAssign ->
                    integerOperands
                ATSubAssign ->
                    integerOperands
                ATMulAssign ->
                    integerOperands
                ATDivAssign ->
                    integerOperands
                ATAndAssign ->
                    integerOperands
                ATOrAssign ->
                    integerOperands
                ATXorAssign ->
                    integerOperands
                ATShlAssign ->
                    integerOperands
                ATShrAssign ->
                    integerOperands
                _ ->
                    False
            where
                integerOperands =
                    isIntegerOperandType (atype lhs)
                        && isIntegerOperandType (atype rhs)

conditional = do
    nd <- logicalOr
    ifM
        (M.option False (True <$ M.lookAhead question))
        (rejectNonScalarCondition nd >> (GNU.condOmitted nd M.<|> condOp nd))
        $ pure nd
    where
        condOp nd = do
            rejectUnsupportedNonAddressableArrayDecay nd
            th <- question *> expr <* colon
            el <- conditional
            ty <- maybeToParser "invalid operands" $ conditionalResultType th el
            rejectUnsupportedNonAddressableArrayDecay th
            rejectUnsupportedNonAddressableArrayDecay el
            pure $ atConditional ty nd th el

logicalOr = binaryOperator logicalAnd [(symbol "||", rejectingScalarBinOp $ binOpBool ATLOr)]

logicalAnd = binaryOperator bitwiseOr [(symbol "&&", rejectingScalarBinOp $ binOpBool ATLAnd)]

bitwiseOr = binaryOperator bitwiseXor [(vertical, rejectingBinOp $ binOpIntOnly ATOr)]

bitwiseXor = binaryOperator bitwiseAnd [(hat, rejectingBinOp $ binOpIntOnly ATXor)]

bitwiseAnd = binaryOperator equality [(MC.char '&' `notFollowedOp` MC.char '&', rejectingBinOp $ binOpIntOnly ATAnd)]

equality = binaryOperator relational
    [ (symbol "==", rejectingScalarBinOp $ binOpBool ATEQ)
    , (symbol "!=", rejectingScalarBinOp $ binOpBool ATNEQ)
    ]

relational = binaryOperator shift
    [ (symbol "<=", rejectingScalarBinOp $ binOpBool ATLEQ)
    , (langle, rejectingScalarBinOp $ binOpBool ATLT)
    , (symbol ">=", rejectingScalarBinOp $ binOpBool ATGEQ)
    , (rangle, rejectingScalarBinOp $ binOpBool ATGT)
    ]

shift = binaryOperator add
    [ (symbol "<<", rejectingBinOp $ binOpIntOnly ATShl)
    , (symbol ">>", rejectingBinOp $ binOpIntOnly ATShr)
    ]

add = binaryOperator term
    [ (symbol "+", pointerAwareBinaryOp addKind)
    , (symbol "-", pointerAwareBinaryOp subKind)
    ]
    where
        pointerAwareBinaryOp mk l r = do
            node <- maybeToParser "invalid operands" $ mk (decayIncompleteArrayExpr l) (decayIncompleteArrayExpr r)
            requireCompletePointerArithmeticNode node
            rejectUnsupportedImmediateValueUse node
            pure node

        rejectUnsupportedImmediateValueUse = \case
            ATNode ATAddPtr _ _ _ -> pure ()
            ATNode ATSubPtr _ _ _ -> pure ()
            node                  -> rejectUnsupportedNonAddressableArrayDecay node

        requireCompletePointerArithmeticNode = \case
            ATNode ATAddPtr _ ptr _ ->
                requirePointerArithmeticTargetAllowDeferred ptr
            ATNode ATSubPtr _ ptr _ ->
                requirePointerArithmeticTargetAllowDeferred ptr
            ATNode ATPtrDis _ lhs rhs ->
                requirePointerArithmeticTargetAllowDeferred lhs
                    >> requirePointerArithmeticTargetAllowDeferred rhs
            _ -> pure ()

term = binaryOperator cast
    [ (star, rejectingBinOp $ binOpIntOnly ATMul)
    , (slash, rejectingBinOp $ binOpIntOnly ATDiv)
    , (percent, rejectingBinOp $ binOpIntOnly ATMod)
    ]

cast = choice
    [ do
        ty <- M.try (parens absDeclarator)
        operand <- cast
        rejectAggregateCast ty operand
        rejectUnsupportedNonAddressableArrayDecay operand
        pure $ atCast ty operand
    , unary
    ]

rejectAggregateCast :: CT.StorageClass i -> ATree i -> Parser i ()
rejectAggregateCast ty operand
    | isAggregateType ty = fail "invalid cast type"
    | not (isVoidType ty) && isAggregateType (atype operand) = fail "invalid cast operand"
    | otherwise = pure ()
    where
        isAggregateType aggregateTy =
            CT.isCTStruct aggregateTy || CT.isIncompleteStruct aggregateTy

        isVoidType voidTy = case CT.toTypeKind voidTy of
            CT.CTVoid -> True
            _         -> False

unary = choice
    [ symbol "++" *> checkedIncDecOperand "lvalue required as increment operand" <&> \n -> ATNode ATPreInc (atype n) n ATEmpty
    , symbol "--" *> checkedIncDecOperand "lvalue required as decrement operand" <&> \n -> ATNode ATPreDec (atype n) n ATEmpty
    , symbol "+" *> checkedUnaryIntegerOperand "+" <&> integerPromotedExpr
    , symbol "-" *> checkedUnaryIntegerOperand "-" <&> \n ->
        let promoted = integerPromotedExpr n
         in ATNode ATSub (atype promoted) (atNumLit 0) promoted
    , lnot *> checkedUnaryScalarOperand <&> flip (ATNode ATNot (CT.SCAuto CT.CTBool)) ATEmpty
    , tilda *> checkedUnaryIntegerOperand "~" <&> \n ->
        let promoted = integerPromotedExpr n
         in ATNode ATBitNot (atype promoted) promoted ATEmpty
    , addr
    , star *> checkedUnaryDerefOperand >>= deref'
    , factor'
    ]
    where
        checkedUnaryDecayOperand = do
            n <- unary
            rejectUnsupportedNonAddressableArrayDecay n
            pure n

        checkedUnaryDerefOperand =
            unary

        checkedUnaryScalarOperand =
            checkedUnaryDecayOperand >>= requireScalarOperand "invalid operands"

        checkedUnaryIntegerOperand op =
            checkedUnaryDecayOperand
                >>= requireNonFunctionOperand op
                >>= requireIntegerOperand "invalid operands"

        checkedIncDecOperand err =
            unary
                >>= requireModifiableLvalue err
                >>= requireScalarOperand "invalid operands"

        addr = do
            n <- MC.char '&' `notFollowedOp` MC.char '&' >> withSuppressedUnsupportedValueChecks unary
            unsupportedChecksSuppressed <- gets suppressUnsupportedValueChecks
            let
                canTakeAddress =
                    isAddressableUnaryOperand n
                        || ( unsupportedChecksSuppressed
                                && isUnevaluatedRvalueArrayElementLvalue n
                           )
            unless canTakeAddress $
                fail "lvalue required as unary '&' operand"
            let node = atUnary ATAddr (CT.mapTypeKind CT.CTPtr $ atype n) n
            rejectUnsupportedNonAddressableArrayDecay node
            pure node

        factor' = factor >>= allAcc
            where
                allAcc fac = M.option fac $ choice
                    [ callAcc fac
                    , idxAcc fac
                    , memberAcc fac
                    , ptrMemberAcc fac
                    , postInc fac
                    , postDec fac
                    ]

                callAcc fac = do
                    rawParams <- lparen *> M.manyTill (M.try (assign <* comma) M.<|> assign) rparen
                    (callTy, formalParamTys) <- maybe
                        (fail "called object is not a function or function pointer")
                        pure
                        (callableSignature $ atype fac)
                    params <- applyCallArgConversions formalParamTys rawParams
                    let
                        params' = if null params then Nothing else Just params
                    shouldValidateUnsupported <- gets (not . suppressUnsupportedValueChecks)
                    when (shouldValidateUnsupported && isJust params' && containsEscapingStmtExprControlFlow fac) $
                        fail "unsupported control flow in function call callee"
                    allAcc =<< case fac of
                        ATNode (ATFuncPtr name) _ _ _ ->
                            pure $ atNoLeaf (ATCallFunc name params') callTy
                        _ ->
                            pure $ ATNode (ATCallPtr params') callTy fac ATEmpty

                idxAcc fac = do
                    idx <- brackets expr
                    kt <- maybeToParser "invalid operands" (addKind (decayIncompleteArrayExpr fac) idx)
                    ty <- maybeToParser "subscripted value is neither array nor pointer nor vector" $ derefObjectType $ atype kt
                    ty' <- resolveDerefObjectType "incomplete value dereference" ty
                    allAcc $ atUnary ATDeref ty' kt

                memberAcc fac = do
                    member <- period *> identifier
                    structTy <- resolveMemOperandType "invalid use of incomplete type" fac
                    member' <- lookupStructMember member structTy
                    allAcc $ atMemberAcc member' (withType structTy fac)

                ptrMemberAcc fac = do
                    member <- M.try (symbol "->") *> identifier
                    rejectUnsupportedNonAddressableArrayDecay fac
                    structTy <- maybeToParser "invalid type argument of '->'" $ derefObjectType $ atype fac
                    structTy' <- resolveDerefObjectType "invalid use of pointer to incomplete type" structTy
                    member' <- lookupStructMember member structTy'
                    allAcc $ atMemberAcc member' (atUnary ATDeref structTy' fac)

                postInc fac = do
                    _ <- symbol "++"
                    fac' <-
                        requireModifiableLvalue "lvalue required as increment operand" fac
                            >>= requireScalarOperand "invalid operands"
                    allAcc $ atUnary ATPostInc (atype fac') fac'

                postDec fac = do
                    _ <- symbol "--"
                    fac' <-
                        requireModifiableLvalue "lvalue required as decrement operand" fac
                            >>= requireScalarOperand "invalid operands"
                    allAcc $ atUnary ATPostDec (atype fac') fac'

                lookupStructMember member ty =
                    maybeToParser
                        ("no member named '" <> T.unpack member <> "'")
                        (CT.lookupMember member $ CT.toTypeKind ty)

                withType ty (ATNode kind _ lhs rhs) = ATNode kind ty lhs rhs
                withType _ ATEmpty                  = ATEmpty

        deref' = runMaybeT . deref'' >=> maybe M.empty pure
            where
                deref'' n
                    | isFunctionType (atype n) = do
                        lift $ rejectUnsupportedNonAddressableArrayDecay n
                        pure n
                deref'' n = do
                    ty <- MaybeT $ pure (derefObjectType $ atype n)
                    case CT.toTypeKind ty of
                        CT.CTVoid -> lift $ fail "void value not ignored as it ought to be"
                        _ -> do
                            ty' <- lift $ resolveDerefObjectType "incomplete value dereference" ty
                            pure $ atUnary ATDeref ty' (derefOperand n)

                derefOperand n
                    | isPointerArithmeticNode n =
                        n
                    | CT.isArray (atype n) =
                        ATNode ATAddPtr (atype n) n (atNumLit 0)
                    | otherwise =
                        n

                isPointerArithmeticNode (ATNode ATAddPtr _ _ _) = True
                isPointerArithmeticNode (ATNode ATSubPtr _ _ _) = True
                isPointerArithmeticNode _                       = False

factor = choice
    [ atNumLit <$> natural
    , atNumLit <$> charLiteral
    , sizeof
    , alignof
    , strLiteral
    , identifier'
    , parensExprOrStmt
    , ATEmpty <$ M.eof
    ]
    where
        parensExprOrStmt = do
            isStmtExpr <- M.option False $ True <$ M.lookAhead (M.try (lparen *> lbrace))
            if isStmtExpr then GNU.stmtExpr else parens expr

        memOp p deferredKind op opS = p *> choice
            [ memOpType
            , memOpUnary
            ]
            where
                memOpType = M.try (parens absDeclarator)
                    >>= requireCompleteObjectType ("invalid application of '" <> opS <> "' to incomplete type")
                    <&> atNumLit . fromIntegral . op

                memOpUnary = do
                    u <- withSuppressedUnsupportedValueChecks unary >>= requireNonFunctionOperand opS
                    if CT.isCTUndef (atype u) then
                        fail $ opS <> " must be an expression or type"
                    else
                        do
                            uTy <- resolveMemOperandType ("invalid application of '" <> opS <> "' to incomplete type") u
                            let u' = case u of
                                    ATNode kind _ lhs rhs -> ATNode kind uTy lhs rhs
                                    ATEmpty               -> ATEmpty
                            pure $ atUnary deferredKind (CT.SCAuto $ CT.CTLong CT.CTInt) u'

        sizeof = memOp kSizeof ATSizeof CT.sizeof "sizeof"
        alignof = memOp kAlignof ATAlignof CT.alignof "_Alignof"

        strLiteral = stringLiteral >>= registerStringLiteral

        identifier' = do
            pos <- getPosState
            ident <- identifier
            gets (lookupVar ident) >>= \case
                FoundGVar gvar -> do
                    let declaredTy = PV.gvtype gvar
                    resolvedTy <- gets (`normalizeCompletedStorageClass` declaredTy)
                    pure $
                        atGVar
                            resolvedTy
                            ident
                FoundLVar sct ->
                    gets (\cd -> normalizeCompletedStorageClass cd (PV.lvtype sct))
                        >>= \resolvedTy -> return $ atLVar resolvedTy (PV.rbpOffset sct)
                FoundEnum sct ->
                    return $ treealize sct
                FoundFunc sct ->
                    gets (\cd -> normalizeCompletedStorageClass cd (PSF.fntype sct))
                        >>= \resolvedTy -> return $ atNoLeaf (ATFuncPtr ident) resolvedTy
                FoundTypedef _ ->
                    fail $ "'" <> T.unpack ident <> "' is a typedef name, not an expression"
                NotFound ->
                    M.try (fnCall ident pos)
                        M.<|> fail ("The '" <> T.unpack ident <> "' is not defined identifier")
            where
                fnCall ident pos = do
                    rawParams <- lparen *> M.manyTill (M.try (assign <* comma) M.<|> assign) rparen
                    gets (lookupFunction ident) >>= \case
                        -- TODO: set warning message
                        -- TODO: Infer the return type of a function
                        Nothing ->
                            do
                                params <- applyCallArgConversions Nothing rawParams
                                let params' = if null params then Nothing else Just params
                                    implicitFnTy = CT.SCAuto $ CT.CTFunc CT.CTInt []
                                shadowingGlobal <- gets (isJust . lookupGVar ident)
                                unless shadowingGlobal $
                                    registerFunc False True implicitFnTy ident
                                pushWarn pos ("the function '" <> T.unpack ident <> "' is not declared.")
                                pure $ atNoLeaf (ATCallFunc ident params') (CT.SCAuto CT.CTInt)
                        Just fn -> do
                            resolvedFnTy <- gets (\cd -> normalizeCompletedStorageClass cd (PSF.fntype fn))
                            (callTy, formalParamTys) <- maybe
                                (fail "internal compiler error: function lookup returned non-callable type")
                                pure
                                (callableSignature resolvedFnTy)
                            params <- applyCallArgConversions formalParamTys rawParams
                            let
                                params' = if null params then Nothing else Just params
                            pure $ atNoLeaf (ATCallFunc ident params') callTy
