{-# LANGUAGE LambdaCase #-}
{-|
Module      : Htcc.Asm.Generate
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The executable module for compilation
-}
module Htcc.Asm.Generate (
    InputCCode,
    normalizeAsmInput,
    prepareAsmInput,
    prepareVisualizableInput,
    -- * Generator
    casm',
    casmNormalized'
) where

import           Control.Applicative                         ((<|>))
import           Control.Monad                               (when)
import           Data.Bits                                   (Bits)
import           Data.Foldable                               (for_, traverse_)
import qualified Data.Map.Strict                             as M
import           Data.Maybe                                  (fromMaybe, isJust)
import qualified Data.Text                                   as T
import           Htcc.Asm.Generate.Core
import           Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Structure                as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text   as IT
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser                                 (ASTs, ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..))
import           Htcc.Parser.Combinators.Program             (convertCallArgsWith,
                                                              foldGlobalInitWith,
                                                              isInvalidAggregateValueConversion)
import           Htcc.Parser.Combinators.Utils               (conditionalResultType,
                                                              containsEscapingStmtExprControlFlow,
                                                              decayExprType,
                                                              isInvalidFunctionPointerValue,
                                                              isInvalidObjectPointerValue,
                                                              requiresUnsupportedNonAddressableArrayDecay)
import           Htcc.Parser.ConstructionData.Core           (hasIncompleteObjectType)
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import           Htcc.Parser.ConstructionData.Scope.Var      (GVar (..),
                                                              GVarInitWith (..),
                                                              GlobalVars,
                                                              Literals,
                                                              materializeTentativeIncompleteArray)

-- | input string, C source code
type InputCCode = T.Text

normalizeGlobalInitializers :: (Integral i, Bits i, Read i, Show i, Ord i) => GlobalVars i -> Either String (GlobalVars i)
normalizeGlobalInitializers = M.traverseWithKey resolveGlobalInit
    where
        resolveGlobalInit _ gvar = case initWith gvar of
            GVarInitWithAST ast ->
                (\ginit -> gvar { initWith = ginit })
                    <$> foldGlobalInitWith (gvtype gvar) ast
            _ ->
                Right gvar

mergedGlobalType :: Eq i => Maybe (GlobalVars i) -> T.Text -> CT.StorageClass i -> CT.StorageClass i
mergedGlobalType maybeGVars name currentTy =
    maybe currentTy mergeDeclaredGlobalType $
        maybeGVars >>= M.lookup name
    where
        mergeDeclaredGlobalType gvar =
            let declaredTy = gvtype gvar
             in maybe declaredTy
                    (\mergedTy -> CT.mapTypeKind (const mergedTy) declaredTy)
                    ( CT.mergeCompatibleTypeKinds
                        (CT.toTypeKind declaredTy)
                        (CT.toTypeKind currentTy)
                        <|> CT.mergeCompatibleTypeKinds
                            (CT.toTypeKind currentTy)
                            (CT.toTypeKind declaredTy)
                    )

retypeResolvedGlobalRefs :: Eq i => GlobalVars i -> ASTs i -> ASTs i
retypeResolvedGlobalRefs = map . retypeResolvedGlobalRefsInATree
    where
        retypeResolvedGlobalRefsInATKindFor gvars kind = case kind of
            ATForkw      -> ATForkw
            ATForInit at -> ATForInit $ retypeResolvedGlobalRefsInATree gvars at
            ATForCond at -> ATForCond $ retypeResolvedGlobalRefsInATree gvars at
            ATForIncr at -> ATForIncr $ retypeResolvedGlobalRefsInATree gvars at
            ATForStmt at -> ATForStmt $ retypeResolvedGlobalRefsInATree gvars at

        retypeResolvedGlobalRefsInATKind gvars kind = case kind of
            ATConditional cond tr fl ->
                ATConditional
                    (retypeResolvedGlobalRefsInATree gvars cond)
                    (retypeResolvedGlobalRefsInATree gvars tr)
                    (retypeResolvedGlobalRefsInATree gvars fl)
            ATSwitch cond cases ->
                ATSwitch
                    (retypeResolvedGlobalRefsInATree gvars cond)
                    (map (retypeResolvedGlobalRefsInATree gvars) cases)
            ATFor kinds ->
                ATFor $ map (retypeResolvedGlobalRefsInATKindFor gvars) kinds
            ATBlock ats ->
                ATBlock $ map (retypeResolvedGlobalRefsInATree gvars) ats
            ATStmtExpr ats ->
                ATStmtExpr $ map (retypeResolvedGlobalRefsInATree gvars) ats
            ATNull at ->
                ATNull $ retypeResolvedGlobalRefsInATree gvars at
            ATDefFunc name args ->
                ATDefFunc name $ map (retypeResolvedGlobalRefsInATree gvars) <$> args
            ATCallFunc name args ->
                ATCallFunc name $ map (retypeResolvedGlobalRefsInATree gvars) <$> args
            ATCallPtr args ->
                ATCallPtr $ map (retypeResolvedGlobalRefsInATree gvars) <$> args
            ATGVar ty name ->
                ATGVar (mergedGlobalType (Just gvars) name ty) name
            _ ->
                kind

        retypeResolvedGlobalRefsInATree _ ATEmpty = ATEmpty
        retypeResolvedGlobalRefsInATree gvars (ATNode kind ty lhs rhs) =
            ATNode
                retypedKind
                retypedTy
                (retypeResolvedGlobalRefsInATree gvars lhs)
                (retypeResolvedGlobalRefsInATree gvars rhs)
            where
                retypedKind = retypeResolvedGlobalRefsInATKind gvars kind
                retypedTy = case retypedKind of
                    ATGVar resolvedTy _ -> resolvedTy
                    _                   -> ty

normalizeAsmInput :: (Integral i, Bits i, Read i, Show i, Ord i)
    => ASTs i
    -> GlobalVars i
    -> Either String (ASTs i, GlobalVars i)
normalizeAsmInput atl gvars = do
    normalizedGVars <- normalizeGlobalInitializers gvars
    pure (retypeResolvedGlobalRefs normalizedGVars atl, normalizedGVars)

data MergedRevalidationMode
    = StrictMergedRevalidation
    | GlobalInitializerMergedRevalidation
    | VisualizableMergedRevalidation

mergedCallableSignature :: Ord i => CT.StorageClass i -> Maybe (CT.StorageClass i, Maybe [CT.StorageClass i])
mergedCallableSignature ty = case CT.toTypeKind ty of
    CT.CTFunc retTy params ->
        Just (CT.SCAuto retTy, explicitFunctionParamTypes params)
    CT.CTPtr (CT.CTFunc retTy params) ->
        Just (CT.SCAuto retTy, explicitFunctionParamTypes params)
    _ ->
        Nothing
    where
        explicitFunctionParamTypes [] = Nothing
        explicitFunctionParamTypes [(CT.CTVoid, Nothing)] = Just []
        explicitFunctionParamTypes params =
            Just $ map (CT.SCAuto . canonicalizeFunctionParamType . fst) params

        canonicalizeFunctionParamType = \case
            CT.CTArray _ elemTy -> CT.CTPtr elemTy
            CT.CTIncomplete (CT.IncompleteArray elemTy) -> CT.CTPtr elemTy
            CT.CTFunc retTy params -> CT.CTPtr $ CT.CTFunc retTy params
            other -> other

mergedFunctionParamBindings :: Ord i => CT.StorageClass i -> Maybe [ATree i] -> M.Map i (CT.StorageClass i)
mergedFunctionParamBindings ty maybeArgs = case (maybeArgs, mergedParamTypes ty) of
    (Just args, Just paramTys)
        | length args == length paramTys
        , Just offsets <- traverse paramOffset args ->
            M.fromList $ zip offsets paramTys
    _ ->
        M.empty
    where
        mergedParamTypes resolvedTy = case mergedCallableSignature resolvedTy of
            Just (_, formalParamTys) -> formalParamTys
            Nothing                  -> Nothing

        paramOffset (ATNode (ATLVar _ offset) _ _ _) = Just offset
        paramOffset _                                = Nothing

mergeCompatibleStorageClass :: Eq i => CT.StorageClass i -> CT.StorageClass i -> CT.StorageClass i
mergeCompatibleStorageClass preferred current =
    maybe preferred (\mergedTy -> CT.mapTypeKind (const mergedTy) preferred) $
        CT.mergeCompatibleTypeKinds preferredTy currentTy
            <|> CT.mergeCompatibleTypeKinds currentTy preferredTy
    where
        preferredTy = CT.toTypeKind preferred
        currentTy = CT.toTypeKind current

mergeCompatibleParamType :: Eq i => CT.TypeKind i -> CT.StorageClass i -> CT.TypeKind i
mergeCompatibleParamType preferred current =
    fromMaybe preferred $
        CT.mergeCompatibleTypeKinds preferred currentTy
            <|> CT.mergeCompatibleTypeKinds currentTy preferred
    where
        currentTy = CT.toTypeKind current

mergedFunctionType :: Eq i => PF.Functions i -> T.Text -> CT.StorageClass i -> CT.StorageClass i
mergedFunctionType funcs name fallback =
    maybe fallback (\fn -> mergeCompatibleStorageClass (PF.fntype fn) fallback) $ M.lookup name funcs

functionReturnType :: CT.StorageClass i -> Maybe (CT.StorageClass i)
functionReturnType ty = case CT.toTypeKind ty of
    CT.CTFunc retTy _ -> Just $ CT.SCAuto retTy
    _                 -> Nothing

isUnsupportedByValueAggregateType :: Ord i => CT.StorageClass i -> Bool
isUnsupportedByValueAggregateType ty =
    CT.isCTStruct ty && CT.sizeof ty > 8

unsupportedByValueFunctionReturnType :: Ord i => CT.StorageClass i -> Bool
unsupportedByValueFunctionReturnType ty = case CT.toTypeKind ty of
    CT.CTFunc retTy _ ->
        isUnsupportedByValueAggregateType $ CT.SCAuto retTy
    _ ->
        False

functionTypeWithMergedCall :: Eq i => CT.StorageClass i -> CT.StorageClass i -> [ATree i] -> CT.StorageClass i
functionTypeWithMergedCall fnTy currentReturnTy currentArgs = case CT.toTypeKind fnTy of
    CT.CTFunc retTy params ->
        CT.mapTypeKind (const $ CT.CTFunc (mergedReturnTy retTy) (mergedParams params)) fnTy
    _ ->
        fnTy
    where
        currentRetTy = CT.toTypeKind currentReturnTy
        mergedReturnTy retTy =
            fromMaybe retTy $
                CT.mergeCompatibleTypeKinds retTy currentRetTy
                    <|> CT.mergeCompatibleTypeKinds currentRetTy retTy

        mergedParams params
            | length params == length currentArgs =
                zipWith mergedParam params currentArgs
            | otherwise =
                params

        mergedParam (paramTy, ident) arg =
            (mergeCompatibleParamType paramTy (atype arg), ident)

derefMergedObjectType :: Ord i => CT.StorageClass i -> Maybe (CT.StorageClass i)
derefMergedObjectType ty = case CT.toTypeKind ty of
    CT.CTArray n (CT.CTIncomplete (CT.IncompleteArray elemTy)) ->
        Just $ CT.mapTypeKind (const $ CT.CTArray n elemTy) ty
    _ ->
        CT.deref ty

objectPointerRetyped :: Ord i => ATree i -> ATree i -> ATree i -> ATree i -> Bool
objectPointerRetyped originalLhs originalRhs lhs rhs =
    atype lhs /= atype originalLhs
        || decayExprType (atype rhs) /= decayExprType (atype originalRhs)

invalidAssignmentOperands :: (Ord i, Bits i, Integral i) => ATKind i -> ATree i -> ATree i -> ATree i -> ATree i -> Bool
invalidAssignmentOperands kind originalLhs originalRhs lhs rhs = case kind of
    ATAssign ->
        isInvalidFunctionPointerValue (atype lhs) rhs
            || isInvalidAggregateValueConversion (atype lhs) rhs
            || ( objectPointerRetyped originalLhs originalRhs lhs rhs
                    && isInvalidObjectPointerValue (atype lhs) rhs
               )
    _ ->
        isInvalidCompoundAssignmentOperands kind lhs rhs

isInvalidCompoundAssignmentOperands :: Ord i => ATKind i -> ATree i -> ATree i -> Bool
isInvalidCompoundAssignmentOperands kind lhs rhs = case kind of
    ATAddPtrAssign ->
        not $
            isPointerArithmeticOperandType (atype lhs)
                && isIntegerOperandType (atype rhs)
    ATSubPtrAssign ->
        not $
            isPointerArithmeticOperandType (atype lhs)
                && isIntegerOperandType (atype rhs)
    ATAddAssign ->
        invalidIntegerOperands lhs rhs
    ATSubAssign ->
        invalidIntegerOperands lhs rhs
    ATMulAssign ->
        invalidIntegerOperands lhs rhs
    ATDivAssign ->
        invalidIntegerOperands lhs rhs
    ATAndAssign ->
        invalidIntegerOperands lhs rhs
    ATOrAssign ->
        invalidIntegerOperands lhs rhs
    ATXorAssign ->
        invalidIntegerOperands lhs rhs
    ATShlAssign ->
        invalidIntegerOperands lhs rhs
    ATShrAssign ->
        invalidIntegerOperands lhs rhs
    _ ->
        False

invalidRefreshedOperandUse :: (Ord i, Bits i, Integral i) => ATKind i -> CT.StorageClass i -> ATree i -> ATree i -> Maybe String
invalidRefreshedOperandUse kind nodeTy lhs rhs = case kind of
    ATIf
        | invalidConditionOperand lhs -> Just "invalid condition type"
    ATWhile
        | invalidConditionOperand lhs -> Just "invalid condition type"
    ATSwitch cond _
        | invalidConditionOperand cond -> Just "invalid condition type"
    ATFor kinds ->
        foldr ((<|>) . invalidForOperandUse) Nothing kinds
    ATConditional cond ATEmpty el ->
        invalidConditionalOperands cond cond el
    ATConditional cond th el ->
        invalidConditionalOperands cond th el
    ATCast ->
        invalidCastOperands nodeTy lhs
    ATPreInc
        | invalidScalarOperand lhs -> Just "invalid operands"
    ATPreDec
        | invalidScalarOperand lhs -> Just "invalid operands"
    ATPostInc
        | invalidScalarOperand lhs -> Just "invalid operands"
    ATPostDec
        | invalidScalarOperand lhs -> Just "invalid operands"
    ATNot
        | invalidScalarOperand lhs -> Just "invalid operands"
    ATBitNot
        | invalidIntegerOperand lhs -> Just "invalid operands"
    ATAdd
        | invalidArithmeticOperands lhs rhs -> Just "invalid operands"
    ATSub
        | invalidArithmeticOperands lhs rhs -> Just "invalid operands"
    ATAddPtr
        | not (isPointerArithmeticOperandType (atype lhs) && isIntegerOperandType (atype rhs)) -> Just "invalid operands"
    ATSubPtr
        | not (isPointerArithmeticOperandType (atype lhs) && isIntegerOperandType (atype rhs)) -> Just "invalid operands"
    ATPtrDis
        | not (isPointerArithmeticOperandType (atype lhs) && isPointerArithmeticOperandType (atype rhs)) -> Just "invalid operands"
    ATMul
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATDiv
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATMod
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATShl
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATShr
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATLT
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATLEQ
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATGT
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATGEQ
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATEQ
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATNEQ
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATLAnd
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATLOr
        | invalidScalarOperands lhs rhs -> Just "invalid operands"
    ATAnd
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATOr
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    ATXor
        | invalidIntegerOperands lhs rhs -> Just "invalid operands"
    _ ->
        Nothing

invalidForOperandUse :: (Ord i, Bits i, Integral i) => ATKindFor i -> Maybe String
invalidForOperandUse = \case
    ATForCond cond
        | cond /= ATEmpty && invalidConditionOperand cond -> Just "invalid condition type"
    _ ->
        Nothing

invalidConditionalOperands :: (Ord i, Bits i, Integral i) => ATree i -> ATree i -> ATree i -> Maybe String
invalidConditionalOperands cond lhs rhs
    | invalidConditionOperand cond = Just "invalid condition type"
    | otherwise = maybe (Just "invalid operands") (const Nothing) $ conditionalResultType lhs rhs

invalidCastOperands :: Eq i => CT.StorageClass i -> ATree i -> Maybe String
invalidCastOperands targetTy operand
    | not (isVoidType targetTy)
        && isAggregateType (atype operand)
        && (not (isAggregateType targetTy) || isInvalidAggregateValueConversion targetTy operand) =
            Just "invalid cast operand"
    | otherwise = Nothing

invalidConditionOperand :: Ord i => ATree i -> Bool
invalidConditionOperand = invalidScalarOperand

invalidScalarOperand :: Ord i => ATree i -> Bool
invalidScalarOperand =
    not . isScalarOperandType . atype

invalidScalarOperands :: Ord i => ATree i -> ATree i -> Bool
invalidScalarOperands lhs rhs =
    invalidScalarOperand lhs || invalidScalarOperand rhs

invalidIntegerOperand :: Ord i => ATree i -> Bool
invalidIntegerOperand =
    not . isIntegerOperandType . atype

invalidIntegerOperands :: Ord i => ATree i -> ATree i -> Bool
invalidIntegerOperands lhs rhs =
    invalidIntegerOperand lhs || invalidIntegerOperand rhs

invalidArithmeticOperands :: Ord i => ATree i -> ATree i -> Bool
invalidArithmeticOperands lhs rhs =
    not $
        isArithmeticOperandType (atype lhs)
            && isArithmeticOperandType (atype rhs)

isScalarOperandType :: Ord i => CT.StorageClass i -> Bool
isScalarOperandType ty =
    CT.isIntegral decayedTy || isPointerType decayedTy
    where
        decayedTy = decayExprType ty

isIntegerOperandType :: Ord i => CT.StorageClass i -> Bool
isIntegerOperandType =
    CT.isIntegral . decayExprType

isArithmeticOperandType :: Ord i => CT.StorageClass i -> Bool
isArithmeticOperandType =
    CT.isFundamental . decayExprType

isPointerArithmeticOperandType :: Ord i => CT.StorageClass i -> Bool
isPointerArithmeticOperandType ty =
    maybe False (not . isFunctionType) $ CT.deref ty

isPointerType :: CT.StorageClass i -> Bool
isPointerType ty = case CT.toTypeKind ty of
    CT.CTPtr _ -> True
    _          -> False

isFunctionType :: CT.StorageClass i -> Bool
isFunctionType ty = case CT.toTypeKind ty of
    CT.CTFunc _ _ -> True
    _             -> False

isAggregateType :: CT.StorageClass i -> Bool
isAggregateType ty =
    CT.isCTStruct ty || CT.isIncompleteStruct ty

isVoidType :: CT.StorageClass i -> Bool
isVoidType ty = case CT.toTypeKind ty of
    CT.CTVoid -> True
    _         -> False

invalidIncompletePointerArithmetic :: Ord i => ATKind i -> ATree i -> ATree i -> Bool
invalidIncompletePointerArithmetic kind lhs rhs = case kind of
    ATAddPtr ->
        hasIncompletePointerTarget lhs
    ATSubPtr ->
        hasIncompletePointerTarget lhs
    ATPtrDis ->
        hasIncompletePointerTarget lhs || hasIncompletePointerTarget rhs
    ATAddPtrAssign ->
        hasIncompletePointerTarget lhs
    ATSubPtrAssign ->
        hasIncompletePointerTarget lhs
    ATPreInc ->
        hasIncompletePointerTarget lhs
    ATPreDec ->
        hasIncompletePointerTarget lhs
    ATPostInc ->
        hasIncompletePointerTarget lhs
    ATPostDec ->
        hasIncompletePointerTarget lhs
    _ ->
        False
    where
        hasIncompletePointerTarget expr =
            maybe False hasIncompleteObjectType $ CT.deref (atype expr)

invalidIncompleteMemOp :: Ord i => ATKind i -> ATree i -> Bool
invalidIncompleteMemOp kind lhs = case kind of
    ATSizeof ->
        hasIncompleteObjectType $ atype lhs
    ATAlignof ->
        hasIncompleteObjectType $ atype lhs
    _ ->
        False

invalidReturnValue :: (Ord i, Bits i, Integral i) => Maybe (CT.StorageClass i) -> ATKind i -> ATree i -> Bool
invalidReturnValue currentReturnTy kind returnedExpr = case kind of
    ATReturn ->
        maybe False
            (\returnTy ->
                returnedExpr /= ATEmpty
                    && ( isInvalidFunctionPointerValue returnTy returnedExpr
                            || isInvalidObjectPointerValue returnTy returnedExpr
                            || isInvalidAggregateValueConversion returnTy returnedExpr
                       )
            )
            currentReturnTy
    _ ->
        False

invalidAddressOfOperand :: Ord i => Bool -> ATree i -> Bool
invalidAddressOfOperand valueChecks operand =
    not (isAddressableUnaryOperand operand)
        && ( valueChecks
                || not (isUnevaluatedRvalueArrayElementAddressOperand operand)
           )

isAddressableUnaryOperand :: Ord i => ATree i -> Bool
isAddressableUnaryOperand (ATNode kind _ lhs _) = case kind of
    ATLVar _ _    -> True
    ATGVar _ _    -> True
    ATFuncPtr _   -> True
    ATMemberAcc _ -> isAddressableLvalueExpr lhs
    ATDeref       -> isAddressableDerefOperand lhs
    _             -> False
isAddressableUnaryOperand _ = False

isAddressableLvalueExpr :: Ord i => ATree i -> Bool
isAddressableLvalueExpr (ATNode kind _ lhs _) = case kind of
    ATLVar _ _    -> True
    ATGVar _ _    -> True
    ATMemberAcc _ -> isAddressableLvalueExpr lhs
    ATDeref       -> isAddressableDerefOperand lhs
    _             -> False
isAddressableLvalueExpr _ = False

isAddressableDerefOperand :: Ord i => ATree i -> Bool
isAddressableDerefOperand (ATNode ATAddPtr _ arrayExpr _)
    | CT.isArray (atype arrayExpr) =
        isAddressableLvalueExpr arrayExpr
isAddressableDerefOperand (ATNode ATSubPtr _ arrayExpr _)
    | CT.isArray (atype arrayExpr) =
        isAddressableLvalueExpr arrayExpr
isAddressableDerefOperand operand
    | CT.isArray (atype operand) =
        isAddressableLvalueExpr operand
isAddressableDerefOperand operand = isJust $ CT.deref (atype operand)

isUnevaluatedRvalueArrayElementAddressOperand :: Ord i => ATree i -> Bool
isUnevaluatedRvalueArrayElementAddressOperand (ATNode ATDeref _ (ATNode ATAddPtr _ arrayExpr _) _)
    | CT.isArray (atype arrayExpr) =
        not $ isAddressableLvalueExpr arrayExpr
isUnevaluatedRvalueArrayElementAddressOperand (ATNode ATDeref _ (ATNode ATSubPtr _ arrayExpr _) _)
    | CT.isArray (atype arrayExpr) =
        not $ isAddressableLvalueExpr arrayExpr
isUnevaluatedRvalueArrayElementAddressOperand (ATNode ATDeref _ arrayExpr _)
    | CT.isArray (atype arrayExpr) =
        not $ isAddressableLvalueExpr arrayExpr
isUnevaluatedRvalueArrayElementAddressOperand (ATNode (ATMemberAcc _) _ lhs _) =
    isUnevaluatedRvalueArrayElementAddressOperand lhs
isUnevaluatedRvalueArrayElementAddressOperand _ = False

unsupportedValueUseContext :: ATKind i -> Bool
unsupportedValueUseContext = \case
    ATExprStmt         -> True
    ATReturn           -> True
    ATIf               -> True
    ATWhile            -> True
    ATSwitch _ _       -> True
    ATFor _            -> True
    ATAssign           -> True
    ATAddAssign        -> True
    ATSubAssign        -> True
    ATMulAssign        -> True
    ATDivAssign        -> True
    ATAddPtrAssign     -> True
    ATSubPtrAssign     -> True
    ATAndAssign        -> True
    ATOrAssign         -> True
    ATXorAssign        -> True
    ATShlAssign        -> True
    ATShrAssign        -> True
    ATComma            -> True
    ATConditional {}    -> True
    ATCast             -> True
    ATDeref            -> True
    ATPreInc           -> True
    ATPreDec           -> True
    ATPostInc          -> True
    ATPostDec          -> True
    ATNot              -> True
    ATBitNot           -> True
    ATAdd              -> True
    ATSub              -> True
    ATMul              -> True
    ATDiv              -> True
    ATMod              -> True
    ATShl              -> True
    ATShr              -> True
    ATLT               -> True
    ATLEQ              -> True
    ATGT               -> True
    ATGEQ              -> True
    ATEQ               -> True
    ATNEQ              -> True
    ATLAnd             -> True
    ATLOr              -> True
    ATAnd              -> True
    ATOr               -> True
    ATXor              -> True
    _                  -> False

refreshMergedValueTypes :: (Ord i, Bits i, Integral i) => PF.Functions i -> Maybe (GlobalVars i) -> ATree i -> ATree i
refreshMergedValueTypes funcs maybeGVars = go
    where
        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

        refreshKindFor = \case
            ATForkw ->
                ATForkw
            ATForInit at ->
                ATForInit $ go at
            ATForCond at ->
                ATForCond $ go at
            ATForIncr at ->
                ATForIncr $ go at
            ATForStmt at ->
                ATForStmt $ go at

        refreshKind = \case
            ATConditional cond tr fl ->
                ATConditional (go cond) (go tr) (go fl)
            ATSwitch cond cases ->
                ATSwitch (go cond) (map go cases)
            ATFor kinds ->
                ATFor $ map refreshKindFor kinds
            ATBlock ats ->
                ATBlock $ map go ats
            ATStmtExpr ats ->
                ATStmtExpr $ map go ats
            ATNull at ->
                ATNull $ go at
            ATDefFunc name args ->
                ATDefFunc name $ map go <$> args
            ATCallFunc name args ->
                ATCallFunc name $ map go <$> args
            ATCallPtr args ->
                ATCallPtr $ map go <$> args
            ATGVar ty name ->
                ATGVar (mergedGlobalType maybeGVars name ty) name
            other ->
                other

        go ATEmpty = ATEmpty
        go (ATNode kind ty lhs rhs) =
            ATNode kind' ty' lhs' rhs'
            where
                kind' = refreshKind kind
                lhs' = go lhs
                rhs' = go rhs
                ty' = case kind' of
                    ATConditional cond ATEmpty el ->
                        fromMaybe ty $ conditionalResultType cond el
                    ATConditional _ tr fl ->
                        fromMaybe ty $ conditionalResultType tr fl
                    ATDefFunc name _ ->
                        mergedFunctionType funcs name ty
                    ATGVar resolvedTy _ ->
                        resolvedTy
                    ATFuncPtr name ->
                        mergedFunctionType funcs name ty
                    ATAddr ->
                        CT.mapTypeKind CT.CTPtr $ atype lhs'
                    ATDeref ->
                        fromMaybe ty $ derefMergedObjectType $ atype lhs'
                    ATAddPtr ->
                        decayExprType $ atype lhs'
                    ATSubPtr ->
                        decayExprType $ atype lhs'
                    ATAssign ->
                        atype lhs'
                    ATAddPtrAssign ->
                        atype lhs'
                    ATSubPtrAssign ->
                        atype lhs'
                    ATComma ->
                        decayExprType $ atype rhs'
                    ATStmtExpr ats ->
                        maybe ty (decayExprType . atype) $ lastMaybe ats
                    _ ->
                        ty


revalidateMergedFunctionTree
    :: (Ord i, Bits i, Integral i)
    => MergedRevalidationMode
    -> PF.Functions i
    -> Maybe (GlobalVars i)
    -> ATree i
    -> Either String (ATree i)
revalidateMergedFunctionTree mode funcs maybeGVars = revalidateTree True True M.empty Nothing
    where
        validateDeferredCodegenChecks = case mode of
            StrictMergedRevalidation            -> True
            GlobalInitializerMergedRevalidation -> True
            VisualizableMergedRevalidation      -> False

        validateAssignmentChecks = case mode of
            StrictMergedRevalidation            -> True
            GlobalInitializerMergedRevalidation -> False
            VisualizableMergedRevalidation      -> False

        validateDeferredValueChecks valueChecks =
            validateDeferredCodegenChecks && valueChecks

        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

        revalidateKindFor valueChecks paramTys currentReturnTy = \case
            ATForkw ->
                Right ATForkw
            ATForInit at ->
                ATForInit <$> revalidateTree valueChecks True paramTys currentReturnTy at
            ATForCond at ->
                ATForCond <$> revalidateTree valueChecks True paramTys currentReturnTy at
            ATForIncr at ->
                ATForIncr <$> revalidateTree valueChecks True paramTys currentReturnTy at
            ATForStmt at ->
                ATForStmt <$> revalidateTree valueChecks True paramTys currentReturnTy at

        revalidateKind valueChecks paramTys currentReturnTy = \case
            ATConditional cond tr fl ->
                ATConditional
                    <$> revalidateTree valueChecks True paramTys currentReturnTy cond
                    <*> revalidateTree valueChecks True paramTys currentReturnTy tr
                    <*> revalidateTree valueChecks True paramTys currentReturnTy fl
            ATSwitch cond cases ->
                ATSwitch
                    <$> revalidateTree valueChecks True paramTys currentReturnTy cond
                    <*> traverse (revalidateTree valueChecks True paramTys currentReturnTy) cases
            ATFor kinds ->
                ATFor <$> traverse (revalidateKindFor valueChecks paramTys currentReturnTy) kinds
            ATBlock ats ->
                ATBlock <$> traverse (revalidateTree valueChecks True paramTys currentReturnTy) ats
            ATStmtExpr ats ->
                ATStmtExpr <$> traverse (revalidateTree valueChecks True paramTys currentReturnTy) ats
            ATNull at ->
                ATNull <$> revalidateTree valueChecks True paramTys currentReturnTy at
            other ->
                Right other

        revalidateTree _ _ _ _ ATEmpty = Right ATEmpty
        revalidateTree valueChecks _ currentParamTys currentReturnTy (ATNode (ATDefFunc name args) ty lhs rhs) = do
            let resolvedFnTy = mergedFunctionType funcs name ty
                nestedReturnTy = functionReturnType resolvedFnTy
                nestedParamTys = mergedFunctionParamBindings resolvedFnTy args
            when (validateDeferredCodegenChecks && unsupportedByValueFunctionReturnType resolvedFnTy) $
                Left "unsupported by-value function return type"
            args' <- traverse (traverse $ revalidateTree True True nestedParamTys nestedReturnTy) args
            lhs' <- revalidateTree True True nestedParamTys nestedReturnTy lhs
            rhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy rhs
            pure $ ATNode (ATDefFunc name args') resolvedFnTy lhs' rhs'
        revalidateTree valueChecks _ currentParamTys currentReturnTy (ATNode (ATLVar ty offset) _ lhs rhs) = do
            lhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy rhs
            let resolvedTy = fromMaybe ty $ M.lookup offset currentParamTys
            pure $ ATNode (ATLVar resolvedTy offset) resolvedTy lhs' rhs'
        revalidateTree valueChecks _ currentParamTys currentReturnTy (ATNode (ATCallFunc name args) ty lhs rhs) = do
            lhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy rhs
            args' <- traverse (traverse $ revalidateTree valueChecks True currentParamTys currentReturnTy) args
            case M.lookup name funcs of
                Nothing ->
                    pure $ ATNode (ATCallFunc name args') ty lhs' rhs'
                Just fn -> do
                    let resolvedFnTy = functionTypeWithMergedCall (PF.fntype fn) ty $ fromMaybe [] args'
                    (callTy, formalParamTys) <- maybe
                        (Left "internal compiler error: merged function lookup returned non-callable type")
                        Right
                        (mergedCallableSignature resolvedFnTy)
                    params <- convertCallArgsWith (validateDeferredValueChecks valueChecks) formalParamTys $ fromMaybe [] args'
                    let params' = if null params then Nothing else Just params
                    pure $ ATNode (ATCallFunc name params') callTy lhs' rhs'
        revalidateTree valueChecks _ currentParamTys currentReturnTy (ATNode (ATCallPtr args) _ lhs rhs) = do
            lhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy rhs
            args' <- traverse (traverse $ revalidateTree valueChecks True currentParamTys currentReturnTy) args
            (callTy, formalParamTys) <- maybe
                (Left "called object is not a function or function pointer")
                Right
                (mergedCallableSignature $ atype lhs')
            params <- convertCallArgsWith (validateDeferredValueChecks valueChecks) formalParamTys $ fromMaybe [] args'
            let params' = if null params then Nothing else Just params
            when (validateDeferredValueChecks valueChecks && isJust params' && containsEscapingStmtExprControlFlow lhs') $
                Left "unsupported control flow in function call callee"
            pure $ ATNode (ATCallPtr params') callTy lhs' rhs'
        revalidateTree valueChecks currentValueChecks currentParamTys currentReturnTy (ATNode kind ty lhs rhs) = do
            let kindValueChecks = case kind of
                    ATSizeof ->
                        False
                    ATAlignof ->
                        False
                    _ ->
                        valueChecks
                lhsCurrentValueChecks = case kind of
                    ATAddr ->
                        False
                    ATMemberAcc _ ->
                        False
                    _ ->
                        True
            kind' <- revalidateKind kindValueChecks currentParamTys currentReturnTy kind
            let nestedReturnTy = case kind' of
                    ATDefFunc name _ ->
                        functionReturnType $ mergedFunctionType funcs name ty
                    _ ->
                        currentReturnTy
            lhs' <- revalidateTree kindValueChecks lhsCurrentValueChecks currentParamTys nestedReturnTy lhs
            rhs' <- revalidateTree valueChecks True currentParamTys currentReturnTy rhs
            let (kind'', ty') = refreshKindAndType kind' ty lhs' rhs'
            when (validateAssignmentChecks && invalidAssignmentOperands kind'' lhs rhs lhs' rhs') $
                Left "invalid operands to assignment"
            when validateDeferredCodegenChecks $
                for_ (invalidRefreshedOperandUse kind'' ty' lhs' rhs') Left
            when (validateDeferredCodegenChecks && invalidIncompletePointerArithmetic kind'' lhs' rhs') $
                Left "invalid use of pointer to incomplete type"
            when (validateDeferredCodegenChecks && invalidIncompleteMemOp kind'' lhs') $
                Left $ case kind'' of
                    ATSizeof -> "invalid application of 'sizeof' to incomplete type"
                    ATAlignof -> "invalid application of '_Alignof' to incomplete type"
                    _ -> "internal compiler error: unexpected incomplete memory operator"
            when (validateDeferredValueChecks kindValueChecks && invalidReturnValue currentReturnTy kind'' lhs') $
                Left "invalid return type"
            when (validateDeferredCodegenChecks && invalidAddressOfOperandKind kindValueChecks kind'' lhs') $
                Left "lvalue required as unary '&' operand"
            let refreshed = ATNode kind'' ty' lhs' rhs'
            when
                ( validateDeferredValueChecks valueChecks
                    && currentValueChecks
                    && unsupportedValueUseContext kind''
                    && requiresUnsupportedNonAddressableArrayDecay refreshed
                ) $
                Left "unsupported non-addressable array member decay"
            pure refreshed

        invalidAddressOfOperandKind valueChecks = \case
            ATAddr ->
                invalidAddressOfOperand valueChecks
            _ ->
                const False

        refreshKindAndType kind currentTy lhs rhs = case kind of
            ATConditional cond ATEmpty el ->
                ( kind
                , fromMaybe currentTy $ conditionalResultType cond el
                )
            ATConditional _ th el ->
                ( kind
                , fromMaybe currentTy $ conditionalResultType th el
                )
            ATDefFunc name _ ->
                (kind, mergedFunctionType funcs name currentTy)
            ATGVar _ name ->
                let resolvedTy = mergedGlobalType maybeGVars name currentTy
                 in (ATGVar resolvedTy name, resolvedTy)
            ATFuncPtr name ->
                let resolvedTy = mergedFunctionType funcs name currentTy
                 in (ATFuncPtr name, resolvedTy)
            ATAddr ->
                (ATAddr, CT.mapTypeKind CT.CTPtr $ atype lhs)
            ATDeref ->
                (ATDeref, fromMaybe currentTy $ derefMergedObjectType $ atype lhs)
            ATPreInc ->
                (ATPreInc, atype lhs)
            ATPreDec ->
                (ATPreDec, atype lhs)
            ATPostInc ->
                (ATPostInc, atype lhs)
            ATPostDec ->
                (ATPostDec, atype lhs)
            ATAddPtr ->
                (ATAddPtr, decayExprType $ atype lhs)
            ATSubPtr ->
                (ATSubPtr, decayExprType $ atype lhs)
            ATPtrDis ->
                (ATPtrDis, CT.SCAuto $ CT.CTLong CT.CTInt)
            ATAssign ->
                (ATAssign, atype lhs)
            ATAddPtrAssign ->
                (ATAddPtrAssign, atype lhs)
            ATSubPtrAssign ->
                (ATSubPtrAssign, atype lhs)
            ATComma ->
                (ATComma, decayExprType $ atype rhs)
            ATStmtExpr ats ->
                (kind, maybe currentTy (decayExprType . atype) $ lastMaybe ats)
            _ ->
                (kind, currentTy)

revalidateMergedFunctionCalls
    :: (Ord i, Bits i, Integral i)
    => PF.Functions i
    -> GlobalVars i
    -> ASTs i
    -> Either String (ASTs i)
revalidateMergedFunctionCalls =
    revalidateMergedFunctionCallsWithMode StrictMergedRevalidation

revalidateMergedFunctionCallsWithMode
    :: (Ord i, Bits i, Integral i)
    => MergedRevalidationMode
    -> PF.Functions i
    -> GlobalVars i
    -> ASTs i
    -> Either String (ASTs i)
revalidateMergedFunctionCallsWithMode mode funcs gvars =
    traverse $ revalidateMergedFunctionTree mode funcs (Just gvars)

revalidateMergedGlobalInitializers
    :: (Ord i, Bits i, Integral i)
    => PF.Functions i
    -> GlobalVars i
    -> Either String (GlobalVars i)
revalidateMergedGlobalInitializers funcs gvars =
    traverse revalidateGVar gvars
    where
        revalidateGVar gvar = case initWith gvar of
            GVarInitWithAST ast -> do
                ast' <- revalidateMergedFunctionTree GlobalInitializerMergedRevalidation funcs (Just gvars) ast
                validateGlobalInitializerAssignments ast ast'
                pure $ gvar { initWith = GVarInitWithAST ast' }
            _ ->
                Right gvar

        validateGlobalInitializerAssignments ATEmpty ATEmpty = Right ()
        validateGlobalInitializerAssignments
            (ATNode originalKind _ originalLhs originalRhs)
            (ATNode kind _ lhs rhs) = do
                when (invalidIncompletePointerArithmetic kind lhs rhs) $
                    Left "invalid use of pointer to incomplete type"
                when (invalidIncompleteMemOp kind lhs) $
                    Left $ case kind of
                        ATSizeof -> "invalid application of 'sizeof' to incomplete type"
                        ATAlignof -> "invalid application of '_Alignof' to incomplete type"
                        _ -> "internal compiler error: unexpected incomplete memory operator"
                case (originalKind, kind) of
                    (ATBlock originalAts, ATBlock ats)
                        | length originalAts == length ats ->
                            traverse_ (uncurry validateGlobalInitializerAssignments) $ zip originalAts ats
                        | otherwise ->
                            Left "internal compiler error: global initializer shape changed during revalidation"
                    (ATExprStmt, ATExprStmt) ->
                        validateGlobalInitializerAssignments originalLhs lhs
                    (_, ATAssign) -> do
                        when (invalidAssignmentOperands kind originalLhs originalRhs lhs rhs) $
                            Left "invalid initializer for scalar object"
                        validateGlobalInitializerAssignments originalLhs lhs
                            *> validateGlobalInitializerAssignments originalRhs rhs
                    _ ->
                        validateGlobalInitializerAssignments originalLhs lhs
                            *> validateGlobalInitializerAssignments originalRhs rhs
        validateGlobalInitializerAssignments _ _ =
            Left "internal compiler error: global initializer shape changed during revalidation"

prepareAsmInput
    :: (Integral i, Bits i, Read i, Show i, Ord i)
    => PF.Functions i
    -> ASTs i
    -> GlobalVars i
    -> Either String (ASTs i, GlobalVars i)
prepareAsmInput funcs asts gvars = do
    let materializedGVars = M.map materializeTentativeIncompleteArray gvars
    revalidatedGVars <- revalidateMergedGlobalInitializers funcs materializedGVars
    (normalizedAsts, normalizedGVars) <- normalizeAsmInput asts revalidatedGVars
    revalidatedAsts <- revalidateMergedFunctionCalls funcs normalizedGVars normalizedAsts
    pure (revalidatedAsts, normalizedGVars)

prepareVisualizableInput
    :: (Integral i, Bits i, Read i, Show i, Ord i)
    => PF.Functions i
    -> ASTs i
    -> GlobalVars i
    -> Either String (ASTs i, GlobalVars i)
prepareVisualizableInput funcs asts gvars = do
    let materializedGVars = M.map materializeTentativeIncompleteArray gvars
        retypedAsts = retypeResolvedGlobalRefs materializedGVars asts
    revalidatedAsts <-
        revalidateMergedFunctionCallsWithMode
            VisualizableMergedRevalidation
            funcs
            materializedGVars
            retypedAsts
    pure (revalidatedAsts, materializedGVars)

casmNormalized' :: (Show e, Show i, Integral e, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i)
    => ASTs i
    -> GlobalVars i
    -> Literals i
    -> SI.Asm SI.AsmCodeCtx e ()
casmNormalized' atl gvars lits =
    dataSection materializedGVars lits >> textSection (retypeResolvedGlobalRefs materializedGVars atl)
    where
        materializedGVars = M.map materializeTentativeIncompleteArray gvars

-- | Executor that receives information about the constructed AST,
-- global variables, and literals and composes assembly code
casm' :: (Bits i, Read i, Show e, Show i, Integral e, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i)
    => ASTs i
    -> GlobalVars i
    -> Literals i
    -> PF.Functions i
    -> SI.Asm SI.AsmCodeCtx e ()
casm' atl gvars lits funcs =
    case prepareAsmInput funcs atl gvars of
        Left err -> SI.errCtx $ T.pack err
        Right (normalizedAsts, normalizedGVars) ->
            casmNormalized' normalizedAsts normalizedGVars lits
