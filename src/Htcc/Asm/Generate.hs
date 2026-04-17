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

import           Control.Monad                               (when)
import           Control.Applicative                         ((<|>))
import           Data.Bits                                   (Bits)
import           Data.Foldable                               (traverse_)
import qualified Data.Map.Strict                             as M
import           Data.Maybe                                  (fromMaybe)
import qualified Data.Text                                   as T
import           Htcc.Asm.Generate.Core
import           Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Structure                as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text   as IT
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser                                 (ASTs, ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..))
import           Htcc.Parser.Combinators.Program             (convertCallArgs,
                                                              foldGlobalInitWith)
import           Htcc.Parser.Combinators.Utils               (conditionalResultType,
                                                              decayExprType,
                                                              isInvalidFunctionPointerValue,
                                                              isInvalidObjectPointerValue)
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import           Htcc.Parser.ConstructionData.Scope.Var      (GVar (..),
                                                              GVarInitWith (..),
                                                              GlobalVars,
                                                              Literals,
                                                              materializeTentativeIncompleteArray)

-- | input string, C source code
type InputCCode = T.Text

normalizeGlobalInitializers :: (Integral i, Bits i, Read i, Show i, Ord i) => GlobalVars i -> Either String (GlobalVars i)
normalizeGlobalInitializers gvars = M.traverseWithKey resolveGlobalInit gvars
    where
        resolveGlobalInit _ gvar = case initWith gvar of
            GVarInitWithAST ast ->
                (\ginit -> gvar { initWith = ginit })
                    <$> foldGlobalInitWith (gvtype gvar) ast
            _ ->
                Right gvar

mergedGlobalType :: Eq i => Maybe (GlobalVars i) -> T.Text -> CT.StorageClass i -> CT.StorageClass i
mergedGlobalType maybeGVars name currentTy =
    case maybeGVars >>= M.lookup name of
        Just gvar ->
            let declaredTy = gvtype gvar
             in fromMaybe declaredTy $
                    (\mergedTy -> CT.mapTypeKind (const mergedTy) declaredTy)
                        <$> ( CT.mergeCompatibleTypeKinds
                                (CT.toTypeKind declaredTy)
                                (CT.toTypeKind currentTy)
                            <|> CT.mergeCompatibleTypeKinds
                                (CT.toTypeKind currentTy)
                                (CT.toTypeKind declaredTy)
                            )
        Nothing ->
            currentTy

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

mergedFunctionType :: PF.Functions i -> T.Text -> CT.StorageClass i -> CT.StorageClass i
mergedFunctionType funcs name fallback =
    maybe fallback PF.fntype $ M.lookup name funcs

functionReturnType :: CT.StorageClass i -> Maybe (CT.StorageClass i)
functionReturnType ty = case CT.toTypeKind ty of
    CT.CTFunc retTy _ -> Just $ CT.SCAuto retTy
    _                 -> Nothing

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
            || ( objectPointerRetyped originalLhs originalRhs lhs rhs
                    && isInvalidObjectPointerValue (atype lhs) rhs
               )
    _ ->
        False

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
        hasIncompletePointerTarget expr = case CT.deref (atype expr) of
            Just ty -> CT.isCTIncomplete ty
            Nothing -> False

invalidIncompleteMemOp :: Ord i => ATKind i -> ATree i -> Bool
invalidIncompleteMemOp kind lhs = case kind of
    ATSizeof ->
        CT.isCTIncomplete $ atype lhs
    ATAlignof ->
        CT.isCTIncomplete $ atype lhs
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
                       )
            )
            currentReturnTy
    _ ->
        False

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
                        atype lhs'
                    ATSubPtr ->
                        atype lhs'
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
revalidateMergedFunctionTree mode funcs maybeGVars = revalidateTree M.empty Nothing
    where
        validateDeferredCodegenChecks = case mode of
            StrictMergedRevalidation       -> True
            VisualizableMergedRevalidation -> False

        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

        revalidateKindFor paramTys currentReturnTy = \case
            ATForkw ->
                Right ATForkw
            ATForInit at ->
                ATForInit <$> revalidateTree paramTys currentReturnTy at
            ATForCond at ->
                ATForCond <$> revalidateTree paramTys currentReturnTy at
            ATForIncr at ->
                ATForIncr <$> revalidateTree paramTys currentReturnTy at
            ATForStmt at ->
                ATForStmt <$> revalidateTree paramTys currentReturnTy at

        revalidateKind paramTys currentReturnTy = \case
            ATConditional cond tr fl ->
                ATConditional
                    <$> revalidateTree paramTys currentReturnTy cond
                    <*> revalidateTree paramTys currentReturnTy tr
                    <*> revalidateTree paramTys currentReturnTy fl
            ATSwitch cond cases ->
                ATSwitch
                    <$> revalidateTree paramTys currentReturnTy cond
                    <*> traverse (revalidateTree paramTys currentReturnTy) cases
            ATFor kinds ->
                ATFor <$> traverse (revalidateKindFor paramTys currentReturnTy) kinds
            ATBlock ats ->
                ATBlock <$> traverse (revalidateTree paramTys currentReturnTy) ats
            ATStmtExpr ats ->
                ATStmtExpr <$> traverse (revalidateTree paramTys currentReturnTy) ats
            ATNull at ->
                ATNull <$> revalidateTree paramTys currentReturnTy at
            other ->
                Right other

        revalidateTree _ _ ATEmpty = Right ATEmpty
        revalidateTree currentParamTys currentReturnTy (ATNode (ATDefFunc name args) ty lhs rhs) = do
            let resolvedFnTy = mergedFunctionType funcs name ty
                nestedReturnTy = functionReturnType resolvedFnTy
                nestedParamTys = mergedFunctionParamBindings resolvedFnTy args
            args' <- traverse (traverse $ revalidateTree nestedParamTys nestedReturnTy) args
            lhs' <- revalidateTree nestedParamTys nestedReturnTy lhs
            rhs' <- revalidateTree currentParamTys currentReturnTy rhs
            pure $ ATNode (ATDefFunc name args') resolvedFnTy lhs' rhs'
        revalidateTree currentParamTys currentReturnTy (ATNode (ATLVar ty offset) _ lhs rhs) = do
            lhs' <- revalidateTree currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree currentParamTys currentReturnTy rhs
            let resolvedTy = fromMaybe ty $ M.lookup offset currentParamTys
            pure $ ATNode (ATLVar resolvedTy offset) resolvedTy lhs' rhs'
        revalidateTree currentParamTys currentReturnTy (ATNode (ATCallFunc name args) ty lhs rhs) = do
            lhs' <- revalidateTree currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree currentParamTys currentReturnTy rhs
            args' <- traverse (traverse $ revalidateTree currentParamTys currentReturnTy) args
            case M.lookup name funcs of
                Nothing ->
                    pure $ ATNode (ATCallFunc name args') ty lhs' rhs'
                Just fn -> do
                    (callTy, formalParamTys) <- maybe
                        (Left "internal compiler error: merged function lookup returned non-callable type")
                        Right
                        (mergedCallableSignature $ PF.fntype fn)
                    params <- convertCallArgs formalParamTys $ fromMaybe [] args'
                    let params' = if null params then Nothing else Just params
                    pure $ ATNode (ATCallFunc name params') callTy lhs' rhs'
        revalidateTree currentParamTys currentReturnTy (ATNode (ATCallPtr args) _ lhs rhs) = do
            lhs' <- revalidateTree currentParamTys currentReturnTy lhs
            rhs' <- revalidateTree currentParamTys currentReturnTy rhs
            args' <- traverse (traverse $ revalidateTree currentParamTys currentReturnTy) args
            (callTy, formalParamTys) <- maybe
                (Left "called object is not a function or function pointer")
                Right
                (mergedCallableSignature $ atype lhs')
            params <- convertCallArgs formalParamTys $ fromMaybe [] args'
            let params' = if null params then Nothing else Just params
            pure $ ATNode (ATCallPtr params') callTy lhs' rhs'
        revalidateTree currentParamTys currentReturnTy (ATNode kind ty lhs rhs) = do
            kind' <- revalidateKind currentParamTys currentReturnTy kind
            let nestedReturnTy = case kind' of
                    ATDefFunc name _ ->
                        functionReturnType $ mergedFunctionType funcs name ty
                    _ ->
                        currentReturnTy
            lhs' <- revalidateTree currentParamTys nestedReturnTy lhs
            rhs' <- revalidateTree currentParamTys currentReturnTy rhs
            let (kind'', ty') = refreshKindAndType kind' ty lhs' rhs'
            when (validateDeferredCodegenChecks && invalidAssignmentOperands kind'' lhs rhs lhs' rhs') $
                Left "invalid operands to assignment"
            when (validateDeferredCodegenChecks && invalidIncompletePointerArithmetic kind'' lhs' rhs') $
                Left "invalid use of pointer to incomplete type"
            when (validateDeferredCodegenChecks && invalidIncompleteMemOp kind'' lhs') $
                Left $ case kind'' of
                    ATSizeof -> "invalid application of 'sizeof' to incomplete type"
                    ATAlignof -> "invalid application of '_Alignof' to incomplete type"
                    _ -> "internal compiler error: unexpected incomplete memory operator"
            when (validateDeferredCodegenChecks && invalidReturnValue currentReturnTy kind'' lhs') $
                Left "invalid return type"
            pure $ ATNode kind'' ty' lhs' rhs'

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
                (ATAddPtr, atype lhs)
            ATSubPtr ->
                (ATSubPtr, atype lhs)
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
                let ast' = refreshMergedValueTypes funcs (Just gvars) ast
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
