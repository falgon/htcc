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
  , foldGlobalInitWith
) where

import           Control.Monad                               (unless, void,
                                                              when, zipWithM,
                                                              (>=>))
import           Control.Monad.Combinators                   (choice, some)
import           Control.Monad.Extra                         (ifM)
import           Control.Monad.State                         (get, gets, modify)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Data.Bits                                   (Bits, bit,
                                                              complement,
                                                              shiftL, shiftR,
                                                              xor, (.&.), (.|.))
import           Data.Char                                   (ord)
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
                                                              atLabel, atNoLeaf,
                                                              atNull, atNumLit,
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
import           Htcc.Parser.Combinators.Type                (toNamedParams)
import           Htcc.Parser.Combinators.Utils               (bracket,
                                                              captureFunctionParamScopes,
                                                              conditionalResultType,
                                                              decayExprType,
                                                              getPosState,
                                                              isInvalidFunctionPointerInitializer,
                                                              isInvalidFunctionPointerValue,
                                                              isInvalidObjectPointerValue,
                                                              maybeToParser,
                                                              registerFunc,
                                                              registerGVar,
                                                              registerGVarWith,
                                                              registerLVar,
                                                              registerStringLiteral,
                                                              registerTypedef)
import           Htcc.Parser.Combinators.Var                 (varInit)
import           Htcc.Parser.ConstructionData.Core           (ConstructionData (scope),
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
                                                              Scoped (curNestDepth, curScopeId, enumerators, structs))
import qualified Htcc.Parser.ConstructionData.Scope.Function as PSF
import qualified Htcc.Parser.ConstructionData.Scope.Var      as PV
import           Numeric.Natural                             (Natural)
import qualified Text.Megaparsec                             as M
import qualified Text.Megaparsec.Char                        as MC

import           Text.Megaparsec.Debug                       (dbg)

parser, program :: (Ord i, Integral i, Bits i, Read i, Show i) => Parser i (ASTs i)
parser = spaceConsumer *> program <* M.eof
program = some global

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

requireInitializedObjectType
    :: (Ord i, Bits i, Read i, Show i, Integral i)
    => String
    -> CT.StorageClass i
    -> Parser i (CT.StorageClass i)
requireInitializedObjectType err ty
    | isTopLevelOmittedBoundArrayType ty = pure ty
    | otherwise = requireCompleteObjectType err ty

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
applyCallArgConversions paramTys args =
    either fail pure $ convertCallArgs paramTys args

convertCallArgs :: (Ord i, Bits i, Integral i) => Maybe [CT.StorageClass i] -> [ATree i] -> Either String [ATree i]
convertCallArgs Nothing args = Right $ map defaultPromotedCallArg args
convertCallArgs (Just paramTys) args
    | actualArgCount < expectedArgCount = Left "too few arguments to function call"
    | actualArgCount > expectedArgCount = Left "too many arguments to function call"
    | otherwise = zipWithM convertTypedCallArg paramTys args
    where
        actualArgCount = length args
        expectedArgCount = length paramTys

        convertTypedCallArg paramTy arg
            | isInvalidFunctionPointerValue paramTy arg
                || isInvalidObjectPointerValue paramTy arg =
                Left "invalid argument type to function call"
            | otherwise =
                Right $ atCast paramTy arg

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

requireNonFunctionOperand
    :: String
    -> ATree i
    -> Parser i (ATree i)
requireNonFunctionOperand opName expr
    | isFunctionType (atype expr) = fail $ "invalid application of '" <> opName <> "' to function type"
    | otherwise = pure expr

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
        not $ isTopLevelOmittedBoundArrayType ty
    ATNode (ATGVar _ _) ty _ _ ->
        not $ isTopLevelOmittedBoundArrayType ty
    ATNode (ATMemberAcc _) ty _ _ ->
        not $ isTopLevelOmittedBoundArrayType ty
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
isModifiableLvalueExpr (ATNode kind ty _ _)
    | CT.isCTArray ty = False
    | isFunctionType ty = False
    | otherwise = case kind of
        ATLVar _ _    -> True
        ATGVar _ _    -> True
        ATMemberAcc _ -> True
        ATDeref       -> True
        _             -> False
isModifiableLvalueExpr _ = False

requireModifiableLvalue
    :: String
    -> ATree i
    -> Parser i (ATree i)
requireModifiableLvalue err expr
    | isModifiableLvalueExpr expr = pure expr
    | otherwise = fail err

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
                    *> gvarDecl declStorage ty' ident

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
                        *> (mapM registerNamedParam =<< toNamedParams fnTy)
                    where
                        registerNamedParam (paramTy, ident) = do
                            resolvedParamTy <-
                                requireCompleteObjectType
                                    "declaration of variable with incomplete type"
                                    paramTy
                            registerLVar resolvedParamTy ident

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

                fromValidFunc fnTy params' st@(ATNode (ATBlock block) _ _ _)
                    | CT.toTypeKind fnTy == CT.CTVoid =
                        if isJust (find isNonEmptyReturn block) then
                            fail $ mconcat
                                [ "the return type of function '"
                                , T.unpack ident
                                , "' is void, but the statement returns a value"
                                ]
                        else
                            pure $ atDefFunc ident (if null params' then Nothing else Just params') fnTy st
                    | otherwise = do
                        when (isJust (find isEmptyReturn block)) $
                            pushWarn pos $ mconcat
                                [ "the return type of function '"
                                , T.unpack ident
                                , "' is "
                                , show (CT.toTypeKind fnTy)
                                    , ", but the statement returns no value"
                                    ]
                        pure $ atDefFunc ident (if null params' then Nothing else Just params') fnTy st
                fromValidFunc _ _ _ = fail "internal compiler error"

        gvarDecl declStorage ty ident = choice
            [ nonInit declStorage ty ident
            , withInit ty ident
            ]
        nonInit declStorage ty ident
            | declStorage == ExternDecl =
                semi
                    *> (requireExternDeclObjectType "declaration of variable with incomplete type" ty
                            >>= \resolvedTy -> registerGVarWith resolvedTy ident PV.GVarInitWithExternDecl
                       )
                    $> ATEmpty
            | CT.isIncompleteArray ty && isValidTentativeFileScopeArrayType ty =
                semi *> registerGVar ty ident $> ATEmpty
            | CT.isIncompleteArray ty =
                fail "defining global variables with a incomplete type"
            | otherwise =
                semi
                    >> requireCompleteObjectType "defining global variables with a incomplete type" ty
                    >>= flip registerGVar ident
                    >> pure ATEmpty

        withInit ty ident = do
            void $ requireInitializedObjectType "defining global variables with a incomplete type" ty
            void equal
            (ty', initWith) <- parseGlobalVarInit ty ident
            registerGVarWith ty' ident initWith <* semi

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
            void $ registerGVar declaredTy name

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
            | CT.isCTIncomplete (atype expr) =
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
    , atExprStmt <$> (expr <* semi)
    , ATEmpty <$ semi
    ]
    where
        returnStmt = choice
            [ atReturn (CT.SCUndef CT.CTUndef) ATEmpty <$ M.try (kReturn *> semi)
            , atReturn (CT.SCUndef CT.CTUndef) <$> (M.try kReturn *> expr) <* semi
            ]

        ifStmt = do
            r <- atIf <$> (M.try kIf >> parens expr) <*> stmt
            M.option ATEmpty (M.try kElse >> stmt) <&> \case
                ATEmpty -> r
                nd -> atElse r nd

        whileStmt = atWhile <$> (M.try kWhile >> parens expr) <*> stmt

        forStmt = (>>) (M.try kFor) $ bracket get (modify . fallBack) $ const $ do
            es <- parens $ do
                modify succNest
                initSect <- ATForInit
                    <$> choice [ATEmpty <$ semi, M.try (atExprStmt <$> expr <* semi), lvarStmt]
                condSect <- ATForCond
                    <$> choice [ATEmpty <$ semi, expr <* semi]
                incrSect <- ATForIncr
                    <$> M.option ATEmpty (atExprStmt <$> expr)
                pure [initSect, condSect, incrSect]
            atFor (es <> [ATForStmt ATEmpty]) <$ semi
                M.<|> atFor . (es <>) . (:[]) . ATForStmt <$> stmt

        breakStmt = atBreak <$ (M.try kBreak *> semi)

        continueStmt = atContinue <$ (M.try kContinue *> semi)

        switchStmt = do
            cond <- M.try kSwitch *> parens expr
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
                                requireNonVoidObjectType "variable declared void" ty'
                                    *> M.choice
                                        [ nonInit ty' ident
                                        , withInit ty' ident
                                        ]

                nonInit ty ident =
                    requireCompleteObjectType "declaration of variable with incomplete type" ty
                        >>= \resolvedTy ->
                            semi *> registerLVar resolvedTy ident <&> atNull
                withInit ty ident = do
                    resolvedTy <-
                        requireInitializedObjectType "declaration of variable with incomplete type" ty
                    equal *> varInit assign resolvedTy ident <* semi
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
                                        registerGVarWith resolvedTy ident PV.GVarInitWithExternDecl
                               )
                            $> ATEmpty
                        ]

expr = assign >>= go
    where
        go lhs = M.option lhs $ do
            void comma
            rhs <- assign
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
            | otherwise =
                pure ()

conditional = do
    nd <- logicalOr
    ifM (M.option False (True <$ M.lookAhead question)) (GNU.condOmitted nd M.<|> condOp nd) $ pure nd
    where
        condOp nd = do
            th <- question *> expr <* colon
            el <- conditional
            ty <- maybeToParser "invalid operands" $ conditionalResultType th el
            pure $ atConditional ty nd th el

logicalOr = binaryOperator logicalAnd [(symbol "||", binOpBool ATLOr)]

logicalAnd = binaryOperator bitwiseOr [(symbol "&&", binOpBool ATLAnd)]

bitwiseOr = binaryOperator bitwiseXor [(vertical, binOpIntOnly ATOr)]

bitwiseXor = binaryOperator bitwiseAnd [(hat, binOpIntOnly ATXor)]

bitwiseAnd = binaryOperator equality [(MC.char '&' `notFollowedOp` MC.char '&', binOpIntOnly ATAnd)]

equality = binaryOperator relational
    [ (symbol "==", binOpBool ATEQ)
    , (symbol "!=", binOpBool ATNEQ)
    ]

relational = binaryOperator shift
    [ (symbol "<=", binOpBool ATLEQ)
    , (langle, binOpBool ATLT)
    , (symbol ">=", binOpBool ATGEQ)
    , (rangle, binOpBool ATGT)
    ]

shift = binaryOperator add
    [ (symbol "<<", binOpIntOnly ATShl)
    , (symbol ">>", binOpIntOnly ATShr)
    ]

add = binaryOperator term
    [ (symbol "+", pointerAwareBinaryOp addKind)
    , (symbol "-", pointerAwareBinaryOp subKind)
    ]
    where
        pointerAwareBinaryOp mk l r = do
            node <- maybeToParser "invalid operands" $ mk (decayIncompleteArrayExpr l) (decayIncompleteArrayExpr r)
            requireCompletePointerArithmeticNode node
            pure node

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
    [ (star, binOpCon ATMul)
    , (slash, binOpCon ATDiv)
    , (percent, binOpCon ATMod)
    ]

cast = choice
    [ atCast <$> M.try (parens absDeclarator) <*> cast
    , unary
    ]

unary = choice
    [ symbol "++" *> unary >>= requireModifiableLvalue "lvalue required as increment operand" <&> \n -> ATNode ATPreInc (atype n) n ATEmpty
    , symbol "--" *> unary >>= requireModifiableLvalue "lvalue required as decrement operand" <&> \n -> ATNode ATPreDec (atype n) n ATEmpty
    , symbol "+" *> unary >>= requireNonFunctionOperand "+" <&> integerPromotedExpr
    , symbol "-" *> unary >>= requireNonFunctionOperand "-" <&> \n ->
        let promoted = integerPromotedExpr n
         in ATNode ATSub (atype promoted) (atNumLit 0) promoted
    , lnot *> unary <&> flip (ATNode ATNot (CT.SCAuto CT.CTBool)) ATEmpty
    , tilda *> unary >>= requireNonFunctionOperand "~" <&> \n ->
        let promoted = integerPromotedExpr n
         in ATNode ATBitNot (atype promoted) promoted ATEmpty
    , addr
    , star *> unary >>= deref'
    , factor'
    ]
    where
        addr = MC.char '&' `notFollowedOp` MC.char '&' >> unary <&> \n ->
            atUnary ATAddr (CT.mapTypeKind CT.CTPtr $ atype n) n

        factor' = factor >>= allAcc
            where
                allAcc fac = M.option fac $ choice
                    [ callAcc fac
                    , idxAcc fac
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

                postInc fac = do
                    _ <- symbol "++"
                    fac' <- requireModifiableLvalue "lvalue required as increment operand" fac
                    allAcc $ atUnary ATPostInc (atype fac') fac'

                postDec fac = do
                    _ <- symbol "--"
                    fac' <- requireModifiableLvalue "lvalue required as decrement operand" fac
                    allAcc $ atUnary ATPostDec (atype fac') fac'

        deref' = runMaybeT . deref'' >=> maybe M.empty pure
            where
                deref'' n
                    | isFunctionType (atype n) =
                    pure n
                deref'' n = do
                    ty <- MaybeT $ pure (derefObjectType $ atype n)
                    case CT.toTypeKind ty of
                        CT.CTVoid -> lift $ fail "void value not ignored as it ought to be"
                        _ -> lift (resolveDerefObjectType "incomplete value dereference" ty)
                            >>= lift . pure . flip (atUnary ATDeref) n

factor = choice
    [ atNumLit <$> natural
    , atNumLit . fromIntegral . ord <$> charLiteral
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
                    u <- unary >>= requireNonFunctionOperand opS
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
                            let params = map defaultPromotedCallArg rawParams
                                params' = if null params then Nothing else Just params
                                implicitFnTy = CT.SCAuto $ CT.CTFunc CT.CTInt []
                             in do
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
