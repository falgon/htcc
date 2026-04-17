{-|
Module      : Htcc.Parser.ConstructionData.Scope
Description : The Data type of scope and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope (
    -- * The types
    Scoped (..),
    LookupVarResult (..),
    -- * Operations for scope
    addLVar,
    addGVar,
    addGVarAllowFunctionConflict,
    addGVarWith,
    addGVarWithAllowFunctionConflict,
    addLiteral,
    addTag,
    addTypedef,
    addFunction,
    addFunctionAllowGlobalConflict,
    addEnumerator,
    succNest,
    fallBack,
    lookupLVar,
    lookupGVar,
    lookupVar,
    lookupTag,
    lookupTypedef,
    lookupFunction,
    lookupEnumerator,
    initScope,
    resetLocal
) where

import           Control.DeepSeq                                 (NFData (..))
import           Data.Bits                                       (Bits (..))
import           Data.Maybe                                      (fromMaybe,
                                                                  isJust)
import qualified Data.Text                                       as T
import           Data.Tuple.Extra                                (second)
import           GHC.Generics                                    (Generic (..),
                                                                  Generic1 (..))
import           Numeric.Natural

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (ATree (..))
import qualified Htcc.Parser.ConstructionData.Scope.Enumerator   as SE
import qualified Htcc.Parser.ConstructionData.Scope.Function     as PF
import qualified Htcc.Parser.ConstructionData.Scope.ManagedScope as SM
import qualified Htcc.Parser.ConstructionData.Scope.Tag          as PS
import qualified Htcc.Parser.ConstructionData.Scope.Typedef      as PT
import qualified Htcc.Parser.ConstructionData.Scope.Var          as PV
import qualified Htcc.Tokenizer.Token                            as HT

-- | The data type of a struct tag
data Scoped i = Scoped -- ^ The constructor of a struct tag
    {
        curNestDepth      :: !Natural, -- ^ The nest depth of the parsing process
        curScopeId        :: !CT.ScopeId, -- ^ The unique identity of the current scope
        nextScopeId       :: !CT.ScopeId, -- ^ The next unique scope identity to allocate
        vars              :: PV.Vars i, -- ^ scoped all identifiers of variables (local variables, global variables and literals) visible during processing
        structs           :: PS.Tags i, -- ^ scoped all struct tags
        typedefs          :: PT.Typedefs i, -- ^ scoped all typedefs
        functions         :: PF.Functions i, -- ^ scoped all identifires of functions
        externalFunctions :: PF.Functions i, -- ^ translation-unit function declarations used for compatibility checks across scopes
        enumerators       :: SE.Enumerators i -- ^ scoped all identifiers of enumerators
    } deriving (Show, Generic, Generic1)

instance NFData i => NFData (Scoped i)

-- | A type that represents the result of a variable search
data LookupVarResult i = FoundGVar (PV.GVar i)  -- ^ A type constructor indicating that a global variable has been found
    | FoundLVar (PV.LVar i) -- ^ A type constructor indicating that a local variable has been found
    | FoundEnum (SE.Enumerator i) -- ^ A type constructor indicating that a enumerator has been found
    | FoundFunc (PF.Function i) -- ^ A type constructor indicating that a function has been found
    | FoundTypedef (PT.Typedef i) -- ^ A type constructor indicating that a typedef has been found in the ordinary identifier namespace
    | NotFound -- ^ A type constructor indicating that it was not found
    deriving (Show, Eq)

{-# INLINE applyVars #-}
applyVars :: Scoped i -> (a, PV.Vars i) -> (a, Scoped i)
applyVars sc = second (\x -> sc { vars = x })

{-# INLINE identifierFromToken #-}
identifierFromToken :: HT.TokenLC i -> Maybe T.Text
identifierFromToken (_, HT.TKIdent ident) = Just ident
identifierFromToken _                     = Nothing

{-# INLINE sameScopeLVar #-}
sameScopeLVar :: T.Text -> Natural -> Scoped i -> Bool
sameScopeLVar ident depth = maybe False ((== depth) . PV.nestDepth) . lookupLVar ident

{-# INLINE sameScopeTypedef #-}
sameScopeTypedef :: T.Text -> Natural -> Scoped i -> Bool
sameScopeTypedef ident depth = maybe False ((== depth) . PT.tdNestDepth) . lookupTypedef ident

{-# INLINE sameScopeEnumerator #-}
sameScopeEnumerator :: T.Text -> Natural -> Scoped i -> Bool
sameScopeEnumerator ident depth = maybe False ((== depth) . SE.enNestDepth) . lookupEnumerator ident

{-# INLINE sameScopeGVar #-}
sameScopeGVar :: T.Text -> Natural -> Scoped i -> Bool
sameScopeGVar ident depth = maybe False ((== depth) . PV.gvNestDepth) . lookupGVar ident

{-# INLINE sameScopeFunction #-}
sameScopeFunction :: T.Text -> Natural -> Scoped i -> Bool
sameScopeFunction ident depth = maybe False ((== depth) . PF.fnNestDepth) . lookupFunction ident

{-# INLINE rejectLocalOrdinaryNameConflict #-}
rejectLocalOrdinaryNameConflict :: HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) ()
rejectLocalOrdinaryNameConflict tkn sc = case identifierFromToken tkn of
    Just ident
        | any ($ sc)
            [ sameScopeGVar ident depth
            , sameScopeTypedef ident depth
            , sameScopeEnumerator ident depth
            , sameScopeFunction ident depth
            ] ->
            Left ("redeclaration of '" <> ident <> "' with no linkage", tkn)
    _ ->
        Right ()
    where
        depth = curNestDepth sc

{-# INLINE lookupExternalFunction #-}
lookupExternalFunction :: T.Text -> Scoped i -> Maybe (PF.Function i)
lookupExternalFunction ident = SM.lookup ident . externalFunctions

{-# INLINE lookupExternalGVar #-}
lookupExternalGVar :: T.Text -> Scoped i -> Maybe (PV.GVar i)
lookupExternalGVar ident = SM.lookup ident . PV.externalGlobals . vars

{-# INLINE rejectObjectNameConflict #-}
rejectObjectNameConflict :: HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) ()
rejectObjectNameConflict tkn sc = case identifierFromToken tkn of
    Just ident
        | depth == 0
            , isJust (lookupExternalFunction ident sc) ->
            Left ("redeclaration of '" <> ident <> "' with no linkage", tkn)
        | depth == 0
            && any ($ sc) [sameScopeTypedef ident 0, sameScopeEnumerator ident 0] ->
            Left ("conflicting types for '" <> ident <> "'", tkn)
        | depth /= 0
            && isJust (lookupFunction ident sc) ->
            Left ("redeclaration of '" <> ident <> "' with no linkage", tkn)
        | depth /= 0
            && any ($ sc)
                [ sameScopeLVar ident depth
                , sameScopeTypedef ident depth
                , sameScopeEnumerator ident depth
                , sameScopeFunction ident depth
                ] ->
            Left ("redeclaration of '" <> ident <> "' with no linkage", tkn)
    _ ->
        Right ()
    where
        depth = curNestDepth sc

{-# INLINE addVar #-}
addVar :: (Integral i, Bits i) => (CT.StorageClass i -> HT.TokenLC i -> PV.Vars i -> Either (T.Text, HT.TokenLC i) (ATree i, PV.Vars i)) -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addVar f ty tkn sc = applyVars sc <$> f ty tkn (vars sc)

-- | `addLVar` has a scoped type argument and is the same function as `PV.addLVar` internally.
{-# INLINE addLVar #-}
addLVar :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addLVar ty tkn scp =
    rejectLocalOrdinaryNameConflict tkn scp *> addVar (PV.addLVar $ curNestDepth scp) ty tkn scp

-- | `addGVar` has a scoped type argument and is the same function as `PV.addGVar` internally.
{-# INLINE addGVar #-}
addGVar :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addGVar ty tkn sc = rejectObjectNameConflict tkn sc *> addVar (PV.addGVar $ curNestDepth sc) ty tkn sc

{-# INLINE addGVarAllowFunctionConflict #-}
addGVarAllowFunctionConflict :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addGVarAllowFunctionConflict ty tkn sc =
    rejectNonFunctionObjectNameConflict tkn sc *> addVar (PV.addGVar $ curNestDepth sc) ty tkn sc
    where
        rejectNonFunctionObjectNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && any ($ sc') [sameScopeTypedef ident 0, sameScopeEnumerator ident 0] ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && isJust (lookupFunction ident sc') ->
                    Left ("redeclaration of '" <> ident <> "' with no linkage", tkn')
                | depth /= 0
                    && any ($ sc')
                        [ sameScopeLVar ident depth
                        , sameScopeTypedef ident depth
                        , sameScopeEnumerator ident depth
                        , sameScopeFunction ident depth
                        ] ->
                    Left ("redeclaration of '" <> ident <> "' with no linkage", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

-- | `addGVarWith` has a scoped type argument and is the same function as `PV.addLiteral` internally.
{-# INLINE addGVarWith #-}
addGVarWith :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> PV.GVarInitWith i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addGVarWith ty tkn iw sc =
    rejectObjectNameConflict tkn sc *> (applyVars sc <$> PV.addGVarWith (curNestDepth sc) ty tkn iw (vars sc))

{-# INLINE addGVarWithAllowFunctionConflict #-}
addGVarWithAllowFunctionConflict :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> PV.GVarInitWith i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addGVarWithAllowFunctionConflict ty tkn iw sc =
    rejectNonFunctionObjectNameConflict tkn sc *> (applyVars sc <$> PV.addGVarWith (curNestDepth sc) ty tkn iw (vars sc))
    where
        rejectNonFunctionObjectNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && any ($ sc') [sameScopeTypedef ident 0, sameScopeEnumerator ident 0] ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && isJust (lookupFunction ident sc') ->
                    Left ("redeclaration of '" <> ident <> "' with no linkage", tkn')
                | depth /= 0
                    && any ($ sc')
                        [ sameScopeLVar ident depth
                        , sameScopeTypedef ident depth
                        , sameScopeEnumerator ident depth
                        , sameScopeFunction ident depth
                        ] ->
                    Left ("redeclaration of '" <> ident <> "' with no linkage", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

-- | `addLiteral` has a scoped type argument and is the same function as `PV.addLiteral` internally.
{-# INLINE addLiteral #-}
addLiteral :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addLiteral = addVar PV.addLiteral

-- | `succNest` has a scoped type argument and is the same function as `PV.succNest` internally.
{-# INLINE succNest #-}
succNest :: Scoped i -> Scoped i
succNest sc = sc
    { curNestDepth = succ $ curNestDepth sc
    , curScopeId = nextScopeId sc
    , nextScopeId = succScopeId $ nextScopeId sc
    }
    where
        succScopeId (CT.ScopeId scopeId) = CT.ScopeId $ succ scopeId

-- | `fallBack` has a scoped type argument and is the same function as `PV.fallBack` internally.
{-# INLINE fallBack #-}
fallBack :: Scoped i -> Scoped i -> Scoped i
fallBack pre post = pre
    {
        vars = PV.fallBack (vars pre) (vars post),
        structs = PS.fallBackTags (structs pre) (structs post),
        typedefs = SM.fallBack (typedefs pre) (typedefs post),
        functions = SM.fallBack (functions pre) (functions post),
        externalFunctions = externalFunctions post,
        enumerators = SM.fallBack (enumerators pre) (enumerators post),
        nextScopeId = nextScopeId post
    }

{-# INLINE lookupVar' #-}
lookupVar' :: (T.Text -> PV.Vars a -> b) -> T.Text -> Scoped a -> b
lookupVar' f s sc = f s $ vars sc

-- | `lookupLVar` has a scoped type argument and is the same function as `PV.lookupLVar` internally.
{-# INLINE lookupLVar #-}
lookupLVar :: T.Text -> Scoped i -> Maybe (PV.LVar i)
lookupLVar = lookupVar' PV.lookupLVar

-- | `lookupGVar` has a scoped type argument and is the same function as `PV.lookupGVar` internally.
{-# INLINE lookupGVar #-}
lookupGVar :: T.Text -> Scoped i -> Maybe (PV.GVar i)
lookupGVar = lookupVar' PV.lookupGVar

-- | `lookupVar` has a scoped type argument and is the same function as `PV.lookupVar` internally.
{-# INLINE lookupVar #-}
lookupVar :: T.Text -> Scoped i -> LookupVarResult i
lookupVar ident scp =
    maybe NotFound snd $
        selectDeepest
            [ (\local -> (PV.nestDepth local, FoundLVar local)) <$> lookupLVar ident scp
            , (\enum -> (SE.enNestDepth enum, FoundEnum enum)) <$> lookupEnumerator ident scp
            , (\gvar -> (PV.gvNestDepth gvar, FoundGVar gvar)) <$> lookupGVar ident scp
            , (\fn -> (PF.fnNestDepth fn, FoundFunc fn)) <$> lookupFunction ident scp
            , (\td -> (PT.tdNestDepth td, FoundTypedef td)) <$> lookupTypedef ident scp
            ]
    where
        selectDeepest =
            foldl
                (\best candidate -> case (best, candidate) of
                    (Nothing, x) -> x
                    (x, Nothing) -> x
                    (Just (bestDepth, _), Just (candidateDepth, _))
                        | candidateDepth > bestDepth -> candidate
                        | otherwise -> best
                )
                Nothing

-- | `lookupTag` has a scoped type argument and is the same function as `PS.lookupTag` internally.
{-# INLINE lookupTag #-}
lookupTag :: T.Text -> Scoped i -> Maybe (PS.Tag i)
lookupTag t sc = PS.lookupVisible t $ structs sc

-- | `lookupTypedef` has a scoped type argument and is the same function as `PT.lookupTypedef` internally.
{-# INLINE lookupTypedef #-}
lookupTypedef :: T.Text -> Scoped i -> Maybe (PT.Typedef i)
lookupTypedef t sc = SM.lookup t $ typedefs sc

-- | `lookupFunction` has a scoped type argument and is the same function as `PF.lookupFunction` internally.
{-# INLINE lookupFunction #-}
lookupFunction :: T.Text -> Scoped i -> Maybe (PF.Function i)
lookupFunction t sc = SM.lookup t $ functions sc

{-# INLINE lookupEnumerator #-}
-- | `lookupEnumerator` has a scoped type argument and is the same function as `PF.lookupFunction` internally.
lookupEnumerator :: T.Text -> Scoped i -> Maybe (SE.Enumerator i)
lookupEnumerator t sc = SM.lookup t $ enumerators sc

-- | `addTag` has a scoped type argument and is the same function as `PS.add` internally.
{-# INLINE addTag #-}
addTag :: Num i => PS.TagKind -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addTag kind ty tkn sc =
    (\x -> sc { structs = x })
        <$> PS.add (curNestDepth sc) (curScopeId sc) kind ty tkn (structs sc)

-- | `addTypedef` has a scoped type argument and is the same function as `PT.add` internally.
{-# INLINE addTypedef #-}
addTypedef :: (Eq i, Num i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addTypedef ty tkn sc =
    rejectTypedefNameConflict tkn sc *> ((\x -> sc { typedefs = x }) <$> PT.add (curNestDepth sc) ty tkn (typedefs sc))
    where
        rejectTypedefNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && (isJust (lookupGVar ident sc') || isJust (lookupFunction ident sc') || sameScopeEnumerator ident 0 sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && (sameScopeLVar ident depth sc' || sameScopeGVar ident depth sc' || sameScopeEnumerator ident depth sc' || sameScopeFunction ident depth sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

-- | `addFunction` has a scoped type argument and is the same function as `PT.add` internally.
{-# INLINE addFunction #-}
addFunction :: (Eq i, Num i) => Bool -> Bool -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addFunction fd isImplicit ty tkn sc = do
    rejectFunctionDeclNameConflict tkn sc
    visibleFunctions <- PF.add (curNestDepth sc) fd isImplicit ty tkn (functions sc)
    declaredFunctions <- PF.add 0 fd isImplicit ty tkn (externalFunctions sc)
    pure sc { functions = visibleFunctions, externalFunctions = declaredFunctions }
    where
        rejectFunctionDeclNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && (isJust (lookupExternalGVar ident sc') || sameScopeTypedef ident 0 sc' || sameScopeEnumerator ident 0 sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && isJust (lookupGVar ident sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && (sameScopeLVar ident depth sc' || sameScopeGVar ident depth sc' || sameScopeTypedef ident depth sc' || sameScopeEnumerator ident depth sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

{-# INLINE addFunctionAllowGlobalConflict #-}
addFunctionAllowGlobalConflict :: (Eq i, Num i) => Bool -> Bool -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addFunctionAllowGlobalConflict fd isImplicit ty tkn sc = do
    rejectFunctionDeclNameConflict tkn sc
    visibleFunctions <- PF.add (curNestDepth sc) fd isImplicit ty tkn (functions sc)
    declaredFunctions <- PF.add 0 fd isImplicit ty tkn (externalFunctions sc)
    pure sc { functions = visibleFunctions, externalFunctions = declaredFunctions }
    where
        rejectFunctionDeclNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && (sameScopeTypedef ident 0 sc' || sameScopeEnumerator ident 0 sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && isJust (lookupGVar ident sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
                | depth /= 0
                    && (sameScopeLVar ident depth sc' || sameScopeGVar ident depth sc' || sameScopeTypedef ident depth sc' || sameScopeEnumerator ident depth sc') ->
                    Left ("conflicting types for '" <> ident <> "'", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

-- | `addEnumerator` has a scoped type argument and is the same function as `SE.add` internally.
{-# INLINE addEnumerator #-}
addEnumerator :: Num i => CT.StorageClass i -> HT.TokenLC i -> i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addEnumerator ty tkn val sc =
    rejectEnumeratorNameConflict tkn sc *> ((\x -> sc { enumerators = x }) <$> SE.add (curNestDepth sc) ty tkn val (enumerators sc))
    where
        rejectEnumeratorNameConflict tkn' sc' = case identifierFromToken tkn' of
            Just ident
                | depth == 0
                    && (isJust (lookupGVar ident sc') || isJust (lookupFunction ident sc') || sameScopeTypedef ident 0 sc') ->
                    Left ("redeclaration of enumerator '" <> ident <> "'", tkn')
                | depth /= 0
                    && (sameScopeLVar ident depth sc' || sameScopeGVar ident depth sc' || sameScopeTypedef ident depth sc' || sameScopeFunction ident depth sc') ->
                    Left ("redeclaration of enumerator '" <> ident <> "'", tkn')
            _ ->
                Right ()
            where
                depth = curNestDepth sc'

{-# INLINE initScope #-}
-- | Helper function representing an empty scoped data
initScope :: Scoped i
initScope = Scoped 0 (CT.ScopeId 0) (CT.ScopeId 1) PV.initVars SM.initial SM.initial SM.initial SM.initial SM.initial

{-# INLINE resetLocal #-}
-- | `resetLocal` has a scoped type argument and is the same function as `PV.resetLocal` internally.
resetLocal :: Scoped i -> Scoped i
resetLocal sc = sc { vars = PV.resetLocal (vars sc) }
