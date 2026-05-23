{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-|
Module      : Htcc.Parser.ConstructionData.Scope.Var
Description : The Data type of variables and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
module Htcc.Parser.ConstructionData.Scope.Var (
    -- * The classes
    Var (..),
    -- * The data type
    SomeVars,
    GVarInitData (..),
    GVarInitWith (..),
    GVar (..),
    LVar (..),
    Literal (..),
    GlobalVars,
    LocalVars,
    Literals,
    Vars (..),
    -- * Functions for adding and searching for variables and literals
    lookupLVar,
    lookupGVar,
    lookupVar,
    addLVar,
    addGVarWith,
    addGVar,
    addLiteral,
    -- * Utilities
    materializeTentativeIncompleteArray,
    initVars,
    resetLocal,
    fallBack
) where

import           Control.DeepSeq                                 (NFData (..))
import           Data.Bits                                       (Bits (..))
import qualified Data.ByteString                                 as B
import qualified Data.Map.Strict                                 as M
import           Data.Maybe                                      (fromMaybe)
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic,
                                                                  Generic1)
import           Numeric.Natural

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (ATKind (..),
                                                                  ATree (..),
                                                                  Treealizable (..),
                                                                  atGVar,
                                                                  atLVar)
import qualified Htcc.Parser.ConstructionData.Scope.ManagedScope as SM
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Tokenizer.Token                            as HT
import           Htcc.Utils                                      (tshow)

-- | The type class of the type representing the variable
class Var a where
    -- | `vtype` returns the type of the variable
    vtype :: a i -> CT.StorageClass i

-- | The informations type about initial value of the global variable
data GVarInitData i
    = GVarInitZeroBytes Natural
    | GVarInitBytes Natural i
    | GVarInitReloc Natural T.Text Integer
    deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (GVarInitData i)

-- | The informations type about initial value of the global variable
data GVarInitWith i
    = GVarInitWithZero
    | GVarInitWithExternDecl
    | GVarInitWithOG T.Text
    | GVarInitWithVal i
    | GVarInitWithData [GVarInitData i]
    | GVarInitWithAST (ATree i)
    deriving (Eq, Show, Generic)

instance NFData i => NFData (GVarInitWith i) where
    rnf GVarInitWithZero       = ()
    rnf GVarInitWithExternDecl = ()
    rnf (GVarInitWithOG ref)   = rnf ref
    rnf (GVarInitWithVal val)  = rnf val
    rnf (GVarInitWithData ds)  = rnf ds
    rnf (GVarInitWithAST ast)  = ast `seq` ()

-- | The data type of the global variable
data GVar i = GVar -- ^ The constructor of the global variable
    {
        gvtype      :: CT.StorageClass i, -- ^ The type of the global variable
        initWith    :: GVarInitWith i, -- ^ The informations about initial value of the global variable
        gvNestDepth :: !Natural -- ^ The nest depth where this declaration is currently visible.
    } deriving (Eq, Show, Generic)

instance NFData i => NFData (GVar i) where
    rnf (GVar ty iw depth) = rnf ty `seq` rnf iw `seq` rnf depth

materializeTentativeIncompleteArray :: Ord i => GVar i -> GVar i
materializeTentativeIncompleteArray gvar = case initWith gvar of
    GVarInitWithZero ->
        gvar { gvtype = CT.mapTypeKind materializeTentativeArrayType $ gvtype gvar }
    _ ->
        gvar
    where
        materializeTentativeArrayType = go
            where
                go (CT.CTArray n innerTy) = CT.CTArray n $ go innerTy
                go (CT.CTIncomplete (CT.IncompleteArray elemTy)) =
                    materializeIncompleteArray elemTy
                go ty = ty

                materializeIncompleteArray elemTy
                    | CT.isCTArray elemTy =
                        fromMaybe fallback $
                            CT.concatCTArray
                                (CT.makeCTArray [1] $ CT.removeAllExtents elemTy)
                                elemTy
                    | otherwise =
                        fallback
                    where
                        fallback = CT.CTArray 1 elemTy

instance Var GVar where
    vtype = gvtype

instance SM.ManagedScope (GVar i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The data type of local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        lvtype    :: CT.StorageClass a, -- ^ The type of the local variable
        rbpOffset :: !a, -- ^ The offset value from RBP
        nestDepth :: !Natural -- ^ The nest depth of a local variable
    } deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (LVar a)

instance Treealizable LVar where
    {-# INLINE treealize #-}
    treealize (LVar t o _) = atLVar t o

instance Var LVar where
    vtype = lvtype

instance SM.ManagedScope (LVar a) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The literal
data Literal a = Literal -- ^ The literal constructor
    {
        litype :: CT.StorageClass a, -- ^ The single literal type
        ln     :: !Natural, -- ^ The number of labels placed in the @.data@ section
        lcts   :: B.ByteString -- ^ The content
    } deriving (Eq, Show, Generic)

instance NFData a => NFData (Literal a)

instance Var Literal where
    vtype = litype

-- | The type of variables
type SomeVars v = M.Map T.Text v

-- | The type of global variables
type GlobalVars a = SomeVars (GVar a)

-- | The type of global variables
type LocalVars a = SomeVars (LVar a)

-- | The type of literals
type Literals a = [Literal a]

-- | The data type of local variables
data Vars a = Vars -- ^ The constructor of variables
    {
        globals         :: GlobalVars a, -- ^ The global variables visible in the current scope
        externalGlobals :: GlobalVars a, -- ^ Global declarations remembered for translation-unit compatibility checks
        locals          :: LocalVars a, -- ^ The local variables
        literals        :: Literals a -- ^ Literals
    } deriving (Show, Generic, Generic1)

instance NFData a => NFData (Vars a)

{-# INLINE initVars #-}
-- | Helper function representing an empty variables
initVars :: Vars a
initVars = Vars SM.initial SM.initial SM.initial []

{-# INLINE resetLocal #-}
-- | `resetLocal` initialize the local variable list for `Vars`
resetLocal :: Vars a -> Vars a
resetLocal vs = vs { locals = M.empty }

{-# INLINE lookupGVar #-}
-- | Search for a global variable with a given name
lookupGVar :: T.Text -> Vars a -> Maybe (GVar a)
lookupGVar s vars = SM.lookup s $ globals vars

{-# INLINE lookupLVar #-}
-- | Search for a local variable with a given name
lookupLVar :: T.Text -> Vars a -> Maybe (LVar a)
lookupLVar s vars = SM.lookup s $ locals vars

{-# INLINE lookupVar #-}
-- | First, search for local variables, and if not found, search for global variables. If nothing is found, Nothing is returned
lookupVar :: T.Text -> Vars a -> Maybe (Either (GVar a) (LVar a))
lookupVar s vars = maybe (Left <$> lookupGVar s vars) (Just . Right) $ lookupLVar s vars

{-# INLINE maximumOffset #-}
maximumOffset :: (Num a, Ord a) => M.Map T.Text (LVar a) -> a
maximumOffset m
    | M.null m = 0
    | otherwise = maximum $ map rbpOffset $ M.elems m

{-# INLINE fallBack #-}
-- | Organize variable list state after scoping
fallBack :: Vars a -> Vars a -> Vars a
fallBack pre post =
    pre
        { externalGlobals = externalGlobals post
        , literals = literals post
        }

-- | If the specified token is `HT.TKIdent` and the local variable does not exist in the list, `addLVar` adds a new local variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addLVar :: (Integral i, Bits i) => Natural -> CT.StorageClass i -> HT.TokenLC i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addLVar cnd t cur@(_, HT.TKIdent ident) vars = case lookupLVar ident vars of
    Just foundedVar
        | nestDepth foundedVar /= cnd -> varnat
        | CT.isIncompleteArray (lvtype foundedVar) -> varnat
        | otherwise -> Left ("redeclaration of '" <> ident <> "' with no linkage", cur) -- ODR
    Nothing -> varnat
    where
        ofs = (+) (fromIntegral $ CT.sizeof t) $ CT.alignas (maximumOffset $ locals vars) $ fromIntegral $ CT.alignof t
        varnat = let lvar = LVar t ofs cnd in
            Right (atLVar (lvtype lvar) (rbpOffset lvar), vars { locals = M.insert ident lvar $ locals vars })
addLVar _ _ _ _ = Left (internalCE, HT.emptyToken)


-- | If the specified token is `HT.TKIdent` and the global variable does not exist in the list, `addLVar` adds a new global variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addGVarWith :: (Ord i, Num i) => Natural -> CT.StorageClass i -> HT.TokenLC i -> GVarInitWith i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addGVarWith cnd t cur@(_, HT.TKIdent ident) iw vars = do
    (visibleGVar, visibleGlobals') <- mergeIntoMap cnd (globals vars)
    (_, externalGlobals') <- mergeIntoMap 0 (externalGlobals vars)
    pure
        ( atGVar (gvtype visibleGVar) ident
        , vars
            { globals = visibleGlobals'
            , externalGlobals = externalGlobals'
            }
        )
    where
        mergeIntoMap storedDepth sts = case M.lookup ident sts of
            Nothing ->
                let new = newGVar storedDepth
                 in Right (new, M.insert ident new sts)
            Just existing ->
                (\merged -> (merged, M.insert ident merged sts)) <$> mergeGVar storedDepth existing (newGVar storedDepth)

        newGVar = GVar t iw

        mergeGVar storedDepth lhs rhs = case mergeGVarTypes lhs rhs of
            Nothing -> Left ("redeclaration of '" <> ident <> "' with no linkage", cur)
            Just mergedType
                | isExternDecl lhs && isExternDecl rhs ->
                    Right $ lhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | isExternDecl lhs ->
                    Right $ rhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | isExternDecl rhs ->
                    Right $ lhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | isTentativeGVar lhs && isTentativeGVar rhs ->
                    Right $ lhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | isTentativeGVar lhs ->
                    Right $ rhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | isTentativeGVar rhs ->
                    Right $ lhs { gvtype = mergedType, gvNestDepth = storedDepth }
                | otherwise ->
                    Left ("redeclaration of '" <> ident <> "' with no linkage", cur)

        isTentativeGVar gvar = case initWith gvar of
            GVarInitWithZero -> True
            _                -> False

        isExternDecl gvar = case initWith gvar of
            GVarInitWithExternDecl -> True
            _                      -> False

        mergeGVarTypes lhs rhs = case (gvtype lhs, gvtype rhs) of
            (CT.SCAuto lhsTy, CT.SCAuto rhsTy) ->
                CT.SCAuto <$> CT.mergeCompatibleTypeKinds lhsTy rhsTy
            (CT.SCStatic lhsTy, CT.SCStatic rhsTy) ->
                CT.SCStatic <$> CT.mergeCompatibleTypeKinds lhsTy rhsTy
            (CT.SCStatic lhsTy, CT.SCAuto rhsTy)
                | isExternDecl rhs ->
                CT.SCStatic <$> CT.mergeCompatibleTypeKinds lhsTy rhsTy
            (CT.SCRegister lhsTy, CT.SCRegister rhsTy) ->
                CT.SCRegister <$> CT.mergeCompatibleTypeKinds lhsTy rhsTy
            (CT.SCUndef lhsTy, CT.SCUndef rhsTy) ->
                CT.SCUndef <$> CT.mergeCompatibleTypeKinds lhsTy rhsTy
            _ ->
                Nothing
addGVarWith _ _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | If the specified token is `HT.TKIdent` and the global variable does not exist in the list, `addLVar` adds a new global variable that will be initialized by zero to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addGVar :: (Ord i, Num i) => Natural -> CT.StorageClass i -> HT.TokenLC i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addGVar cnd t ident = addGVarWith cnd t ident GVarInitWithZero

-- | If the specified token is `HT.TKString`, `addLiteral` adds a new literal to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addLiteral :: (Ord i, Num i) => CT.StorageClass i -> HT.TokenLC i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addLiteral t (_, HT.TKString cont) vars
    | null (literals vars) = let lit = Literal (CT.removeAllExtents t) 0 cont; nat = ATNode (ATGVar t ".L.data.0") t ATEmpty ATEmpty in
        Right (nat, vars { literals = lit : literals vars })
    | otherwise = let ln' = succ $ ln $ head $ literals vars; lit = Literal (CT.removeAllExtents t) ln' cont; nat = ATNode (ATGVar t (".L.data." <> tshow ln')) t ATEmpty ATEmpty in
        Right (nat, vars { literals = lit : literals vars })
addLiteral _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))
