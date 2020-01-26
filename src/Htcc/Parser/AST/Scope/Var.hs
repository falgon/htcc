{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Htcc.Parser.AST.Scope.Var
Description : The Data type of variables and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
module Htcc.Parser.AST.Scope.Var (
    -- * The data type
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
    addGVar,
    addLiteral,
    -- * Utilities
    initVars,
    resetLocal,
    fallBack
) where

import Data.Bits (Bits (..))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Numeric.Natural
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData (..))

import qualified Htcc.Parser.AST.Scope.ManagedScope as SM
import qualified Htcc.Tokenizer.Token as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parser.AST.Core (ATree (..), ATKind (..), Treealizable (..))
import Htcc.Parser.AST.Scope.Utils (internalCE)
import Htcc.Utils (tshow)

-- | The data type of global variable
newtype GVar i = GVar -- ^ The constructor of global variable
    {
        gvtype :: CT.StorageClass i -- ^ The type of the global variable
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (GVar i)

instance SM.ManagedScope (GVar i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The data type of local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        lvtype :: CT.StorageClass a, -- ^ The type of the local variable
        rbpOffset :: !a, -- ^ The offset value from RBP
        nestDepth :: !Natural -- ^ The nest depth of a local variable
    } deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (LVar a)

instance Treealizable LVar where
    {-# INLINE treealize #-}
    treealize (LVar t o _) = ATNode (ATLVar t o) t ATEmpty ATEmpty

instance SM.ManagedScope (LVar a) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The literal
data Literal a = Literal -- ^ The literal constructor
    {
        litype :: CT.StorageClass a, -- ^ The single literal type
        ln :: !Natural, -- ^ The number of labels placed in the @.data@ section
        lcts :: B.ByteString -- ^ The content
    } deriving (Eq, Show, Generic)

instance NFData a => NFData (Literal a)

-- The type of variables
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
        globals :: GlobalVars a, -- ^ The global variables
        locals :: LocalVars a, -- ^ The local variables
        literals :: Literals a -- ^ Literals
    } deriving (Show, Generic, Generic1)
    
instance NFData a => NFData (Vars a)

{-# INLINE initVars #-}
-- | Helper function representing an empty variables
initVars :: Vars a
initVars = Vars SM.initial SM.initial []

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
fallBack pre post = pre { literals = literals post }

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
            Right (ATNode (ATLVar (lvtype lvar) (rbpOffset lvar)) t ATEmpty ATEmpty, vars { locals = M.insert ident lvar $ locals vars })
addLVar _ _ _ _ = Left (internalCE, HT.emptyToken)


-- | If the specified token is `HT.TKIdent` and the global variable does not exist in the list, `addLVar` adds a new global variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addGVar :: Num i => CT.StorageClass i -> HT.TokenLC i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addGVar t cur@(_, HT.TKIdent ident) vars = flip (flip maybe $ const $ Left ("redeclaration of '" <> ident <> "' with no linkage", cur)) (lookupGVar ident vars) $ -- ODR
    let gvar = GVar t; nat = ATNode (ATGVar (gvtype gvar) ident) t ATEmpty ATEmpty in
            Right (nat, vars { globals = M.insert ident gvar $ globals vars })
addGVar _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | If the specified token is `HT.TKString`, `addLiteral` adds a new literal to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addLiteral :: (Ord i, Num i) => CT.StorageClass i -> HT.TokenLC i -> Vars i -> Either (SM.ASTError i) (ATree i, Vars i)
addLiteral t (_, HT.TKString cont) vars 
    | null (literals vars) = let lit = Literal (CT.removeAllExtents t) 0 cont; nat = ATNode (ATGVar t ".L.data.0") t ATEmpty ATEmpty in
        Right (nat, vars { literals = lit : literals vars })
    | otherwise = let ln' = succ $ ln $ head $ literals vars; lit = Literal (CT.removeAllExtents t) ln' cont; nat = ATNode (ATGVar t (".L.data." <> tshow ln')) t ATEmpty ATEmpty in
        Right (nat, vars { literals = lit : literals vars })
addLiteral _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))
