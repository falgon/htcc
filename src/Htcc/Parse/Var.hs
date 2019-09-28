{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Htcc.Parse.Var
Description : The Data type of variables and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
module Htcc.Parse.Var (
    -- * The data type
    GVar (..),
    LVar (..),
    Vars (..),
    -- * Functions for adding and searching for variables
    lookupLVar,
    lookupGVar,
    lookupVar,
    addLVar,
    addGVar,
    resetLocal
) where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.List (find)
import qualified Data.Map.Strict as M

import qualified Htcc.Token as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parse.AST (ATree (..), ATKind (..))
import Htcc.Parse.Utils (internalCE)
import Htcc.Utils (tshow)


-- | The data type of global variable
newtype GVar = GVar -- ^ The constructor of global variable
    {
        gvtype :: CT.TypeKind -- ^ The type of global variable
    } deriving (Eq, Ord, Show)

-- | The data type of local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        lvtype :: CT.TypeKind, -- ^ The type of local variable
        rbpOffset :: a -- ^ The offset value from RBP
    } deriving (Eq, Ord, Show)

-- | The data type of local variables
data Vars a = Vars -- ^ The constructor of variables
    { 
        globals :: M.Map T.Text GVar, -- ^ The global variables
        locals :: M.Map T.Text (LVar a) -- ^ The local variables
    } deriving Show

{-# INLINE resetLocal #-}
-- | `resetLocal` initialize the local variable list for `Vars`
resetLocal :: Vars a -> Vars a
resetLocal = flip Vars M.empty . globals

{-# INLINE lookupGVar #-}
-- | Search for a global variable with a given name
lookupGVar :: T.Text -> Vars a -> Maybe GVar
lookupGVar s vars =  M.lookup s $ globals vars

{-# INLINE lookupLVar #-}
-- | Search for a local variable with a given name
lookupLVar :: T.Text -> Vars a -> Maybe (LVar a)
lookupLVar s vars = M.lookup s $ locals vars

{-# INLINE lookupVar #-}
-- | First, search for local variables, and if not found, search for global variables. If nothing is found, Nothing is returned
lookupVar :: T.Text -> Vars a -> Maybe (Either GVar (LVar a))
lookupVar s vars = maybe (Left <$> lookupGVar s vars) (Just . Right) $ lookupLVar s vars

{-# INLINE maximumOffset #-}
maximumOffset :: (Num a, Ord a) => M.Map T.Text (LVar a) -> a
maximumOffset m 
    | M.null m = 0
    | otherwise = maximum $ map rbpOffset $ M.elems m

-- | If the specified token is `HT.TKIdent` and the local variable does not exist in the list, `addLVar` adds a new local variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addLVar :: (Num i, Ord i) => CT.TypeKind -> HT.TokenIdx i -> Vars i -> Either (T.Text, HT.TokenIdx i) (ATree i, Vars i)
addLVar t cur@(_, HT.TKIdent ident) vars = flip (flip maybe $ const $ Left ("redeclaration of '" <> T.pack ident <> "' with no linkage", cur)) (lookupLVar (T.pack ident) vars) $ -- ODR rule
    let lvar = LVar t ((+) (fromIntegral (CT.sizeof t)) $ maximumOffset $ locals vars)
        nat = ATNode (ATLVar (lvtype lvar) (rbpOffset lvar)) t ATEmpty ATEmpty in
            Right (nat, vars { locals = M.insert (T.pack ident) lvar $ locals vars })
addLVar _ _ _ = Left (internalCE, (0, HT.TKEmpty))


-- | If the specified token is `HT.TKIdent` and the global variable does not exist in the list, `addLVar` adds a new global variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Right` and return it. Otherwise, returns an error message and token pair wrapped in `Left`.
addGVar :: Num i => (CT.TypeKind, HT.TokenIdx i) -> Vars i -> Either (T.Text, HT.TokenIdx i) (ATree i, Vars i)
addGVar (t, cur@(_, HT.TKIdent ident)) vars = flip (flip maybe $ const $ Left ("redeclaration of '" <> tshow ident <> "' with no linkage", cur)) (lookupGVar (T.pack ident) vars) $ -- ODR rule
    let gvar = GVar t
        nat = ATNode (ATGVar (gvtype gvar) $ T.pack ident) t ATEmpty ATEmpty in
            Right (nat, vars { globals = M.insert (T.pack ident) gvar $ globals vars })
addGVar _ _ = Left (internalCE, (0, HT.TKEmpty))
