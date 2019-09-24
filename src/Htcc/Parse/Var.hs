{-# LANGUAGE OverloadedStrings, TupleSections #-}
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
    Var (..),
    -- * Functions for adding and searching for variables
    lookupLVar,
    lookupGVar,
    lookupVar,
    addLVar,
    addGVar,
    -- * Helper functions
    isGVar,
    isLVar
) where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.List (find)

import qualified Htcc.Token as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parse.AST (ATree (..), ATKind (..))

-- | The data type of variable
data Var a = GVar -- ^ The constructor of global variable
    { 
        gname :: String, -- ^ The name of global variable
        gvtype :: CT.TypeKind -- ^ The type of global variable
    } -- ^ The data type of global variable
    | LVar -- ^ The constructor of local variable
    {
        lname :: String, -- ^ The name of local variable
        lvtype :: CT.TypeKind, -- ^ The type of local variable
        offset :: a -- ^ The offset from RBP
    } -- ^ The data type of local variable
    deriving (Show, Eq)

{-# INLINE isGVar #-}
-- | `isGVar` returns `True` if `Var` is a `GVar`. Otherwise returns `False`.
isGVar :: Var a -> Bool
isGVar (GVar _ _) = True
isGVar _ = False

{-# INLINE isLVar #-}
-- | `isLVar` returns `True` if `Var` is a `LVar`. Otherwise returns `False`.
isLVar :: Var a -> Bool
isLVar = not . isGVar

{-# INLINE lookupVar #-}
lookupVar' :: Num i => (Var i -> String) -> (Var i -> Bool) -> String -> [Var i] -> Maybe (Var i)
lookupVar' f bf s = find ((==s) . f) . filter bf

-- | `lookupLVar` searches for a local variable that matches a specified token from a list of variables.
-- If it exists, it is wrapped in `Just` and returned. Otherwise, `Nothing` is returned.
lookupLVar :: Num i => HT.Token i -> [Var i] -> Maybe (Var i)
lookupLVar (HT.TKIdent s) vs = lookupVar' lname isLVar s vs
lookupLVar _ _ = Nothing

-- | `lookupGVar` searches for a global variable that matches a specified token from a list of variables.
-- If it exists, it is wrapped in `Just` and returned. Otherwise, `Nothing` is returned.
lookupGVar :: Num i => HT.Token i -> [Var i] -> Maybe (Var i)
lookupGVar (HT.TKIdent s) vs = lookupVar' gname isGVar s vs
lookupGVar _ _ = Nothing

-- | `lookupVar` first searches for local variables, and if it exists, wraps it in `Just` and returns that `Var`.
-- Otherwise, it searches for a global variable, and it it exists, wraps it in `Just` and returns it. Otherwise, `Nothing` is returned.
lookupVar :: Num i => HT.Token i -> [Var i] -> Maybe (Var i)
lookupVar s@(HT.TKIdent _) vs = lookupLVar s vs <|> lookupGVar s vs
lookupVar _ _ = Nothing

-- | If the specified token is `HT.TKIdent` and the local variable does not exist in the list, `addLVar` adds a new local variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Just` and return it. Otherwise, `Nothing` is returned.
addLVar :: Num i => (CT.TypeKind, HT.TokenIdx i) -> [Var i] -> Maybe (ATree i, [Var i])
addLVar (t, (_, HT.TKIdent ident)) vars = flip (`maybe` const Nothing) (lookupLVar (HT.TKIdent ident) vars) $ -- ODR rule
    let lvars = LVar ident t (if null vars then fromIntegral (CT.sizeof t) else offset (head vars) + fromIntegral (CT.sizeof t)):vars in
        Just (ATNode (ATLVar (lvtype $ head lvars) (offset $ head lvars)) t ATEmpty ATEmpty, lvars)
addLVar _ _ = Nothing

-- | If the specified token is `HT.TKIdent` and the global variable does not exist in the list, `addLVar` adds a new global variable to the list,
-- constructs a pair with the node representing the variable, wraps it in `Just` and return it. Otherwise, `Nothing` is returned.
addGVar :: Num i => (CT.TypeKind, HT.TokenIdx i) -> [Var i] -> Maybe (ATree i, [Var i])
addGVar (t, (_, HT.TKIdent ident)) vars = flip (`maybe` const Nothing) (lookupGVar (HT.TKIdent ident) vars) $ -- ODR rule
    let gvars = GVar ident t:vars in Just (ATNode (ATGVar (gvtype $ head gvars) $ T.pack $ gname $ head gvars) t ATEmpty ATEmpty, gvars)
addGVar _ _ = Nothing
