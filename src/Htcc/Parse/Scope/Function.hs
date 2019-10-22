{-|
Module      : Htcc.Parse.Scope.Function
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parse.Scope.Function (
    Function (..),
    Functions,
    addFunction,
    lookupFunction,
    fallBack
) where

import GHC.Generics (Generic (..))
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parse.Utils (internalCE)
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Token.Core as HT

-- | The data type of a typedef tag
data Function = Function -- ^ The contypedefor of a typedef tag
    {
        fntype :: CT.TypeKind, -- ^ The type of this typedef
        fnDefined :: Bool -- ^ If the function is defined, it will be `True`, otherwise will be `False`.
    } deriving (Eq, Ord, Show, Generic)

instance NFData Function

-- | The typedefs data type
type Functions = M.Map T.Text Function

-- | Given the flag (when that is added function, it is `True`. otherwise `False`), type, identifier token, and `Functions`, 
-- if the specified identifier already exists in the same scope, 
-- return an error message and its location as a pair. 
-- Otherwise, add a new tag to `Functions` and return it. 
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
addFunction :: Num i => Bool -> CT.TypeKind -> HT.TokenLC i -> Functions -> Either (T.Text, HT.TokenLC i) Functions
addFunction df t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundFunc
        | not (fnDefined foundFunc) -> Right $ M.insert ident (Function t True) sts 
        | otherwise -> Left ("conflicting types for '" <> ident <> "'", cur) -- ODR
    Nothing -> Right $ M.insert ident (Function t df) sts
addFunction _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | `lookupFunction` searches for tags by the specified `T.Text` from `Functions`.
-- This is equivalent to `M.lookup`.
{-# INLINE lookupFunction #-}
lookupFunction :: T.Text -> Functions -> Maybe Function
lookupFunction = M.lookup

-- | Organize typedef list state after scoping. 
-- This function do nothing but it is defined so that it can be written in the same way as `Htcc.Parse.Var.fallBack`.
{-# INLINE fallBack #-}
fallBack :: Functions -> Functions -> Functions
fallBack = flip const
