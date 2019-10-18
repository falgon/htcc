{-|
Module      : Htcc.Parse.Typedef
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parse.Typedef (
    Typedef (..),
    Typedefs,
    addTypedef,
    lookupTypedef,
    fallBack
) where

import GHC.Generics (Generic (..))
import Numeric.Natural
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parse.Utils (internalCE)
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Token.Core as HT

-- | The data type of a typedef tag
data Typedef = Typedef -- ^ The contypedefor of a typedef tag
    {
        tdtype :: CT.TypeKind, -- ^ The type of this typedef
        tdNestDepth :: !Natural -- ^ The nest depth of this typedef
    } deriving (Eq, Ord, Show, Generic)

instance NFData Typedef

-- | The typedefs data type
type Typedefs = M.Map T.Text Typedef

-- | Given the current nesting number, type, identifier token, and `Typedefs`, if the specified identifier already exists in the same scope, 
-- return an error message and its location as a pair. 
-- Otherwise, add a new tag to `Typedefs` and return it. 
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
addTypedef :: Num i => Natural -> CT.TypeKind -> HT.TokenLC i -> Typedefs -> Either (T.Text, HT.TokenLC i) Typedefs
addTypedef cnd t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundedTag
        | tdNestDepth foundedTag /= cnd -> tdnat
        | tdtype foundedTag == t -> tdnat
        | otherwise -> Left ("conflicting types for '" <> ident <> "'", cur) -- ODR
    Nothing -> tdnat
    where
        tdnat = Right $ M.insert ident (Typedef t cnd) sts
addTypedef _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | `lookupTypedef` searches for tags by the specified `T.Text` from `Typedefs`.
-- This is equivalent to `M.lookup`.
{-# INLINE lookupTypedef #-}
lookupTypedef :: T.Text -> Typedefs -> Maybe Typedef
lookupTypedef = M.lookup

-- | Organize typedef list state after scoping. 
-- This function do nothing but it is defined so that it can be written in the same way as `Htcc.Parse.Var.fallBack`.
{-# INLINE fallBack #-}
fallBack :: Typedefs -> Typedefs -> Typedefs
fallBack = const
