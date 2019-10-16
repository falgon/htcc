{-|
Module      : Htcc.Parse.Struct
Description : The Data type of struct and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parse.Struct (
    StructTag (..),
    Structs,
    addStructTag,
    lookupStructTag,
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

-- | The data type of a struct tag
data StructTag = StructTag -- ^ The constructor of a struct tag
    {
        sttype :: CT.TypeKind, -- ^ The type of this struct
        stNestDepth :: !Natural -- ^ The nest depth of this struct
    } deriving (Eq, Ord, Show, Generic)

instance NFData StructTag

-- | The structs data type
type Structs = M.Map T.Text StructTag

-- | Given the current nesting number, type, identifier token, and `Structs`, if the specified identifier already exists in the same scope, 
-- return an error message and its location as a pair. 
-- Otherwise, add a new tag to `Structs` and return it. 
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
addStructTag :: Num i => Natural -> CT.TypeKind -> HT.TokenLC i -> Structs -> Either (T.Text, HT.TokenLC i) Structs
addStructTag cnd t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundedTag
        | stNestDepth foundedTag /= cnd -> stnat
        | otherwise -> Left ("redefinition of 'struct " <> ident <> "'", cur) -- ODR
    Nothing -> stnat
    where
        stnat = Right $ M.insert ident (StructTag t cnd) sts
addStructTag _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | `lookupStructTag` searches for tags by the specified `T.Text` from `Structs`.
-- This is equivalent to `M.lookup`.
{-# INLINE lookupStructTag #-}
lookupStructTag :: T.Text -> Structs -> Maybe StructTag
lookupStructTag = M.lookup

-- | Organize struct list state after scoping. 
-- This function do nothing but it is defined so that it can be written in the same way as `Htcc.Parse.Var.fallBack`.
{-# INLINE fallBack #-}
fallBack :: Structs -> Structs -> Structs
fallBack = const
