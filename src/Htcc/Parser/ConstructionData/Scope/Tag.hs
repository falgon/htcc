{-|
Module      : Htcc.Parser.ConstructionData.Scope.Tag
Description : The Data type of struct and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parser.ConstructionData.Scope.Tag (
    Tag (..),
    Tags,
    add
) where

import GHC.Generics (Generic (..))
import Numeric.Natural
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parser.ConstructionData.Scope.ManagedScope
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Tokenizer.Token as HT

-- | The data type of a tag
data Tag i = Tag -- ^ The constructor of a tag
    {
        sttype :: CT.StorageClass i, -- ^ The type of this tag
        stNestDepth :: !Natural -- ^ The nest depth of this tag
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (Tag i)

instance ManagedScope (Tag i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The `Tags` data type
type Tags i = M.Map T.Text (Tag i)

-- | Given the current nesting number, type, identifier token, and `Tags`, if the specified identifier already exists in the same scope, 
-- return an error message and its location as a pair. 
-- Otherwise, add a new tag to `Tags` and return it. 
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: Num i => Natural -> CT.StorageClass i -> HT.TokenLC i -> Tags i -> Either (ASTError i) (Tags i)
add cnd t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundedTag
        | stNestDepth foundedTag /= cnd -> stnat
        | CT.isCTIncomplete (sttype foundedTag) -> stnat
        | otherwise -> Left ("redefinition of 'struct " <> ident <> "'", cur) -- ODR
    Nothing -> stnat
    where
        stnat = Right $ M.insert ident (Tag t cnd) sts
add _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

