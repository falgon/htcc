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
{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope.Tag (
    TagKind (..),
    Tag (..),
    Tags,
    TagHistory,
    emptyTagHistory,
    add
  , fallBackTags
  , lookupVisible
  , lookupAtScope
  , remember
) where

import           Control.DeepSeq                                 (NFData (..))
import qualified Data.Map                                        as M
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic (..))
import           Numeric.Natural

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.ConstructionData.Scope.ManagedScope
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Tokenizer.Token                            as HT

data TagKind
    = StructTag
    | EnumTag
    deriving (Eq, Ord, Show, Generic)

instance NFData TagKind

-- | The data type of a tag
data Tag i = Tag -- ^ The constructor of a tag
    {
        sttype      :: CT.StorageClass i, -- ^ The type of this tag
        stKind      :: TagKind, -- ^ The kind of this tag.
        stScopeId   :: !CT.ScopeId, -- ^ The scope identity of this tag.
        stNestDepth :: !Natural -- ^ The nest depth of this tag
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (Tag i)

instance ManagedScope (Tag i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The `Tags` data type
type Tags i = M.Map T.Text (Tag i)

-- | Historical tag bindings keyed by tag name and scope identity.
type TagHistory i = M.Map (T.Text, CT.ScopeId) (Tag i)

{-# INLINE emptyTagHistory #-}
emptyTagHistory :: TagHistory i
emptyTagHistory = M.empty

{-# INLINE fallBackTags #-}
fallBackTags :: Tags i -> Tags i -> Tags i
fallBackTags = const

{-# INLINE lookupVisible #-}
lookupVisible :: T.Text -> Tags i -> Maybe (Tag i)
lookupVisible = M.lookup

{-# INLINE lookupAtScope #-}
lookupAtScope :: T.Text -> CT.ScopeId -> TagHistory i -> Maybe (Tag i)
lookupAtScope ident scopeId =
    M.lookup (ident, scopeId)

{-# INLINE remember #-}
remember :: CT.ScopeId -> Natural -> TagKind -> CT.StorageClass i -> T.Text -> TagHistory i -> TagHistory i
remember scopeId depth kind ty ident =
    M.insert (ident, scopeId) (Tag ty kind scopeId depth)

-- | Given the current nesting number, type, identifier token, and `Tags`, if the specified identifier already exists in the same scope,
-- return an error message and its location as a pair.
-- Otherwise, add a new tag to `Tags` and return it.
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: Num i => Natural -> CT.ScopeId -> TagKind -> CT.StorageClass i -> HT.TokenLC i -> Tags i -> Either (ASTError i) (Tags i)
add cnd scopeId kind t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundedTag
        | stNestDepth foundedTag /= cnd -> stnat
        | stKind foundedTag /= kind -> Left (tagRedefinitionMsg kind ident, cur)
        | CT.isCTIncomplete (sttype foundedTag) -> stnat
        | otherwise -> Left (tagRedefinitionMsg kind ident, cur) -- ODR
    Nothing -> stnat
    where
        stnat = Right $ M.insert ident (Tag t kind scopeId cnd) sts

        tagRedefinitionMsg kind' ident' =
            "redefinition of '" <> tagKindText kind' <> " " <> ident' <> "'"

        tagKindText = \case
            StructTag -> "struct"
            EnumTag   -> "enum"
add _ _ _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))
