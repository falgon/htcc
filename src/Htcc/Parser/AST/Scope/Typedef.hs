{-|
Module      : Htcc.Parser.AST.Scope.Typedef
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parser.AST.Scope.Typedef (
    Typedef (..),
    Typedefs,
    add
) where

import GHC.Generics (Generic (..))
import Numeric.Natural
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parser.AST.Scope.ManagedScope
import Htcc.Parser.Utils (internalCE)
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Tokenizer.Token as HT

-- | The data type of a typedef tag
data Typedef a = Typedef -- ^ The contypedefor of a typedef tag
    {
        tdtype :: CT.TypeKind a, -- ^ The type of this typedef
        tdNestDepth :: !Natural -- ^ The nest depth of this typedef
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (Typedef i)

instance ManagedScope (Typedef i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The typedefs data type
type Typedefs a = M.Map T.Text (Typedef a)

-- | Given the current nesting number, type, identifier token, and `Typedefs`, if the specified identifier already exists in the same scope, 
-- return an error message and its location as a pair. 
-- Otherwise, add a new tag to `Typedefs` and return it. 
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: (Num i, Eq i) => Natural -> CT.TypeKind i -> HT.TokenLC i -> Typedefs i -> Either (ASTError i) (Typedefs i)
add cnd t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundedTag
        | tdNestDepth foundedTag /= cnd -> tdnat
        | tdtype foundedTag == t -> tdnat
        | otherwise -> Left ("conflicting types for '" <> ident <> "'", cur) -- ODR
    Nothing -> tdnat
    where
        tdnat = Right $ M.insert ident (Typedef t cnd) sts
add _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))