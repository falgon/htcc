{-|
Module      : Htcc.Parser.ConstructionData.Scope.Enumerator
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope.Enumerator (
    Enumerator (..),
    Enumerators,
    add
) where

import           Control.DeepSeq                                 (NFData (..))
import qualified Data.Map                                        as M
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic (..))
import           Numeric.Natural                                 (Natural)

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (Treealizable (..),
                                                                  atNumLit)
import           Htcc.Parser.ConstructionData.Scope.ManagedScope
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Tokenizer.Token                            as HT

-- | The data type of a enumerator
data Enumerator i = Enumerator
    {
        enVal        :: i, -- ^ The value of enumerator
        enUnderlying :: CT.StorageClass i, -- ^ The underlying type of this enumerator
        enNestDepth  :: !Natural -- ^ The nest depth of this enumerator
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (Enumerator i)

instance Treealizable Enumerator where
    treealize (Enumerator val _ _) = atNumLit val

instance ManagedScope (Enumerator i) where
    lookup = M.lookup
    fallBack = const
    initial = M.empty

-- | The typedefs data typedefs
type Enumerators i = M.Map T.Text (Enumerator i)

-- | Given the flag (when that is added function, it is `True`. otherwise `False`), type, identifier token, and `Enumerators`,
-- if the specified identifier already exists in the same scope,
-- return an error message and its location as a pair.
-- Otherwise, add a new tag to `Enumerators` and return it.
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: Num i => Natural -> CT.StorageClass i -> HT.TokenLC i -> i -> Enumerators i -> Either (ASTError i) (Enumerators i)
add cnd t cur@(_, HT.TKIdent ident) val sts = case M.lookup ident sts of
    Just foundedEnum
        | enNestDepth foundedEnum /= cnd -> enat
        | otherwise -> Left ("redeclaration of enumerator '" <> ident <> "'", cur) -- ODR
    Nothing -> enat
    where
        enat = Right $ M.insert ident (Enumerator val t cnd) sts
add _ _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))
