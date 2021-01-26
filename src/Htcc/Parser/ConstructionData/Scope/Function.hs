{-|
Module      : Htcc.Parser.ConstructionData.Scope.Function
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope.Function (
    Function (..),
    Functions,
    add
) where

import           Control.DeepSeq                                 (NFData (..))
import qualified Data.Map                                        as M
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic (..))

import Htcc.Parser.AST.Core (Treealizable (..), ATree (..), ATKind (..), atUnary)
import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.ConstructionData.Scope.ManagedScope
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Tokenizer.Token                            as HT

-- | The data type of a typedef tag
data Function a = Function -- ^ The contypedefor of a typedef tag
    {
        fntype    :: CT.StorageClass a, -- ^ The type of this typedef
        fnDefined :: Bool -- ^ If the function is defined, it will be `True`, otherwise will be `False`.
    } deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (Function a)

instance ManagedScope (Function i) where
    lookup = M.lookup
    fallBack = flip const
    initial = M.empty

-- TODO: allow function pointer
instance Treealizable Function where
    treealize (Function ftype _) = atUnary ATFuncPtr ftype ATEmpty

-- | The typedefs data typedefs
type Functions i = M.Map T.Text (Function i)

-- | Given the flag (when that is added function, it is `True`. otherwise `False`), type, identifier token, and `Functions`,
-- if the specified identifier already exists in the same scope,
-- return an error message and its location as a pair.
-- Otherwise, add a new tag to `Functions` and return it.
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: Num i => Bool -> CT.StorageClass i -> HT.TokenLC i -> Functions i -> Either (ASTError i) (Functions i)
add df t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundFunc
        | not (fnDefined foundFunc) -> Right $ M.insert ident (Function t True) sts
        | otherwise -> Left ("conflicting types for '" <> ident <> "'", cur) -- ODR
    Nothing -> Right $ M.insert ident (Function t df) sts
add _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

