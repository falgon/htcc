{-|
Module      : Htcc.Parser.AST.Scope.Enumerator
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Htcc.Parser.AST.Scope.Enumerator (
    Enumerator (..),
    Enumerators,
    add
) where

import GHC.Generics (Generic (..))
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parser.AST.Scope.ManagedScope
import Htcc.Parser.Utils (internalCE)
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Tokenizer.Token as HT

-- | The data type of a enumerator
data Enumerator i = Enumerator 
    {
        enVal :: i, -- ^ The value of enumerator
        enUnderlying :: CT.TypeKind i -- ^ The underlying type of this enumerator
    } deriving (Eq, Ord, Show, Generic)

instance NFData i => NFData (Enumerator i)

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
add :: Num i => CT.TypeKind i -> HT.TokenLC i -> i -> Enumerators i -> Either (ASTError i) (Enumerators i)
add t cur@(_, HT.TKIdent ident) val sts = case M.lookup ident sts of
    Just _ -> Left ("redeclaration of enumerator '" <> ident <> "'", cur) -- ODR
    Nothing -> Right $ M.insert ident (Enumerator val t) sts
add _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))
