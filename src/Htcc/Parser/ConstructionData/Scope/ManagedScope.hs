{-|
Module      : Htcc.Parser.ConstructionData.Scope.ManagedScope
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
module Htcc.Parser.ConstructionData.Scope.ManagedScope (
    ManagedScope (..),
    ASTError
) where

import qualified Data.Map             as M
import qualified Data.Text            as T
import           Htcc.Tokenizer.Token (TokenLC)

-- | Type classes common to concepts managed in scope
class ManagedScope a where
    -- | `Htcc.Parser.Scope.ManagedScope.lookup` searches for something managed by the scope by the specified `T.Text` from @a@.
    lookup :: T.Text -> M.Map T.Text a -> Maybe a
    -- | Organize @a@ list state after scoping.
    fallBack :: M.Map T.Text a -> M.Map T.Text a -> M.Map T.Text a
    -- | Helper function representing an empty something managed by the scope
    initial :: M.Map T.Text a

-- | The type to be used if an error occurs during AST construction.
type ASTError i = (T.Text, TokenLC i)
