{-|
Module      : Htcc.Tokenizer
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
module Htcc.Tokenizer (
    module Htcc.Tokenizer.Token,
    tokenize
) where

import qualified Data.Text as T
import Control.Monad ((>=>))

import Htcc.Tokenizer.Core (tokenize')
import Htcc.Tokenizer.Token
import Htcc.CRules.Preprocessor as CP
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

-- | Tokenize the `T.Text`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, @[TokenIdx i]@ is returned.
tokenize :: (Integral i, Read i, Show i) => T.Text -> Either (ASTError i) [TokenLC i]
tokenize = tokenize' >=> CP.preprocess
