{-|
Module      : Htcc.CRules.Preprocessor.Core
Description : The preprocessor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The preprocessor
-}
module Htcc.CRules.Preprocessor.Core (
    preprocess
) where

import qualified Htcc.Tokenizer.Token as HT
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

-- | The function that executes a proprocess. 
-- __NOTE__: This is not yet implemented.
preprocess :: [HT.TokenLC i] -> Either (ASTError i) [HT.TokenLC i]
preprocess = Right . filter (not . HT.isTKMacro . snd)
