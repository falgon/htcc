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

import qualified Data.Text as T

import qualified Htcc.Tokenizer.Token as HT

-- | The function that executes a proprocess. 
-- __NOTE__: This is not yet implemented.
preprocess :: [HT.TokenLC i] -> Either (HT.TokenLCNums i, T.Text) [HT.TokenLC i]
preprocess = Right . filter (not . HT.isTKMacro . snd)
