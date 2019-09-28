{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Htcc.Parse.Utils
Description : The AST data type and its utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The utilities of parsing
-}
module Htcc.Parse.Utils (
    expectedMessage,
    internalCE
) where

import qualified Data.Text as T
import qualified Htcc.Token as HT
import Htcc.Utils (tshow)

-- | "expected" error message
expectedMessage :: Show i => T.Text -> HT.TokenIdx i -> [HT.TokenIdx i] -> (T.Text, HT.TokenIdx i)
expectedMessage x t xs
    | length xs > 1 = ("expected '" <> x <> "' token before '" <> tshow (snd (xs !! 1)) <> "'", head xs)
    | otherwise = ("expected '" <> x <> "' token", if null xs then t else head xs)

{-# INLINE internalCE #-}
-- | the message of an internal compiler error
internalCE :: T.Text
internalCE = "internal compiler error: Please submit a bug report with preprocessed source if appropriate.\nPlease see this repository: <URL> "
