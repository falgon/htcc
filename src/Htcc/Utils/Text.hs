{-|
Module      : Htcc.Utils.Text
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Text utilities
-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Htcc.Utils.Text (
    -- * For Data.Text
    tshow,
    spanLenT,
    subTextIndex
) where

import Prelude hiding (toInteger)
import qualified Data.Text.Internal.Search as T
import qualified Data.Text as T
import Htcc.Utils.Tuple (second3)

{-# INLINE tshow #-}
-- | Convert `Show` class instance to `Data.Text`.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | `T.Text` version of the `Htcc.Utils.List.spanLen`.
spanLenT :: (Char -> Bool) -> T.Text -> (Int, T.Text, T.Text)
spanLenT = spanLenT' 0
    where
        spanLenT' !n f xs = case T.uncons xs of
            Just (x, xs') 
                | f x -> second3 (T.cons x) $ spanLenT' (succ n) f xs'
                | otherwise -> (n, T.empty, xs)
            Nothing -> (n, T.empty, T.empty)

-- | `subTextIndex` searches text for a substring of text and returns its starting position.
-- If nothing is found, `Nothing` is returned.
subTextIndex :: T.Text -> T.Text -> Maybe Int
subTextIndex s t = case T.indices s t of
    (i:_) -> Just i
    _ -> Nothing

