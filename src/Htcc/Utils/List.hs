{-|
Module      : Htcc.Utils.List
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

List utilities
-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Htcc.Utils.List (
    -- * Extra functions for lists
    takeWhileLen,
    splitAtLen,
    spanLen,
    lastInit
) where

import           Data.Tuple.Extra (second)
import           Htcc.Utils.Tuple
import           Prelude          hiding (toInteger)

-- | `lastInit` returns @Just (init xxs)@ when @f (last x) == True@ for then given list @xxs@.
-- Otherwise, returns `Nothing`
lastInit :: (a -> Bool) -> [a] -> Maybe [a]
lastInit _ [] = Nothing
lastInit f [x]
    | f x = Just []
    | otherwise = Nothing
lastInit y (x:xs) = (x:) <$> lastInit y xs

-- | `takeWhileLen`, applied to a predicate @f@ and a list @xs@, returns the
-- longest prefix (possibly empty) of @xs@ of elements that satisfy @f@ and
-- the length of the list taken. The time complexity of this function is
-- equivalent to `takeWhile`.
takeWhileLen :: (a -> Bool) -> [a] -> (Int, [a])
takeWhileLen = takeWhileLen' 0
    where
        takeWhileLen' !n _ [] = (n, [])
        takeWhileLen' !n f (x:xs)
            | f x = second (x:) $ takeWhileLen' (succ n) f xs
            | otherwise = (n, [])

-- | `splitAtLen`, simmilar to `splitAt` but also returns the length of the splited list.
splitAtLen :: Int -> [a] -> (Int, [a], [a])
splitAtLen !n = go n
    where
        go 0 xs       = (n, [], xs)
        go !n' (x:xs) = second3 (x:) $ go (pred n') xs
        go !n' []     = (n - n', [], [])

-- | Almost the same as `span`, but returns the number of elements in the list that
-- satisfy @f@ at the same time.
spanLen :: (a -> Bool) -> [a] -> (Int, [a], [a])
spanLen = spanLen' 0
    where
        spanLen' !n _ [] = (n, [], [])
        spanLen' !n f xs@(x:xs')
            | f x = second3 (x:) $ spanLen' (succ n) f xs'
            | otherwise = (n, [], xs)
