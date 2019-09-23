{-|
Module      : Htcc.Utils
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

General-purpose utilities
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Htcc.Utils (
    -- * Extra functions for lists
    takeWhileLen,
    spanLen,
    lastInit,
    -- * For Data.Text
    tshow,
    -- * For Data.Maybe
    fstNothingIdx,
    -- * For Numeric.Natural
    toNatural,
    -- * For IO shortcuts
    putStrErr,
    putStrLnErr,
    err,
    -- * For triples
    first3,
    second3,
    third3,
    dropFst,
    dropSnd,
    dropThd,
    -- * Counter
    counter,
    -- * Boolean methods
    lor,
    land,
    sop,
    sopText,
    -- * For data type
    toInts
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple.Extra (both)
import Data.IORef (newIORef, readIORef, writeIORef)
import Numeric.Natural
import System.IO (stderr)
import System.Exit (exitFailure)

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
        takeWhileLen' n _ [] = (n, [])
        takeWhileLen' n f (x:xs)
            | f x = let (n', ys) = takeWhileLen' (succ n) f xs in (n', x:ys)
            | otherwise = (n, [])

-- | Almost the same as `span`, but returns the number of elements in the list that
-- satisfy @f@ at the same time.
spanLen :: (a -> Bool) -> [a] -> (Int, [a], [a])
spanLen = spanLen' 0
    where
        spanLen' n _ [] = (n, [], [])
        spanLen' n f xs@(x:xs')
            | f x = let (n', ys, zs) = spanLen' (succ n) f xs' in (n', x:ys, zs)
            | otherwise = (n, [], xs)

{-# INLINE tshow #-}
-- | Convert `Show` class instance to `Data.Text`.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | `fstNothingIdx` returns the index of the first `Nothing`. If `Nothing` is not exist in given list, returns `Nothing`.
fstNothingIdx :: [Maybe a] -> Maybe Int
fstNothingIdx [] = Nothing
fstNothingIdx (Just _:xs) = (1+) <$> fstNothingIdx xs
fstNothingIdx (Nothing:_) = Just 0

-- | `toNatural` is a shortcut for @fromIntegral :: Integral i => i -> Natural@
{-# INLINE toNatural #-}
toNatural :: forall i. Integral i => i -> Natural
toNatural = fromIntegral :: i -> Natural

-- | Standard error output shortcut (with new line).
putStrLnErr :: T.Text -> IO ()
putStrLnErr = T.hPutStrLn stderr

-- | Standard error output shortcut.
putStrErr :: T.Text -> IO ()
putStrErr = T.hPutStr stderr

-- | Standard error output and exit shortcut.
err :: T.Text -> IO ()
err = flip (>>) exitFailure . putStrLnErr

{-# INLINE first3 #-}
-- | Update the first component of triple.
first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (x, y, z) = (f x, y, z)

{-# INLINE second3 #-}
-- | Update the second component of triple.
second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (x, y, z) = (x, f y, z)

{-# INLINE third3 #-}
-- | Update the third component of triple.
third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (x, y, z) = (x, y, f z)

{-# INLINE dropFst #-}
-- | Drop first element of triple and returns pair
dropFst :: (a, b, c) -> (b, c)
dropFst (_, y, z) = (y, z)

{-# INLINE dropSnd #-}
-- | Drop second element of triple and returns pair
dropSnd :: (a, b, c) -> (a, c)
dropSnd (x, _, z) = (x, z)

{-# INLINE dropThd #-}
-- | Drop third element of triple and returns pair
dropThd :: (a, b, c) -> (a, b)
dropThd (x, y, _) = (x, y)

-- | The counter is incremented by one each time it is executed.
counter :: Enum a => a -> IO (IO a)
counter n = do
    c <- newIORef n
    return $ do
        t <- readIORef c
        writeIORef c $ succ t
        readIORef c

-- | For mappings \(f_i:X\to B\) to an element \(x\in X\) of a set \(X\), \(\displaystyle\bigvee_{i} f_i(x)\) where \(B\) is the boolean domain. 
-- This function will stop evaluation when the result of \(f_i(x)\) is `True` (short circuit evaluation).
-- This is equivalent to:
--
-- > f1 x || f2 x || f3 x == lor [f1, f2, f3] x
lor :: [a -> Bool] -> a -> Bool
lor [] _ = False
lor (f:fs) x | f x = True | otherwise = lor fs x

-- | For mappings \(f_i:X\to B\) to an element (\x\in X\) of a set \(X\), \(\displaystyle\bigwedge_{i} f_i(x)\) where \(B\) is the boolean domain.
-- This is equivalent to:
--
-- > f1 x && f2 x && f3 x == land [f1, f2, f3] x
land :: [a -> Bool] -> a -> Bool
land [] _ = False
land (f:fs) x = foldr ((&&) . flip id x) (f x) fs

-- | Sum of product form.
-- For mappings \(f_i:X\to B\) to an element \(x\in X\) of a set \(X\), \(\displaystyle\bigwedge_{j}\bigvee_{i} f_i(x_j)\) where \(B\) is the Boolean domain.
-- This function will stop evaluation when the result of \(f_i(x)\) is `True` (short circuit evaluation).
sop :: [a -> Bool] -> [a] -> Bool
sop = all . lor 

-- | The `T.Text` version of `sop`.
sopText :: [Char -> Bool] -> T.Text -> Bool
sopText = T.all . lor

-- | Convert the instance of `Integral` to Int. When it cause overflow, express it as a list of `Int`s divided into multiple values.
-- `toInts` is useful for functions that have an `Int` type as an argument. e.g.:
--
-- >>> toInts (fromIntegral (maxBound :: Int) + 1 :: Integer)
-- [9223372036854775807,1]
-- >>> toInts (fromIntegral (maxBound :: Int) * 3 + 4 :: Integer)
-- [9223372036854775807,9223372036854775807,9223372036854775807,4]
toInts :: Integral i => i -> [Int]
toInts x = if xd >= 1 && xm == 0 then [fromIntegral x] else replicate xd (maxBound :: Int) ++ [xm]
    where
        (xd, xm) = both fromIntegral $ x `divMod` fromIntegral (maxBound :: Int)
