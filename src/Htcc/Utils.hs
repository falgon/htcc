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
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Htcc.Utils (
    -- * Extra functions for lists
    takeWhileLen,
    splitAtLen,
    spanLen,
    lastInit,
    -- * For Char
    isStrictSpace,
    -- * For Data.Text
    tshow,
    spanLenT,
    subTextIndex,
    -- * For Data.Either
    mapEither,
    -- * For Numeric.Natural
    toNatural,
    toInteger,
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
    -- * For quadruple
    fst4,
    snd4,
    thd4,
    fou4,
    first4,
    second4,
    third4,
    fourth4,
    dropFst4,
    dropSnd4,
    dropThd4,
    dropFourth4,
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

import Prelude hiding (toInteger)
import qualified Data.Text.Internal.Search as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isSpace)
import Data.Tuple.Extra (second, both)
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
        takeWhileLen' !n _ [] = (n, [])
        takeWhileLen' !n f (x:xs)
            | f x = second (x:) $ takeWhileLen' (succ n) f xs 
            | otherwise = (n, [])

-- | `splitAtLen`, simmilar to `splitAt` but also returns the length of the splited list.
splitAtLen :: Int -> [a] -> (Int, [a], [a])
splitAtLen !n = go n
    where
        go 0 xs = (n, [], xs)
        go !n' (x:xs) = second3 (x:) $ go (pred n') xs
        go !n' [] = (n - n', [], [])

-- | Almost the same as `span`, but returns the number of elements in the list that
-- satisfy @f@ at the same time.
spanLen :: (a -> Bool) -> [a] -> (Int, [a], [a])
spanLen = spanLen' 0
    where
        spanLen' !n _ [] = (n, [], [])
        spanLen' !n f xs@(x:xs')
            | f x = second3 (x:) $ spanLen' (succ n) f xs'
            | otherwise = (n, [], xs)

{-# INLINE tshow #-}
-- | Convert `Show` class instance to `Data.Text`.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | `T.Text` version of the `spanLen`.
spanLenT :: (Char -> Bool) -> T.Text -> (Int, T.Text, T.Text)
spanLenT = spanLenT' 0
    where
        spanLenT' !n f xs = case T.uncons xs of
            Just (x, xs') 
                | f x -> second3 (T.cons x) $ spanLenT' (succ n) f xs'
                | otherwise -> (n, T.empty, xs)
            Nothing -> (n, T.empty, T.empty)

-- | Continue mapping as long as the function returns `Right`. 
-- When the function returns `Left`, 
-- it discards the previous mapping and returns the value of `Left`.
mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither _ [] = Right []
mapEither f (x:xs) = f x >>= flip fmap (mapEither f xs) . (:) 

-- | `toNatural` is a shortcut for @fromIntegral :: Integral i => i -> Natural@
{-# INLINE toNatural #-}
toNatural :: Integral i => i -> Natural
toNatural = fromIntegral

-- | `toInteger` is a shortcut for @fromIntegral :: Natural -> Integer@
{-# INLINE toInteger #-}
toInteger :: Natural -> Integer
toInteger = fromIntegral

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

{-# INLINE dropFst4 #-}
-- | Drop first element of quadruple
dropFst4 :: (a, b, c, d) -> (b, c, d)
dropFst4 (_, b, c, d) = (b, c, d)

{-# INLINE dropSnd4 #-}
-- | Drop second element of quadruple
dropSnd4 :: (a, b, c, d) -> (a, c, d)
dropSnd4 (a, _, c, d) = (a, c, d)

{-# INLINE dropThd4 #-}
-- | Drop third element of quadruple
dropThd4 :: (a, b, c, d) -> (a, b, d)
dropThd4 (a, b, _, d) = (a, b, d)

{-# INLINE dropFourth4 #-}
-- | Drop fourth element of quadruple
dropFourth4 :: (a, b, c, d) -> (a, b, c)
dropFourth4 (a, b, c, _) = (a, b, c)

{-# INLINE fst4 #-}
-- | Take first element of quadruple
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

{-# INLINE snd4 #-}
-- | Take second element of quadruple
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

{-# INLINE thd4 #-}
-- | Take third element of quadruple
thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

{-# INLINE fou4 #-}
-- | Take fourth element of quadruple
fou4 :: (a, b, c, d) -> d
fou4 (_, _, _, d) = d

{-# INLINE first4 #-}
-- | Update first component of quadruple
first4 :: (a -> e) -> (a, b, c, d) -> (e, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

{-# INLINE second4 #-}
-- | Update second component of quadruple
second4 :: (b -> e) -> (a, b, c, d) -> (a, e, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

{-# INLINE third4 #-}
-- | Update third component of quadruple
third4 :: (c -> e) -> (a, b, c, d) -> (a, b, e, d)
third4 f (a, b, c, d) = (a, b, f c, d)

{-# INLINE fourth4 #-}
-- | Update fourth component of quadruple
fourth4 :: (d -> e) -> (a, b, c, d) -> (a, b, c, e)
fourth4 f (a, b, c, d) = (a, b, c, f d)

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
toInts !x = if xd >= 1 && xm == 0 then [fromIntegral x] else replicate xd (maxBound :: Int) ++ [xm]
    where
        (!xd, !xm) = both fromIntegral $ x `divMod` fromIntegral (maxBound :: Int)

-- | `isStrictSpace` returns True only if the given string is not a linefeed code and `Data.Char.isSpace` returns `True`, otherwise returns `False`. 
isStrictSpace :: Char -> Bool
isStrictSpace = land [(/='\n'), (/='\r'), isSpace]

-- | `subTextIndex` searches text for a substring of text and returns its starting position.
-- If nothing is found, `Nothing` is returned.
subTextIndex :: T.Text -> T.Text -> Maybe Int
subTextIndex s t = case T.indices s t of
    (i:_) -> Just i
    _ -> Nothing
