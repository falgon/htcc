{-|
Module      : Htcc.Utils.Tuple
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Utilities of tuple
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Htcc.Utils.Tuple (
    -- * For double
    swap,
    -- * For triples
    first3,
    second3,
    third3,
    dropFst3,
    dropSnd3,
    dropThd3,
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
    curry4,
    uncurry4,
) where

import Data.Tuple.Extra (first, second, dupe)

{-# INLINE swap #-}
-- | Swap a first element and second element
swap :: (a, b) -> (b, a)
swap = first snd . second fst . dupe

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

{-# INLINE dropFst3 #-}
-- | Drop first element of triple and returns pair
dropFst3 :: (a, b, c) -> (b, c)
dropFst3 (_, y, z) = (y, z)

{-# INLINE dropSnd3 #-}
-- | Drop second element of triple and returns pair
dropSnd3 :: (a, b, c) -> (a, c)
dropSnd3 (x, _, z) = (x, z)

{-# INLINE dropThd3 #-}
-- | Drop third element of triple and returns pair
dropThd3 :: (a, b, c) -> (a, b)
dropThd3 (x, y, _) = (x, y)

{-# INLINE dropFst4 #-}
-- | Drop first element of quadruple.
dropFst4 :: (a, b, c, d) -> (b, c, d)
dropFst4 (_, b, c, d) = (b, c, d)

{-# INLINE dropSnd4 #-}
-- | Drop second element of quadruple.
dropSnd4 :: (a, b, c, d) -> (a, c, d)
dropSnd4 (a, _, c, d) = (a, c, d)

{-# INLINE dropThd4 #-}
-- | Drop third element of quadruple.
dropThd4 :: (a, b, c, d) -> (a, b, d)
dropThd4 (a, b, _, d) = (a, b, d)

{-# INLINE dropFourth4 #-}
-- | Drop fourth element of quadruple.
dropFourth4 :: (a, b, c, d) -> (a, b, c)
dropFourth4 (a, b, c, _) = (a, b, c)

{-# INLINE fst4 #-}
-- | Take first element of quadruple.
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

{-# INLINE snd4 #-}
-- | Take second element of quadruple.
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

{-# INLINE thd4 #-}
-- | Take third element of quadruple.
thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

{-# INLINE fou4 #-}
-- | Take fourth element of quadruple.
fou4 :: (a, b, c, d) -> d
fou4 (_, _, _, d) = d

{-# INLINE first4 #-}
-- | Update first component of quadruple.
first4 :: (a -> e) -> (a, b, c, d) -> (e, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

{-# INLINE second4 #-}
-- | Update second component of quadruple.
second4 :: (b -> e) -> (a, b, c, d) -> (a, e, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

{-# INLINE third4 #-}
-- | Update third component of quadruple.
third4 :: (c -> e) -> (a, b, c, d) -> (a, b, e, d)
third4 f (a, b, c, d) = (a, b, f c, d)

{-# INLINE fourth4 #-}
-- | Update fourth component of quadruple.
fourth4 :: (d -> e) -> (a, b, c, d) -> (a, b, c, e)
fourth4 f (a, b, c, d) = (a, b, c, f d)

{-# INLINE curry4 #-}
-- | Converts an uncurried function to a curried function.
curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)

{-# INLINE uncurry4 #-}
-- | Converts a curried function to a function on a quadruple.
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
