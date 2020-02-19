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
{-# LANGUAGE ScopedTypeVariables, BangPatterns, TupleSections, TypeOperators, Rank2Types #-}
module Htcc.Utils (
    -- * Extra functions for lists
    module Htcc.Utils.List,
    -- * For Monad
    bothM, (*^*),
    -- * For Data.Maybe
    maybe',
    -- * For Char
    isStrictSpace,
    -- * For Data.Text
    module Htcc.Utils.Text,
    -- * For Numeric.Natural
    toNatural, toInteger,
    -- * For print shortcuts
    module Htcc.Utils.Print,
    -- * For triples and quadruple
    module Htcc.Utils.Tuple,
    -- * Boolean methods
    module Htcc.Utils.Bool,
    -- * Natural transformations
    module Htcc.Utils.NaturalTransformations,
    -- * For data type
    toInts
) where

import Prelude hiding (toInteger)
import Data.Char (isSpace)
import Data.Tuple.Extra (both)
import Numeric.Natural

import Htcc.Utils.Print
import Htcc.Utils.Text
import Htcc.Utils.Tuple
import Htcc.Utils.List
import Htcc.Utils.Bool
import Htcc.Utils.NaturalTransformations

{-# INLINE maybe' #-}
-- | `maybe'` is `maybe` with changed argument order.
maybe' :: b -> Maybe a -> (a -> b) -> b
maybe' n m f = maybe n f m

-- | `toNatural` is a shortcut for @fromIntegral :: Integral i => i -> Natural@
{-# INLINE toNatural #-}
toNatural :: Integral i => i -> Natural
toNatural = fromIntegral

-- | `toInteger` is a shortcut for @fromIntegral :: Natural -> Integer@
{-# INLINE toInteger #-}
toInteger :: Natural -> Integer
toInteger = fromIntegral

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

-- | The monadic `Data.Tuple.Extra.both`.
-- e.g.:
-- 
-- >>> a <- newIORef (42 :: Int)
-- >>> b <- newIORef (53 :: Int)
-- >>> bothM readIORef (a, b) >>= print
-- (42,53)
bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = do
    x' <- f x
    (x',) <$> f y

infixr 3 *^*

-- | The monadic `Data.Tuple.Extra.***`.
-- e.g.:
--
-- >>> a <- newIORef 1
-- >>> b <- newIORef 2
-- >>> (writeIORef a *^* writeIORef b) (42, 53) >> bothM readIORef (a, b) >>= print
-- (42,53)
(*^*) :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
(*^*) f g (x, y) = do
    x' <- f x
    (x',) <$> g y
