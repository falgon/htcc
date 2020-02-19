{-|
Module      : Htcc.Utils.Bool
Description : Utilities of boolean
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Utilities of boolean
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Htcc.Utils.Bool (
    -- * Boolean methods
    lor,
    land,
    sop,
    sopText
) where

import Prelude hiding (toInteger)
import qualified Data.Text as T

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

