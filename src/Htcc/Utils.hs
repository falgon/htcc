{-|
Module      : Htcc.Utils
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module Htcc.Utils (
    putStrLnErr,
    err,
    first3,
    second3,
    third3,
    counter,
    lor,
    sop,
    sopText
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (stderr)
import System.Exit (exitFailure)

-- | Standard error output shortcut.
putStrLnErr :: T.Text -> IO ()
putStrLnErr = T.hPutStrLn stderr

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

-- | Sum of product form.
-- For mappings \(f_i:X\to B\) to an element \(x\in X\) of a set \(X\), \(\displaystyle\bigwedge_{j}\bigvee_{i} f_i(x_j)\) where \(B\) is the Boolean domain.
-- This function will stop evaluation when the result of \(f_i(x)\) is `True` (short circuit evaluation).
sop :: [a -> Bool] -> [a] -> Bool
sop = all . lor 

-- | The `T.Text` version of `sop`.
sopText :: [Char -> Bool] -> T.Text -> Bool
sopText = T.all . lor
