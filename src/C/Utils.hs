{-|
Module      : C.Utils
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module C.Utils (
    putStrLnErr,
    err,
    first3,
    second3
) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- | Standard error output shortcut
putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

-- | Standard error output and exit shortcut
err :: String -> IO ()
err = flip (>>) exitFailure . putStrLnErr

{-# INLINE first3 #-}
first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (x, y, z) = (f x, y, z)

{-# INLINE second3 #-}
second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (x, y, z) = (x, f y, z)
