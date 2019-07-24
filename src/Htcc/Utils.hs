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
    third3
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stderr)
import System.Exit (exitFailure)

-- | Standard error output shortcut
putStrLnErr :: T.Text -> IO ()
putStrLnErr = T.hPutStrLn stderr

-- | Standard error output and exit shortcut
err :: T.Text -> IO ()
err = flip (>>) exitFailure . putStrLnErr

{-# INLINE first3 #-}
-- | Update the first component of triple
first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (x, y, z) = (f x, y, z)

{-# INLINE second3 #-}
-- | Update the second component of triple
second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (x, y, z) = (x, f y, z)

{-# INLINE third3 #-}
-- | Update the third component of triple
third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (x, y, z) = (x, y, f z)
