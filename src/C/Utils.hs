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
    err
) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- | Standard error output shortcut
putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

-- | Standard error output and exit shortcut
err :: String -> IO ()
err = flip (>>) exitFailure . putStrLnErr
