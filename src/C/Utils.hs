module C.Utils (
    putStrLnErr,
    err
) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

err :: String -> IO ()
err = flip (>>) exitFailure . putStrLnErr
