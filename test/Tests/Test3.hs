{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.Test3 (
    test
) where

import Tests.Utils
import Control.Monad (forM_)
import Control.Exception (finally)
import qualified Data.Text as T 
import Data.List (unwords)

-- | `test` performs a test by comparison with the standard output string.
test :: String -> [String] -> IO (Either T.Text T.Text, String)
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s"] ++ obj) $ do 
        execErrFin $ "echo \'" <> T.pack x <> "\' | stack exec htcc -- /dev/stdin > tmp.s"
        forM_ fnames $ \fname -> execErrFin $ "cc -c test/Tests/csrc/" <> T.pack fname <> ".c"
        execErrFin $ "gcc " <> T.pack (unwords obj) <> " tmp.s -o tmp"
        maybe (Left "The command did not execute successfully", x) ((, x) . Right) <$> execStdOut "./tmp"
