{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.Test2 (
    test
) where

import Tests.Utils
import Control.Monad (forM_)
import Control.Exception (finally)
import qualified Data.Text as T 
import Data.List (unwords)

test :: String -> [String] -> IO (Int, String)
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s"] ++ obj) $ do 
        execErrFin $ "echo \'" <> T.pack x <> "\' | stack exec htcc -- /dev/stdin > tmp.s" 
        forM_ fnames $ \fname -> execErrFin $ "cc -c test/Tests/csrc/" <> T.pack fname <> ".c"
        execErrFin $ "gcc " <> T.pack (unwords obj) <> " tmp.s -o tmp"
        exitCode (,x) (0, x) <$> exec "./tmp"
