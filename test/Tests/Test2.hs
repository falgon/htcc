{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.Test2 (
    test
) where

import Tests.Utils
import Control.Monad (forM_)
import Control.Exception (finally)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Data.List (unwords)

test :: String -> [String] -> IO (Int, String)
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s", "tmp.c"] ++ obj) $ do 
        T.writeFile "./tmp.c" (T.pack x)
        execErrFin "stack exec htcc -- tmp.c > tmp.s"
        forM_ fnames $ \fname -> execErrFin $ "cc -c test/Tests/csrc/" <> T.pack fname <> ".c"
        execErrFin $ "gcc " <> T.pack (unwords obj) <> " tmp.s -o tmp"
        exitCode (,x) (0, x) <$> exec "./tmp"
