{-# LANGUAGE OverloadedStrings #-}
module Tests.Test3 (
    test
) where

import Tests.Utils
import Control.Exception (finally)
import qualified Data.Text as T 

test :: String -> IO (Either T.Text T.Text)
test x = flip finally (clean ["tmp", "tmp.s", "test_func1.o"]) $ do 
    execErrFin "stack build"
    execErrFin $ "stack exec htcc -- \"" <> T.pack x <> "\" > tmp.s"
    execErrFin "cc -c test/Tests/csrc/test_func1.c"
    execErrFin "gcc test_func1.o tmp.s -o tmp"
    maybe (Left "The command did not execute successfully") Right <$> execStdOut "./tmp"
