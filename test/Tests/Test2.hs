{-# LANGUAGE OverloadedStrings #-}
module Tests.Test2 (
    test
) where

import Tests.Utils
import Control.Exception (finally)
import Data.Text (pack)

test :: String -> IO Int
test x = flip finally (clean ["tmp", "tmp.s", "test_func1.o"]) $ do 
    execErrFin "stack build"
    execErrFin $ "stack exec htcc -- \"" <> pack x <> "\" > tmp.s"
    execErrFin "cc -c test/Tests/csrc/test_func1.c"
    execErrFin "gcc test_func1.o tmp.s -o tmp"
    exitCode id 0 <$> exec "./tmp"
