{-# LANGUAGE OverloadedStrings #-}
module Tests.Test1 (
    test
) where

import Tests.Utils
import Control.Exception (finally)
import Data.Text (pack)

test :: String -> IO Int
test x = flip finally (clean ["tmp", "tmp.s"]) $ do 
    execErrFin "stack build"
    execErrFin $ "stack exec htcc -- \"" <> pack x <> "\" > tmp.s"
    execErrFin "gcc -o tmp tmp.s"
    exitCode id 0 <$> exec "./tmp"
