{-# LANGUAGE OverloadedStrings #-}
module Tests.Test2 (
    test
) where

import Tests.Utils
import Control.Monad (forM_)
import Control.Exception (finally)
import Data.Text (pack)
import Data.List (unwords)

test :: String -> [String] -> IO Int
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s"] ++ obj) $ do 
        execErrFin "stack build"
        execErrFin $ "stack exec htcc -- \"" <> pack x <> "\" > tmp.s"
        forM_ fnames $ \fname -> execErrFin $ "cc -c test/Tests/csrc/" <> pack fname <> ".c"
        execErrFin $ "gcc " <> pack (unwords obj) <> " tmp.s -o tmp"
        exitCode id 0 <$> exec "./tmp"
