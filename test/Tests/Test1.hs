{-# LANGUAGE OverloadedStrings #-}
module Tests.Test1 (
    test
) where

import Tests.Utils
import Control.Exception (finally)
import qualified Data.Text as T
import qualified Data.Text.IO as T 

test :: String -> IO Int
test x = flip finally (clean ["tmp", "tmp.s", "tmp.c"]) $ do 
    T.writeFile "./tmp.c" (T.pack x)
    execErrFin "stack exec htcc -- tmp.c > tmp.s"
    execErrFin "gcc -no-pie -o tmp tmp.s"
    exec "./tmp" >>= exitCode (\ec -> putStr x >> putStrLn ": [Processing]" >> return ec) (return 0)
