{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tests.Utils
import qualified Tests.Test1 as Test1

main :: IO ()
main = runTestsEx [
    (Test1.test "42", 42),
    (Test1.test "1+2+4", 7),
    (Test1.test "10-7+3", 6),
    (Test1.test "42+23-30", 35)
    ]
