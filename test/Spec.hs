{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tests.Utils
import qualified Tests.Test1 as Test1

main :: IO ()
main = runTestsEx [
    (Test1.test "42;", 42),
    (Test1.test "1+2;", 3),
    (Test1.test "1+2+4;", 7),
    (Test1.test "10-7+3;", 6),
    (Test1.test "42+23-30;", 35),
    (Test1.test "42/2+2-5;", 18),
    (Test1.test "(3+5)/2;", 4),
    (Test1.test "(4-2)*8+20/4;",21),
    (Test1.test "-(-3*+5);", 15),
    (Test1.test "-25+30;", 5),
    (Test1.test "42 == 42;", 1),
    (Test1.test "42 != 53;", 1),
    (Test1.test "42 < 53;", 1),
    (Test1.test "53 > 42;", 1),
    (Test1.test "42 <= 42;", 1),
    (Test1.test "32 <= 42;", 1),
    (Test1.test "42 >= 42;", 1),
    (Test1.test "53 >= 42;", 1),
    (Test1.test "(1 + 1) == 2;", 1),
    (Test1.test "(2 * 3) != 2;", 1),
    (Test1.test "a=42; b=20; a+b;", 62),
    (Test1.test "a=42; b=20; c=32; (a - c) * b / 10;", 20),
    (Test1.test "hoge=42; foo=20; hoge - foo;", 22)
    ]
