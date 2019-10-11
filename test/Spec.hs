{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Htcc.Utils (putStrLnErr)
import Tests.Utils
import qualified Tests.SubProcTests as SubProcTests

inc :: IO ()
inc = flip finally (clean ["spec", "spec.s"]) $ do
    execErrFin "stack exec htcc -- ./test/Tests/csrc/test_core.c > spec.s"
    execErrFin "gcc -no-pie -o spec spec.s"
    execErrFin "./spec"

main :: IO ()
main = do
    args <- getArgs
    if not (null args) && head args == "subp" then SubProcTests.exec 
    else if not (null args) && head args == "inc" || null args then inc
    else do
        putStrLnErr "--test-arguments are available by:"
        putStrLnErr "\tinc\t: Test itself with test code written in C (default, more faster)."
        putStrLnErr "\tsubp\t: Given C codes as input, run HUnit tests."
        exitFailure
