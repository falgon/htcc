{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T

import Htcc.Utils (putStrLnErr)
import Tests.Utils
import qualified Tests.SubProcTests as SubProcTests

{-# INLINE workDir #-}
workDir :: FilePath
workDir = "/tmp/htcc/"

{-# INLINE specPath #-}
specPath :: FilePath
specPath = workDir ++ "spec.s"

{-# INLINE dockerComposePath #-}
dockerComposePath :: FilePath
dockerComposePath = "./docker/docker-compose.yml"

genTestAsm :: IO ()
genTestAsm = do
    createDirectoryIfMissing False workDir
    execErrFin $ "stack exec htcc -- ./test/Tests/csrc/test_core.c > " <> T.pack specPath

inc :: IO ()
inc = flip finally (clean [workDir, "spec"]) $ do
    genTestAsm
    execErrFin $ "gcc -no-pie -o spec " <> T.pack specPath
    execErrFin "./spec"

docker :: IO ()
docker = flip finally (clean [workDir]) $
    genTestAsm >> execErrFin ("docker-compose -f " <> T.pack dockerComposePath <> " up --build")

main :: IO ()
main = do
    args <- getArgs
    case args of
        "subp":[] -> SubProcTests.exec 
        "inc":[] -> inc
        "docker":[] -> docker
        [] -> inc
        _ -> do
            putStrLnErr "--test-arguments are available by:"
            putStrLnErr "\tinc\t: Test itself with test code written in C (default, more faster)."
            putStrLnErr "\tsubp\t: Given C codes as input, run HUnit tests."
            exitFailure
