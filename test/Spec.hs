{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception  (finally)
import qualified Data.Text          as T
import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           Htcc.Utils         (putStrLnErr)
import qualified Tests.SubProcTests as SubProcTests
import           Tests.Utils

{-# INLINE workDir #-}
workDir :: FilePath
workDir = "/tmp/htcc/"

{-# INLINE specPath #-}
specPath :: FilePath
specPath = workDir ++ "spec.s"

{-# INLINE dockerComposePath #-}
dockerComposePath :: FilePath
dockerComposePath = "./docker/test.yml"

genTestAsm :: IO ()
genTestAsm = do
    createDirectoryIfMissing False workDir
    execErrFin $ "stack exec htcc -- ./test/Tests/csrc/test_core.c > " <> T.pack specPath

inc :: IO ()
inc = flip finally (clean [workDir, "spec"]) $ do
    genTestAsm
    execErrFin $ "gcc -no-pie -o spec " <> T.pack specPath
    execErrFin "./spec"

data DockerFlag = DkBuild | DkClean

docker :: DockerFlag -> IO ()
docker DkBuild = flip finally (clean [workDir]) $
    genTestAsm >> execErrFin ("docker-compose -f " <> T.pack dockerComposePath <> " up --build")
docker DkClean = execErrFin $ "docker-compose -f " <> T.pack dockerComposePath <> " down --rmi all"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["subp"] -> SubProcTests.exec
        ["inc"] -> inc
        ["docker"] -> docker DkBuild
        ["cleanDocker"] -> docker DkClean
        [] -> inc
        _ -> do
            putStrLnErr "--test-arguments are available by:"
            putStrLnErr "\tinc\t\t: Test itself with test code written in C (default, more faster)."
            putStrLnErr "\tsubp\t\t: Given C codes as input, run HUnit tests."
            putStrLnErr "\tdocker\t\t: Build the test using the environment inside the Linux container (This is useful when running tests in a development environment other than Linux)."
            putStrLnErr "\tcleanDocker\t: Erases containers and images built with the docker option."
            exitFailure
