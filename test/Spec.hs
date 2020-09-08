{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Binary.UTF8.String (decodeString)
import           Control.Exception        (finally)
import qualified Data.ByteString.Char8    as B
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Dhall.JSON               (omitNull)
import           Dhall.Yaml               (Options (..), defaultOptions,
                                           dhallToYaml)
import qualified Options.Applicative      as OA
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          ((</>))
import           System.Process           (readCreateProcess, shell)
import qualified Tests.SubProcTests       as SubProcTests
import           Tests.Utils

workDir :: FilePath
workDir = "/tmp" </> "htcc"

specPath :: FilePath
specPath = workDir </> "spec.s"

dockerComposePath :: FilePath
dockerComposePath = "./docker" </> "test.dhall"

data Command = WithSubProc | WithDocker | WithSelf

data Opts = Opts
    { optClean :: !Bool
    , optCmd   :: !Command
    }

subProcCmd :: OA.Mod OA.CommandFields Command
subProcCmd = OA.command "subp" $
    OA.info (pure WithSubProc) $ OA.progDesc "run tests with subprocess"

dockerCmd :: OA.Mod OA.CommandFields Command
dockerCmd = OA.command "docker" $
    OA.info (pure WithDocker) $ OA.progDesc "run tests in docker container"

selfCmd :: OA.Mod OA.CommandFields Command
selfCmd = OA.command "self" $
    OA.info (pure WithSelf) $ OA.progDesc "run the test using htcc's processing power"

cleanOpt :: OA.Parser Bool
cleanOpt = OA.switch $ mconcat [
    OA.long "clean"
  , OA.help "clean the docker container"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> cleanOpt
    <*> OA.hsubparser (mconcat [
        subProcCmd
      , dockerCmd
      , selfCmd
      ])

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ "The htcc unit tester"
  ]

genTestAsm :: IO ()
genTestAsm = do
    createDirectoryIfMissing False workDir
    execErrFin $ "stack exec htcc -- " <> T.pack testCoreFile <> " > " <> T.pack specPath
    where
        testCoreFile = "./test" </> "Tests" </> "csrc" </> "test_core.c"

createProcessDhallDocker :: FilePath -> String -> IO ()
createProcessDhallDocker fp cmd = T.readFile fp
    >>= dhallToYaml (defaultOptions { explain = True, omission = omitNull }) (Just fp)
    >>= readCreateProcess (shell $ "docker-compose -f - " <> cmd) . decodeString . B.unpack
    >>= putStrLn

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    case optCmd opts of
        WithSubProc -> SubProcTests.exec
        WithDocker  -> let runDhallDocker = createProcessDhallDocker dockerComposePath in
            if optClean opts then
                runDhallDocker "down --rmi all"
            else
                flip finally (clean [workDir]) $
                    genTestAsm >> runDhallDocker "up --build"
        WithSelf    -> flip finally (clean [workDir, "spec"]) $ do
            genTestAsm
            execErrFin $ "gcc -no-pie -o spec " <> T.pack specPath
            execErrFin "./spec"
