{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Binary.UTF8.String  (decodeString)
import           Control.Monad             (foldM)
import           Control.Monad.Extra       (partitionM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, gets, modify,
                                            put)
import qualified Data.ByteString.Char8     as B
import           Data.List                 (isSuffixOf)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Dhall.JSON                (omitNull)
import           Dhall.Yaml                (Options (..), defaultOptions,
                                            dhallToYaml)
import           Htcc.Utils                (tshow)
import qualified Options.Applicative       as OA
import           System.Directory          (createDirectoryIfMissing)
import           System.Directory          (doesDirectoryExist, listDirectory)
import           System.FilePath           ((</>))
import           System.IO                 (hFlush, stdout)
import           System.Process            (readCreateProcess, shell)
import qualified Tests.ComponentsTests     as ComponentsTests
import qualified Tests.SubProcTests        as SubProcTests
import           Tests.Utils

workDir :: FilePath
workDir = "/tmp" </> "htcc"

asmDir :: FilePath
asmDir = workDir </> "asm"

dockerComposePath :: FilePath
dockerComposePath = "." </> "docker" </> "test.dhall"

data Command = WithSubProc | WithDocker | WithSelf | WithComponents

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

componentsCmd :: OA.Mod OA.CommandFields Command
componentsCmd = OA.command "components" $
    OA.info (pure WithComponents) $ OA.progDesc "run unit tests of components"

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
      , componentsCmd
      ])

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ "The htcc unit tester"
  ]

genTestAsm' :: StateT Int IO [T.Text]
genTestAsm' = lift (createDirectoryIfMissing False workDir *> createDirectoryIfMissing False asmDir)
    *> go [] ("." </> "test" </> "Tests" </> "csrc" </> "self")
    where
        go s fname = do
            names <- lift $ map (fname </>) <$> listDirectory fname
            (dirPaths, filePaths) <- lift $ partitionM doesDirectoryExist names
            foldM (\fs f -> if ".c" `isSuffixOf` f then (:fs) <$> mkBin (T.pack f) else pure fs) s filePaths
                >>= flip (foldM go) dirPaths

        mkBin fname = do
            outAsmName <- gets (\n -> T.pack (asmDir </> "spec") <> tshow n <> ".s")
            lift $
                T.putStr ("[compiling] " <> fname)
                    *> hFlush stdout
                    *> execErrFin ("stack exec htcc -- " <> fname <> " > " <> outAsmName)
                    *> T.putStrLn (" -> " <> outAsmName)
            outAsmName <$ modify succ

genTestAsm :: IO [T.Text]
genTestAsm = evalStateT genTestAsm' 0

genTestBins' :: StateT Int IO [T.Text]
genTestBins' = (genTestAsm' <* put 0) >>= mapM f
    where
        f fname = do
            binName <- gets (\n -> T.pack (workDir </> "spec") <> tshow n <> ".out")
            lift $
                T.putStr ("[assembling] " <> fname)
                    *> hFlush stdout
                    *> execErrFin ("gcc -xassembler -no-pie -o " <> binName <> " " <> fname)
                    *> T.putStrLn (" -> " <> binName)
            binName <$ modify succ

genTestBins :: IO [T.Text]
genTestBins = evalStateT genTestBins' 0

createProcessDhallDocker :: FilePath -> String -> IO ()
createProcessDhallDocker fp cmd = T.readFile fp
    >>= dhallToYaml (defaultOptions { explain = True, omission = omitNull }) (Just fp)
    >>= readCreateProcess (shell $ "docker-compose -f - " <> cmd) . decodeString . B.unpack
    >>= putStrLn

runDhallDocker :: String -> IO ()
runDhallDocker = createProcessDhallDocker dockerComposePath

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    case optCmd opts of
        WithSubProc -> SubProcTests.exec
        WithDocker
            | optClean opts -> runDhallDocker "down --rmi all"
            | otherwise -> genTestAsm *> runDhallDocker "up --build" *> clean [workDir]
        WithSelf    -> genTestBins >>= mapM_ execErrFin >> clean [workDir]
        WithComponents -> ComponentsTests.exec
