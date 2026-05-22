{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception         (bracket, finally, try)
import           Control.Monad             (foldM, when)
import           Control.Monad.Extra       (partitionM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, gets, modify,
                                            put)
import           Data.List                 (isSuffixOf)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Htcc.Utils                (tshow)
import qualified Options.Applicative       as OA
import           System.Directory          (createDirectoryIfMissing,
                                            doesDirectoryExist, listDirectory)
import           System.Environment        (lookupEnv, setEnv, unsetEnv)
import           System.Exit               (ExitCode (..), exitFailure,
                                            exitWith)
import           System.FilePath           ((</>))
import           System.IO                 (hFlush, hPutStr, stderr, stdout)
import           System.IO.Error           (isDoesNotExistError)
import           System.Process            (proc, readCreateProcessWithExitCode)
import           Tests.CommandSelection    (Command (..), autoHtccBinOverride,
                                            collectCommandExitCodes,
                                            commandsToRun,
                                            needsHtccCommandOverride,
                                            resolveCommand)
import qualified Tests.ComponentsTests     as ComponentsTests
import qualified Tests.SubProcTests        as SubProcTests
import           Tests.Utils

workDir :: FilePath
workDir = "/tmp" </> "htcc"

asmDir :: FilePath
asmDir = workDir </> "asm"

dockerComposePath :: FilePath
dockerComposePath = "." </> "docker" </> "test.dhall"

data Opts = Opts
    { optClean :: !Bool
    , optCmd   :: !(Maybe Command)
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
    <*> OA.optional (OA.hsubparser (mconcat [
            subProcCmd
          , dockerCmd
          , selfCmd
          , componentsCmd
          ]))

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "The htcc unit tester"
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
                htccCommand >>= \htccCmd ->
                T.putStr ("[compiling] " <> fname)
                    *> hFlush stdout
                    *> execErrFin (htccCmd <> " " <> fname <> " > " <> outAsmName)
                    *> T.putStrLn (" -> " <> outAsmName)
            outAsmName <$ modify succ

genTestAsm :: IO [T.Text]
genTestAsm = evalStateT genTestAsm' 0

genTestBins' :: StateT Int IO [T.Text]
genTestBins' = (genTestAsm' <* put 0) >>= mapM f
    where
        f fname = do
            binName <- gets (\n -> T.pack (workDir </> "spec") <> tshow n <> ".out")
            asmCmd <- lift $ assemblerCommand
                [ "-x"
                , "assembler"
                , "-no-pie"
                , "-o"
                , T.unpack binName
                , T.unpack fname
                ]
            lift $
                T.putStr ("[assembling] " <> fname)
                    *> hFlush stdout
                    *> execErrFin asmCmd
                    *> T.putStrLn (" -> " <> binName)
            binName <$ modify succ

genTestBins :: IO [T.Text]
genTestBins = evalStateT genTestBins' 0

createProcessDhallDocker :: FilePath -> [String] -> IO ()
createProcessDhallDocker fp cmd = do
    dockerCompose <- dockerComposeCommand
    dockerInput <- renderDhallYaml fp
    (dockerExitCode, dockerStdout, dockerStderr) <-
        readCreateProcessWithExitCode
            (uncurry proc $ dockerComposeArgs dockerCompose cmd)
            dockerInput
    putStr dockerStdout
    hFlush stdout
    hPutStr stderr dockerStderr
    when (dockerExitCode /= ExitSuccess) $
        exitWith dockerExitCode
    where
        dockerComposeCommand =
            maybe (pure ["docker", "compose"]) parseDockerComposeCommand =<< lookupEnv "DOCKER_COMPOSE"

        parseDockerComposeCommand "docker compose" = pure ["docker", "compose"]
        parseDockerComposeCommand "docker-compose" = pure ["docker-compose"]
        parseDockerComposeCommand value =
            fail $ "unsupported DOCKER_COMPOSE value: " <> value

        dockerComposeArgs [] composeArgs =
            dockerComposeArgs ["docker", "compose"] composeArgs
        dockerComposeArgs (exe:args) composeArgs =
            (exe, args <> ["-f", "-"] <> composeArgs)

renderDhallYaml :: FilePath -> IO String
renderDhallYaml fp = do
    dhallToYaml <- maybe "dhall-to-yaml" id <$> lookupEnv "DHALL_TO_YAML"
    result <- try $ readCreateProcessWithExitCode
        (proc dhallToYaml ["--explain", "--file", fp])
        ""
    case result of
        Left err
            | isDoesNotExistError err -> do
                hPutStr stderr $ unlines [
                    "dhall-to-yaml executable not found: " <> dhallToYaml
                  , "Run .travis/install-dhall-to-yaml.sh and ensure $HOME/.local/bin is on PATH, or set DHALL_TO_YAML."
                  ]
                exitFailure
            | otherwise -> ioError err
        Right (dhallExitCode, dhallStdout, dhallStderr) -> do
            hPutStr stderr dhallStderr
            when (dhallExitCode /= ExitSuccess) $
                exitWith dhallExitCode
            pure dhallStdout

runDhallDocker :: [String] -> IO ()
runDhallDocker = createProcessDhallDocker dockerComposePath

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    command <- resolveCommand (optClean opts) (optCmd opts)
    let commands = commandsToRun (optCmd opts) command
    autoCompilerCommand <-
        if needsHtccCommandOverride (optCmd opts) command
            then autoHtccBinOverride
            else pure Nothing
    exitCodes <- collectCommandExitCodes $ map (runCommand opts autoCompilerCommand) commands
    when (any (/= ExitSuccess) exitCodes) exitFailure

runCommand :: Opts -> Maybe T.Text -> Command -> IO ()
runCommand opts autoCompilerCommand command = case command of
    WithSubProc ->
        maybe
            SubProcTests.exec
            (\compilerCommand ->
                withEnvOverride "HTCC_BIN" (T.unpack compilerCommand) SubProcTests.exec
            )
            autoCompilerCommand
    WithDocker
        | optClean opts -> runDhallDocker ["down", "--rmi", "all"]
        | otherwise ->
            ( clean [workDir]
                *> genTestAsm
                *> runDhallDocker ["up", "--build", "--exit-code-from", "htcc"]
            ) `finally` clean [workDir]
    WithSelf ->
        maybe
            runGeneratedSelfTests
            (\compilerCommand ->
                withEnvOverride
                    "HTCC_BIN"
                    (T.unpack compilerCommand)
                    runGeneratedSelfTests
            )
            autoCompilerCommand
    WithComponents -> ComponentsTests.exec
    where
        runGeneratedSelfTests =
            (genTestBins >>= mapM_ execErrFin) `finally` clean [workDir]

withEnvOverride :: String -> String -> IO a -> IO a
withEnvOverride name value =
    bracket
        (do
            oldValue <- lookupEnv name
            setEnv name value
            pure oldValue
        )
        (maybe (unsetEnv name) (setEnv name))
        . const
