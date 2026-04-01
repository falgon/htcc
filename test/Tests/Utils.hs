{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils (
    runTests
  , runTestsEx
  , Test (..)
  , (~:)
  , (~?=)
  , exitCode
  , exec
  , execStdOut
  , execErrFin
  , assemblerCommand
  , assemblerCommandPrefix
  , htccCommand
  , clean
) where

import qualified Control.Foldl            as F
import           Control.Monad            (void, when, zipWithM)
import           Data.Bool                (bool)
import           Data.Char                (isAlpha, isAlphaNum, isSpace)
import           Data.Functor             ((<&>))
import qualified Data.Text                as DT
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           executable, getPermissions,
                                           removeDirectoryRecursive, removeFile)
import           System.Environment       (lookupEnv)
import           System.FilePath          (searchPathSeparator, (</>))
import           Test.Hspec               (parallel)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.Hspec.Core.Runner   (Config (..), defaultConfig,
                                           evaluateSummary, runSpec)
import           Test.HUnit               (Test (..), (~:), (~?=))
import qualified Text.Parsec              as Parsec
import qualified Turtle                   as T

cfg :: Config
cfg = defaultConfig { configPrintCpuTime = True }

data CompilerCommand = CompilerCommand
    { compilerEnvOverrides :: [(String, String)]
    , compilerExecutable   :: FilePath
    , compilerArguments    :: [String]
    }

runTests :: Test -> IO ()
runTests ts = runSpec (parallel $ fromHUnitTest ts) cfg
    >>= evaluateSummary

runTestsSequential :: Test -> IO ()
runTestsSequential ts = runSpec (fromHUnitTest ts) cfg
    >>= evaluateSummary

exitCode :: (Int -> a) -> a -> T.ExitCode -> a
exitCode _ x T.ExitSuccess     = x
exitCode f _ (T.ExitFailure n) = f n

exec :: T.MonadIO m => DT.Text -> m T.ExitCode
exec = flip T.shell T.empty

execStdOut :: T.MonadIO m => DT.Text -> m (Maybe T.Text)
execStdOut cmd = fmap T.lineToText
    <$> T.fold (T.inshell cmd T.empty) F.head

execErrFin :: T.MonadIO m => DT.Text -> m ()
execErrFin cmd = T.shell cmd T.empty
    >>= exitCode (\x -> void $ T.die (cmd <> " failed with exit code: " <> T.repr x)) (return ())

htccCommand :: IO DT.Text
htccCommand = maybe "stack exec htcc --" DT.pack <$> lookupEnv "HTCC_BIN"

assemblerCommand :: [String] -> IO DT.Text
assemblerCommand args =
    renderShellWords . (++ args) <$> resolvedAssemblerWords

assemblerCommandPrefix :: IO DT.Text
assemblerCommandPrefix =
    renderShellWords <$> resolvedAssemblerWords

runTestsEx :: (Eq a, Show a) => [(IO (a, String), a)] -> IO ()
runTestsEx ts = putStrLn "\n\n== Unit Tests started =="
    *> zipWithM f ts ms
    >>= runTestsSequential . TestList
    where
        ms = take (length ts) $ iterate (+1) (1 :: Int)
        f (t, e) i = t
            <&> \(ec, t') -> (~:) ("test: #" ++ show i ++ ": " ++ t' ++ "\"") $ (~?= e) ec

clean :: [FilePath] -> IO ()
clean = mapM_ $ \x -> (>>=) (doesFileExist x) $ flip bool (removeFile x) $
    doesDirectoryExist x >>= flip when (removeDirectoryRecursive x)

resolvedAssemblerWords :: IO [String]
resolvedAssemblerWords = do
    compiler <- resolveCompilerCommand =<< resolveAssemblerSpec
    pure $
        map (\(name, value) -> name <> "=" <> value) (compilerEnvOverrides compiler)
            <> [compilerExecutable compiler]
            <> compilerArguments compiler

resolveAssemblerSpec :: IO String
resolveAssemblerSpec =
    pure . maybe "gcc" id . nonEmptyEnv =<< lookupEnv "HTCC_ASSEMBLER"

nonEmptyEnv :: Maybe String -> Maybe String
nonEmptyEnv (Just s) | all isSpace s = Nothing
nonEmptyEnv x                        = x

renderShellWords :: [String] -> DT.Text
renderShellWords = DT.unwords . map shellQuote

shellQuote :: String -> DT.Text
shellQuote word = "'" <> DT.replace "'" "'\"'\"'" (DT.pack word) <> "'"

shellWords :: String -> Either String [String]
shellWords commandLine =
    either (Left . show) Right $ Parsec.parse shellParser "<compiler>" commandLine
    where
        shellParser = skipSpaces *> Parsec.sepEndBy word spaces <* Parsec.eof
        skipSpaces = Parsec.skipMany $ Parsec.satisfy isSpace
        spaces = Parsec.skipMany1 $ Parsec.satisfy isSpace
        word = concat <$> Parsec.many1 chunk
        chunk = Parsec.choice [singleQuoted, doubleQuoted, escaped, bare]
        singleQuoted = Parsec.char '\'' *> Parsec.manyTill Parsec.anyChar (Parsec.char '\'')
        doubleQuoted = concat <$> (Parsec.char '"' *> Parsec.manyTill doubleChunk (Parsec.char '"'))
        doubleChunk = Parsec.choice [doubleEscaped, pure <$> Parsec.noneOf "\""]
        doubleEscaped = do
            _ <- Parsec.char '\\'
            c <- Parsec.anyChar
            pure $ case c of
                '\\' -> "\\"
                '"'  -> "\""
                '`'  -> "`"
                '$'  -> "$"
                '\n' -> ""
                _    -> ['\\', c]
        escaped = Parsec.char '\\' *> (pure <$> Parsec.anyChar)
        bare = Parsec.many1 $ Parsec.noneOf "'\"\\ \t\r\n"

resolveCompilerCommand :: String -> IO CompilerCommand
resolveCompilerCommand compiler = do
    parts <- case shellWords compiler of
        Left parseErr -> ioError . userError $
            "failed to parse compiler command " <> show compiler <> ": " <> parseErr
        Right [] -> ioError . userError $
            "empty compiler command: " <> show compiler
        Right xs -> pure xs
    let (envAssignments, compilerParts) = span isEnvironmentAssignmentWord parts
    when (null compilerParts) . ioError . userError $
        "empty compiler command: " <> show compiler
    let envOverrides = map splitEnvironmentAssignment envAssignments
    resolvedPrefix <- findExecutablePrefix envOverrides compilerParts
    pure $
        case resolvedPrefix of
            Just (compilerLen, resolvedCompiler) ->
                CompilerCommand
                    { compilerEnvOverrides = envOverrides
                    , compilerExecutable = resolvedCompiler
                    , compilerArguments = drop compilerLen compilerParts
                    }
            Nothing ->
                CompilerCommand
                    { compilerEnvOverrides = envOverrides
                    , compilerExecutable = head compilerParts
                    , compilerArguments = tail compilerParts
                    }
    where
        isEnvironmentAssignmentWord word = case span (/= '=') word of
            ([], _) -> False
            (name, '=':_) ->
                let startsLikeIdentifier c = isAlpha c || c == '_'
                 in startsLikeIdentifier (head name) && all (\c -> isAlphaNum c || c == '_') name
            _ -> False

        splitEnvironmentAssignment word = case span (/= '=') word of
            (name, '=':value) -> (name, value)
            _                 -> error "internal compiler error"

        findExecutablePrefix _ [] = pure Nothing
        findExecutablePrefix envOverrides' (cmd:_) = do
            resolved <- resolveExecutableCommand envOverrides' cmd
            pure $ fmap (\resolvedCmd -> (1, resolvedCmd)) resolved

        resolveExecutableCommand envOverrides' cmd =
            if hasExplicitPath cmd
                then localExecutablePath cmd
                else
                    firstResolved $
                        [findExecutableInSearchPath envOverrides' cmd]
                            <> [localExecutablePath cmd | not (hasOverriddenSearchPath envOverrides')]

        findExecutableInSearchPath envOverrides' cmd = do
            pathValue <- maybe
                (maybe "" id <$> lookupEnv "PATH")
                pure
                (lookup "PATH" envOverrides')
            firstResolved $
                map (localExecutablePath . searchPathCommand cmd) $
                    searchPathEntries pathValue

        searchPathCommand cmd ""  = cmd
        searchPathCommand cmd dir = dir </> cmd

        searchPathEntries pathValue = case break (== searchPathSeparator) pathValue of
            (dir, [])       -> [dir]
            (dir, _:remain) -> dir : searchPathEntries remain

        localExecutablePath cmd = do
            isLocalFile <- doesFileExist cmd
            isLocalExec <- if isLocalFile then executable <$> getPermissions cmd else pure False
            pure $
                if isLocalExec
                    then Just $ normalizeLocalExecutablePath cmd
                    else Nothing

        hasExplicitPath = any (`elem` ['/', '\\'])

        hasOverriddenSearchPath = any ((== "PATH") . fst)

        firstResolved [] = pure Nothing
        firstResolved (resolvePath : resolvePaths) = do
            resolved <- resolvePath
            maybe (firstResolved resolvePaths) (pure . Just) resolved

        normalizeLocalExecutablePath cmd
            | hasExplicitPath cmd = cmd
            | otherwise = "./" <> cmd
