{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.Utils (
    runTests
  , runTestsSequential
  , runTestsEx
  , Test (..)
  , (~:)
  , (~?=)
  , CompilerCommand (..)
  , exitCode
  , exec
  , execStdOut
  , execErrFin
  , assemblerCompilerCommand
  , assemblerCommand
  , assemblerCommandPrefix
  , absoluteHtccCommand
  , absoluteHtccCommandWith
  , currentCheckoutRootFromDirectories
  , findRepoBuiltExecutableNear
  , htccCommand
  , htccCommandFor
  , htccCommandForHost
  , expandWindowsEnvironmentVariablesForTest
  , renderCompilerCommandForHost
  , resolveCheckoutRootAndCompilerFromDirectories
  , pinStackLauncherToRepoRoot
  , probeCompilerCommandAvailable
  , probeCompilerShellCommandAvailable
  , probeCompilerShellCommandAvailableInDirectory
  , readCompilerProcessWithExitCode
  , readCompilerProcessWithExitCodeIn
  , resolveCompilerCommandInForHost
  , resolveCompilerCommandIn
  , resolveCompilerCommand
  , clean
) where

import           Control.Applicative      ((<|>))
import           Control.Exception        (bracket)
import qualified Control.Foldl            as F
import           Control.Monad            (filterM, void, when, zipWithM)
import           Data.Bifunctor           (first)
import           Data.Bool                (bool)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import           Data.Char                (isAlpha, isAlphaNum, isControl,
                                           isSpace, toLower)
import           Data.Functor             ((<&>))
import           Data.List                (foldl', isInfixOf, isPrefixOf,
                                           isSuffixOf, tails)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (catMaybes, fromMaybe, isJust,
                                           isNothing)
import qualified Data.Text                as DT
import           Data.Time.Clock          (UTCTime)
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           executable, getCurrentDirectory,
                                           getModificationTime, getPermissions,
                                           getTemporaryDirectory, listDirectory,
                                           makeAbsolute,
                                           removeDirectoryRecursive, removeFile)
import           System.Environment       (getEnvironment, getExecutablePath,
                                           lookupEnv)
import           System.Exit              (ExitCode (ExitSuccess))
import           System.FilePath          (isAbsolute, makeRelative, normalise,
                                           pathSeparator, searchPathSeparator,
                                           splitDirectories, takeDirectory,
                                           takeExtension, takeFileName, (</>))
import           System.Info              (os)
import           System.IO                (hClose, hPutStr, openTempFile)
import           System.IO.Error          (catchIOError)
import qualified System.Process           as Process
import           System.Process           (CreateProcess (cwd, env), proc,
                                           readCreateProcessWithExitCode)
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
    { compilerEnvOverrides     :: [(String, String)]
    , compilerEnvOverrideSpecs :: Maybe [CompilerEnvOverrideSpec]
    , compilerExecutable       :: FilePath
    , compilerArguments        :: [String]
    }

data ShellWordSpan = ShellWordSpan
    { shellWordSpanText                      :: String
    , shellWordSpanAllowsPosixExpansion      :: Bool
    , shellWordSpanAllowsPosixFieldSplitting :: Bool
    }

data ParsedShellWord = ParsedShellWord
    { parsedShellWordSpans               :: [ShellWordSpan]
    , parsedShellWordPreservesEmptyField :: Bool
    }

newtype CompilerEnvOverrideSpec = CompilerEnvOverrideSpec
    { compilerEnvOverrideSpans :: [ShellWordSpan]
    }

data PosixShellContext
    = PosixShellLiteralContext
    | PosixShellDoubleQuotedContext
    | PosixShellUnquotedContext
    deriving (Eq)

data ShellWordChar = ShellWordChar
    { shellWordCharText    :: Char
    , shellWordCharContext :: PosixShellContext
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
htccCommand = do
    maybeExplicitCompilerCommand <- lookupEnv "HTCC_BIN"
    (_, maybeRepoBuiltCompilerPath) <- resolveCheckoutRootAndCompiler
    pure $ htccCommandFor maybeExplicitCompilerCommand maybeRepoBuiltCompilerPath

htccCommandFor :: Maybe String -> Maybe FilePath -> DT.Text
htccCommandFor =
    htccCommandForHost os

htccCommandForHost :: String -> Maybe String -> Maybe FilePath -> DT.Text
htccCommandForHost hostOs maybeCompilerCommand maybeRepoBuiltCompilerPath =
    maybe
        (maybe "stack exec htcc --" (shellQuoteForHost hostOs) maybeRepoBuiltCompilerPath)
        DT.pack
        (nonEmptyEnv maybeCompilerCommand)

shellQuoteForHost :: String -> String -> DT.Text
shellQuoteForHost hostOs
    | hostOs == "mingw32" = cmdExeQuote
    | otherwise           = shellQuote

cmdExeQuote :: String -> DT.Text
cmdExeQuote =
    DT.pack . quoteWindowsCommandWord . escapeWindowsDelayedExpansion . escapeWindowsPercentExpansion

cmdExeSetAssignmentQuote :: String -> DT.Text
cmdExeSetAssignmentQuote =
    DT.pack
        . (\value -> "\"" <> value <> "\"")
        . concatMap escapeWindowsSetAssignmentQuote
        . escapeWindowsSetAssignmentExpansion

escapeWindowsSetAssignmentQuote :: Char -> String
escapeWindowsSetAssignmentQuote c
    | c == '"'  = "\"\""
    | otherwise = [c]

escapeWindowsPercentExpansion :: String -> String
escapeWindowsPercentExpansion =
    concatMap $ \c ->
        if c == '%'
            then "%%"
            else [c]

escapeWindowsDelayedExpansion :: String -> String
escapeWindowsDelayedExpansion =
    concatMap $ \c ->
        if c == '!'
            then "^!"
            else [c]

escapeWindowsSetAssignmentExpansion :: String -> String
escapeWindowsSetAssignmentExpansion [] = []
escapeWindowsSetAssignmentExpansion ('%':xs) =
    case break (== '%') xs of
        (name, '%':rest)
            | isWindowsEnvExpansion name ->
                "!" <> name <> "!" <> escapeWindowsSetAssignmentExpansion rest
        _ ->
            "%%" <> escapeWindowsSetAssignmentExpansion xs
    where
        isWindowsEnvExpansion name =
            not (null name) && all isWindowsEnvExpansionChar name

        isWindowsEnvExpansionChar c =
            not (isSpace c) && c /= '"' && c /= '%'
escapeWindowsSetAssignmentExpansion ('!':xs) =
    "^!" <> escapeWindowsSetAssignmentExpansion xs
escapeWindowsSetAssignmentExpansion (x:xs) =
    x : escapeWindowsSetAssignmentExpansion xs

quoteWindowsCommandWord :: String -> String
quoteWindowsCommandWord word =
    "\"" <> go word <> "\""
    where
        go [] = []
        go xs =
            let (backslashes, rest) = span (== '\\') xs
                escapedBackslashes n = replicate n '\\'
             in case rest of
                    [] ->
                        escapedBackslashes (2 * length backslashes)
                    '"':ys ->
                        escapedBackslashes (2 * length backslashes + 1)
                            <> "\""
                            <> go ys
                    c:ys ->
                        backslashes <> [c] <> go ys

assemblerCompilerCommand :: IO CompilerCommand
assemblerCompilerCommand =
    resolveCompilerCommand =<< resolveAssemblerSpec

assemblerCommand :: [String] -> IO DT.Text
assemblerCommand args = do
    compiler <- assemblerCompilerCommand
    pure $ renderCompilerCommand compiler args

assemblerCommandPrefix :: IO DT.Text
assemblerCommandPrefix = do
    compiler <- assemblerCompilerCommand
    pure $ renderCompilerCommand compiler []

absoluteHtccCommand :: IO DT.Text
absoluteHtccCommand = do
    maybeExplicitCompilerCommand <- lookupEnv "HTCC_BIN"
    (maybeRepoRoot, maybeRepoBuiltCompilerPath) <- resolveCheckoutRootAndCompiler
    absoluteHtccCommandWith
        maybeExplicitCompilerCommand
        maybeRepoRoot
        maybeRepoBuiltCompilerPath

absoluteHtccCommandWith :: Maybe String -> Maybe FilePath -> Maybe FilePath -> IO DT.Text
absoluteHtccCommandWith maybeExplicitCompilerCommand maybeRepoRoot maybeRepoBuiltCompilerPath =
    case nonEmptyEnv maybeExplicitCompilerCommand of
        Just explicitCompilerCommand ->
            pure $ DT.pack explicitCompilerCommand
        Nothing -> do
            maybeFreshRepoBuiltCompilerPath <-
                preferredRepoBuiltCompilerPath maybeRepoRoot maybeRepoBuiltCompilerPath
            compiler0 <-
                maybe
                    (resolveCompilerCommand . DT.unpack $ htccCommandFor Nothing Nothing)
                    (\repoBuiltCompilerPath ->
                        pure $
                            CompilerCommand
                                { compilerEnvOverrides = []
                                , compilerEnvOverrideSpecs = Nothing
                                , compilerExecutable = repoBuiltCompilerPath
                                , compilerArguments = []
                                }
                    )
                    maybeFreshRepoBuiltCompilerPath
            compilerPath <- absolutizeCompilerExecutable $ compilerExecutable compiler0
            let compiler1 = compiler0 { compilerExecutable = compilerPath }
                compiler =
                    maybe
                        compiler1
                        (`pinStackLauncherToRepoRoot` compiler1)
                        maybeRepoRoot
            pure $ renderCompilerCommand compiler []

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

resolveAssemblerSpec :: IO String
resolveAssemblerSpec =
    fromMaybe "gcc" . nonEmptyEnv <$> lookupEnv "HTCC_ASSEMBLER"

nonEmptyEnv :: Maybe String -> Maybe String
nonEmptyEnv (Just s) | all isSpace s = Nothing
nonEmptyEnv x                        = x

renderCompilerCommand :: CompilerCommand -> [String] -> DT.Text
renderCompilerCommand =
    renderCompilerCommandForHost os

renderCompilerCommandForHost :: String -> CompilerCommand -> [String] -> DT.Text
renderCompilerCommandForHost hostOs compiler extraArgs
    | hostOs == "mingw32" =
        DT.intercalate " && " $
            ["setlocal EnableDelayedExpansion" | windowsCommandNeedsDelayedExpansion]
                <> map renderWindowsEnvOverride (compilerEnvOverrides compiler)
                <> [renderCommandWords]
    | otherwise =
        DT.unwords $
            renderPosixEnvAssignments
                <> quotedCommandWords
    where
        quoteWord = shellQuoteForHost hostOs
        renderPosixEnvAssignments =
            zipOverrideSpecs (compilerEnvOverrides compiler) (compilerEnvOverrideSpecs compiler)
                <&> uncurry renderPosixEnvAssignment
        commandWords =
            [compilerExecutable compiler] <> compilerInvocationArgs compiler extraArgs
        windowsCommandNeedsDelayedExpansion =
            not (null (compilerEnvOverrides compiler))
                || any ('!' `elem`) commandWords
        quotedCommandWords =
            map quoteWord commandWords
        renderCommandWords =
            DT.unwords quotedCommandWords
        renderPosixEnvAssignment (name, value) maybeOverrideSpec =
            DT.pack name
                <> "="
                <> maybe
                    (quoteWord value)
                    renderPosixEnvOverrideSpec
                    maybeOverrideSpec
        renderWindowsEnvOverride (name, value) =
            "set " <> cmdExeSetAssignmentQuote (name <> "=" <> value)
        zipOverrideSpecs overrides maybeOverrideSpecs =
            case maybeOverrideSpecs of
                Just overrideSpecs
                    | length overrideSpecs == length overrides ->
                        zip overrides $ map Just overrideSpecs
                _ ->
                    map (, Nothing) overrides
        renderPosixEnvOverrideSpec (CompilerEnvOverrideSpec spans) =
            let wordChars = shellWordCharsFromSpans spans
             in maybe
                    (renderPosixShellWordChars wordChars)
                    (renderUniformPosixShellWordChars wordChars)
                    (uniformShellWordContext wordChars)
        uniformShellWordContext [] = Nothing
        uniformShellWordContext (firstChar:remainingChars)
            | all ((== shellWordCharContext firstChar) . shellWordCharContext) remainingChars =
                Just $ shellWordCharContext firstChar
            | otherwise =
                Nothing
        renderUniformPosixShellWordChars wordChars context =
            case context of
                PosixShellLiteralContext ->
                    shellQuote wordText
                PosixShellDoubleQuotedContext ->
                    "\"" <> DT.pack (renderPosixDoubleQuotedExpandableText wordText) <> "\""
                PosixShellUnquotedContext ->
                    renderPosixShellWordChars wordChars
            where
                wordText = map shellWordCharText wordChars
        renderPosixDoubleQuotedExpandableText [] = []
        renderPosixDoubleQuotedExpandableText ('$':xs) =
            case parsePosixParameterExpansion xs of
                Just (parameterExpansion, remaining) ->
                    renderPosixParameterExpansion parameterExpansion
                        <> renderPosixDoubleQuotedExpandableText remaining
                Nothing ->
                    "\\$" <> renderPosixDoubleQuotedExpandableText xs
        renderPosixDoubleQuotedExpandableText (x:xs) =
            escapePosixDoubleQuotedChar x <> renderPosixDoubleQuotedExpandableText xs
        renderPosixShellWordChars [] = shellQuote ""
        renderPosixShellWordChars chars =
            DT.pack $ renderNonEmptyPosixShellWordChars chars
        renderPosixParameterExpansionWord [] = []
        renderPosixParameterExpansionWord chars =
            renderNonEmptyPosixShellWordChars chars
        renderNonEmptyPosixShellWordChars [] = []
        renderNonEmptyPosixShellWordChars chars@(currentChar:_)
            | shellWordCharContext currentChar == PosixShellLiteralContext =
                let (segment, remainingChars) =
                        span
                            ((== PosixShellLiteralContext) . shellWordCharContext)
                            chars
                 in DT.unpack (shellQuote (map shellWordCharText segment))
                        <> renderNonEmptyPosixShellWordChars remainingChars
            | shellWordCharContext currentChar == PosixShellDoubleQuotedContext =
                "\"" <> renderDoubleQuotedShellWordChars chars
            | otherwise =
                renderUnquotedShellWordChars chars
        renderDoubleQuotedShellWordChars [] = "\""
        renderDoubleQuotedShellWordChars chars@(currentChar:remainingChars)
            | shellWordCharContext currentChar /= PosixShellDoubleQuotedContext =
                "\"" <> renderNonEmptyPosixShellWordChars chars
            | shellWordCharText currentChar == '$' =
                case
                    parsePosixParameterExpansionInShellWord
                        PosixShellDoubleQuotedContext
                        remainingChars
                of
                    Just (parameterExpansion, trailingChars) ->
                        renderPosixParameterExpansion parameterExpansion
                            <> renderDoubleQuotedShellWordChars trailingChars
                    Nothing ->
                        "\\$" <> renderDoubleQuotedShellWordChars remainingChars
            | shellWordCharText currentChar == '`' =
                "\\`" <> renderDoubleQuotedShellWordChars remainingChars
            | otherwise =
                escapePosixDoubleQuotedChar (shellWordCharText currentChar)
                    <> renderDoubleQuotedShellWordChars remainingChars
        renderUnquotedShellWordChars [] = []
        renderUnquotedShellWordChars chars@(currentChar:remainingChars)
            | shellWordCharContext currentChar /= PosixShellUnquotedContext =
                renderNonEmptyPosixShellWordChars chars
            | shellWordCharText currentChar == '$' =
                case
                    parsePosixParameterExpansionInShellWord
                        PosixShellUnquotedContext
                        remainingChars
                of
                    Just (parameterExpansion, trailingChars) ->
                        renderPosixParameterExpansion parameterExpansion
                            <> renderUnquotedShellWordChars trailingChars
                    Nothing ->
                        "\\$" <> renderUnquotedShellWordChars remainingChars
            | shellWordCharText currentChar == '`' =
                "\\`" <> renderUnquotedShellWordChars remainingChars
            | otherwise =
                escapePosixUnquotedChar (shellWordCharText currentChar)
                    <> renderUnquotedShellWordChars remainingChars
        escapePosixUnquotedChar c
            | isSpace c || c `elem` ['\\', '"', '\'', '#', ';', '&', '|', '<', '>', '*', '?', '[', ']', '(', ')', '{', '}'] =
                ['\\', c]
            | otherwise =
                [c]
        renderPosixParameterExpansion parameterExpansion =
            case parameterExpansion of
                PosixSimpleParameterExpansion _ name ->
                    '$' : name
                PosixBracedParameterExpansion _ name Nothing ->
                    "${" <> name <> "}"
                PosixBracedParameterExpansion _ name (Just (wordMode, word)) ->
                    "${"
                        <> name
                        <> renderPosixParameterExpansionWordMode wordMode
                        <> renderPosixParameterExpansionWord word
                        <> "}"
        renderPosixParameterExpansionWordMode wordMode =
            case wordMode of
                PosixParameterUseDefaultWord colonSensitive ->
                    bool "-" ":-" colonSensitive
                PosixParameterUseAlternativeWord colonSensitive ->
                    bool "+" ":+" colonSensitive
        escapePosixDoubleQuotedChar c
            | c == '\\' =
                "\\\\"
            | c == '"' =
                "\\\""
            | c == '$' =
                "\\$"
            | c == '`' =
                "\\`"
            | otherwise =
                [c]

shellQuote :: String -> DT.Text
shellQuote word = "'" <> DT.replace "'" "'\"'\"'" (DT.pack word) <> "'"

currentCheckoutRootFromDirectories :: [FilePath] -> IO (Maybe FilePath)
currentCheckoutRootFromDirectories =
    firstResolved . map findRepoRootDirectory

resolveCheckoutRootAndCompiler :: IO (Maybe FilePath, Maybe FilePath)
resolveCheckoutRootAndCompiler = do
    maybeCurrentExecutable <- catchIOError
        (Just <$> getExecutablePath)
        (const $ pure Nothing)
    maybeCurrentDirectory <- catchIOError
        (Just <$> getCurrentDirectory)
        (const $ pure Nothing)
    case maybeCurrentExecutable of
        Just currentExecutable ->
            resolveCheckoutRootAndCompilerFromDirectories
                currentExecutable
                (takeDirectory currentExecutable : catMaybes [maybeCurrentDirectory])
        _ -> do
            maybeRepoRoot <- currentCheckoutRootFromDirectories $ catMaybes [maybeCurrentDirectory]
            pure (maybeRepoRoot, Nothing)

resolveCheckoutRootAndCompilerFromDirectories :: FilePath -> [FilePath] -> IO (Maybe FilePath, Maybe FilePath)
resolveCheckoutRootAndCompilerFromDirectories _ [] =
    pure (Nothing, Nothing)
resolveCheckoutRootAndCompilerFromDirectories currentExecutable (executableDirectory : fallbackDirectories) = do
    maybeExecutableRepoRoot <- findRepoRootDirectory executableDirectory
    case maybeExecutableRepoRoot of
        Just repoRoot -> do
            maybeRepoBuiltCompilerPath <-
                findFreshRepoBuiltExecutableNear repoRoot currentExecutable htccExecutableName
            pure (Just repoRoot, maybeRepoBuiltCompilerPath)
        Nothing -> do
            maybeFallbackRepoRoot <- currentCheckoutRootFromDirectories fallbackDirectories
            maybeBuildTreeRepoRoot <- case maybeFallbackRepoRoot of
                Just _ ->
                    pure Nothing
                Nothing ->
                    buildTreeCheckoutRootFromExecutable currentExecutable
            let maybeRepoRoot =
                    maybeFallbackRepoRoot <|> maybeBuildTreeRepoRoot
            maybeRepoBuiltCompilerPath <- case maybeRepoRoot of
                Just repoRoot -> do
                    buildTreeMatchesCheckout <-
                        buildTreeReferencesRepoRoot repoRoot currentExecutable
                    if buildTreeMatchesCheckout
                        then
                            findFreshRepoBuiltExecutableNear
                                repoRoot
                                currentExecutable
                                htccExecutableName
                        else pure Nothing
                Nothing ->
                    pure Nothing
            pure (maybeRepoRoot, maybeRepoBuiltCompilerPath)

buildTreeCheckoutRootFromExecutable :: FilePath -> IO (Maybe FilePath)
buildTreeCheckoutRootFromExecutable currentExecutable =
    firstResolved $
        map
            buildTreeCheckoutRootFromSetupConfig
            (buildTreeSetupConfigPaths currentExecutable)
    where
        buildTreeCheckoutRootFromSetupConfig setupConfigPath =
            catchIOError
                (do
                    exists <- doesFileExist setupConfigPath
                    if not exists
                        then pure Nothing
                        else do
                            contents <- BC.unpack <$> B.readFile setupConfigPath
                            currentCheckoutRootFromDirectories $
                                setupConfigReferencedPaths contents
                )
                (const $ pure Nothing)

buildTreeReferencesRepoRoot :: FilePath -> FilePath -> IO Bool
buildTreeReferencesRepoRoot repoRoot currentExecutable =
    go (buildTreeSetupConfigPaths currentExecutable)
    where
        repoRootNeedles =
            [repoRoot, repoRoot <> [pathSeparator] <> "."]

        go [] =
            pure False
        go (setupConfigPath : remainingSetupConfigPaths) = do
            matches <- setupConfigReferencesRepoRoot setupConfigPath
            if matches
                then pure True
                else go remainingSetupConfigPaths

        setupConfigReferencesRepoRoot setupConfigPath =
            catchIOError
                (do
                    exists <- doesFileExist setupConfigPath
                    if not exists
                        then pure False
                        else do
                            contents <- B.readFile setupConfigPath
                            pure $ any (setupConfigContentsReferenceRepoRoot $ BC.unpack contents) repoRootNeedles
                )
                (const $ pure False)

        setupConfigContentsReferenceRepoRoot contents repoRootNeedle =
            goContents Nothing contents
            where
                repoRootNeedleLength = length repoRootNeedle

                goContents _ [] =
                    False
                goContents maybePreviousChar remainingContents@(currentChar : nextChars)
                    | repoRootNeedle `isPrefixOf` remainingContents
                    , isRepoRootMatchBoundary maybePreviousChar
                    , isRepoRootMatchBoundary (nextRepoRootBoundaryChar remainingContents) =
                        True
                    | otherwise =
                        goContents (Just currentChar) nextChars

                nextRepoRootBoundaryChar remainingContents =
                    case drop repoRootNeedleLength remainingContents of
                        nextChar : _ -> Just nextChar
                        []           -> Nothing

        isRepoRootMatchBoundary =
            maybe True (not . isRepoRootPathContinuationChar)

        isRepoRootPathContinuationChar c =
            isAlphaNum c
                || c == '.'
                || c == '_'
                || c == '-'
                || c == '+'
                || c == '/'
                || c == '\\'
                || c == ':'

buildTreeSetupConfigPaths :: FilePath -> [FilePath]
buildTreeSetupConfigPaths currentExecutable =
    [ ancestorDir </> "setup-config"
    | ancestorDir <- allAncestorDirectories $ takeDirectory currentExecutable
    ]

setupConfigReferencedPaths :: String -> [FilePath]
setupConfigReferencedPaths =
    map normalizeSetupConfigReferencedPath
        . filter looksLikeSetupConfigReferencedPath
        . collectReferencedPaths
    where
        collectReferencedPaths [] = []
        collectReferencedPaths remainingContents =
            case setupConfigReferencedPathPrefix remainingContents of
                Just path ->
                    path : collectReferencedPaths (drop (length path) remainingContents)
                Nothing ->
                    collectReferencedPaths $ tail remainingContents

        setupConfigReferencedPathPrefix remainingContents
            | startsUnixAbsolutePath remainingContents =
                Just $ takeWhile isSetupConfigReferencedPathChar remainingContents
            | startsWindowsAbsolutePath remainingContents =
                Just $ takeWhile isSetupConfigReferencedPathChar remainingContents
            | otherwise =
                Nothing

        startsUnixAbsolutePath ('/':_) =
            True
        startsUnixAbsolutePath _ =
            False

        startsWindowsAbsolutePath (driveLetter : ':' : pathSeparatorChar : _)
            | isAlpha driveLetter =
                pathSeparatorChar == '/' || pathSeparatorChar == '\\'
        startsWindowsAbsolutePath _ =
            False

        isSetupConfigReferencedPathChar c =
            not (isControl c)
                && c /= '"'
                && c /= '\''
                && c /= '('
                && c /= ')'
                && c /= '['
                && c /= ']'
                && c /= '{'
                && c /= '}'
                && c /= '<'
                && c /= '>'
                && c /= ','
                && c /= ';'

        looksLikeSetupConfigReferencedPath path =
            any (`elem` path) ['/', '\\']

        normalizeSetupConfigReferencedPath path
            | "/." `isSuffixOf` path || "\\." `isSuffixOf` path =
                takeDirectory path
            | otherwise =
                path

preferredRepoBuiltCompilerPath :: Maybe FilePath -> Maybe FilePath -> IO (Maybe FilePath)
preferredRepoBuiltCompilerPath _ Nothing = pure Nothing
preferredRepoBuiltCompilerPath Nothing _ =
    pure Nothing
preferredRepoBuiltCompilerPath (Just repoRoot) (Just repoBuiltCompilerPath) = do
    isFresh <- repoBuiltCompilerIsFresh repoRoot repoBuiltCompilerPath
    pure $
        if isFresh
            then Just repoBuiltCompilerPath
            else Nothing

repoBuiltCompilerIsFresh :: FilePath -> FilePath -> IO Bool
repoBuiltCompilerIsFresh repoRoot repoBuiltCompilerPath = do
    maybeBuiltModificationTime <- safeGetModificationTime repoBuiltCompilerPath
    maybeLatestInputModificationTime <- latestCompilerInputModificationTime repoRoot
    pure $ case (maybeBuiltModificationTime, maybeLatestInputModificationTime) of
        (Just builtModificationTime, Just latestInputModificationTime) ->
            builtModificationTime >= latestInputModificationTime
        (Just _, Nothing) ->
            True
        _ ->
            False

latestCompilerInputModificationTime :: FilePath -> IO (Maybe UTCTime)
latestCompilerInputModificationTime =
    computeLatestCompilerInputModificationTime

computeLatestCompilerInputModificationTime :: FilePath -> IO (Maybe UTCTime)
computeLatestCompilerInputModificationTime repoRoot = do
    relevantFiles <- compilerInputFiles repoRoot
    modificationTimes <- catMaybes <$> mapM safeGetModificationTime relevantFiles
    pure $ case modificationTimes of
        [] ->
            Nothing
        _ ->
            Just $ maximum modificationTimes

compilerInputFiles :: FilePath -> IO [FilePath]
compilerInputFiles repoRoot = do
    sourceFiles <- concatMap (filter isCompilerSourceFile) <$> mapM collectExistingFiles compilerInputRoots
    configFiles <- filterM doesFileExist compilerInputConfigFiles
    pure $ sourceFiles <> configFiles
    where
        compilerInputRoots =
            map (repoRoot </>) ["app", "src"]

        compilerInputConfigFiles =
            map
                (repoRoot </>)
                [ "Setup.hs"
                , "cabal.project"
                , "cabal.project.local"
                , "cabal.project.freeze"
                , "htcc.cabal"
                , "package.yaml"
                , "stack.yaml"
                ]

        isCompilerSourceFile path =
            takeExtension path `elem`
                [ ".hs"
                , ".hs-boot"
                , ".hsc"
                , ".lhs"
                ]

collectExistingFiles :: FilePath -> IO [FilePath]
collectExistingFiles path =
    catchIOError go (const $ pure [])
    where
        go = do
            isDirectory <- doesDirectoryExist path
            if isDirectory
                then do
                    childEntries <- listDirectory path
                    concat <$> mapM (collectExistingFiles . (path </>)) childEntries
                else do
                    isFile <- doesFileExist path
                    pure [path | isFile]

safeGetModificationTime :: FilePath -> IO (Maybe UTCTime)
safeGetModificationTime path =
    catchIOError
        (Just <$> getModificationTime path)
        (const $ pure Nothing)

absolutizeCompilerExecutable :: FilePath -> IO FilePath
absolutizeCompilerExecutable executablePath
    | hasExplicitPath executablePath = makeAbsolute executablePath
    | otherwise = pure executablePath

shellWordsWithContextForHost :: String -> String -> Either String [ParsedShellWord]
shellWordsWithContextForHost hostOs commandLine =
    either (Left . show) Right $ Parsec.parse shellParser "<compiler>" commandLine
    where
        shellParser = skipSpaces *> Parsec.sepEndBy word spaces <* Parsec.eof
        skipSpaces = Parsec.skipMany $ Parsec.satisfy isSpace
        spaces = Parsec.skipMany1 $ Parsec.satisfy isSpace
        word = buildParsedShellWord <$> Parsec.many1 chunk
        chunk
            | hostOs == "mingw32" =
                Parsec.choice [doubleQuoted, caretEscaped, bare]
            | otherwise =
                Parsec.choice [singleQuoted, doubleQuoted, escaped, bare]
        singleQuoted = do
            text <- Parsec.char '\'' *> Parsec.manyTill Parsec.anyChar (Parsec.char '\'')
            pure (True, [literalShellWordSpan text])
        doubleQuoted =
            fmap ((,) True . coalesceShellWordSpans . concat) $
                Parsec.char '"' *> Parsec.manyTill doubleChunk (Parsec.char '"')
        doubleChunk
            | hostOs == "mingw32" =
                Parsec.choice
                    [ doubleEscaped
                    , doublePercentEscaped
                    , doublePercentChar
                    , doubleCaretEscaped
                    , doubleBare
                    ]
            | otherwise =
                Parsec.choice [doubleEscaped, doubleBare]
        doubleBare =
            pure . doubleQuotedExpandableShellWordSpan
                <$> Parsec.many1 (Parsec.satisfy isDoubleBareChar)
        doubleEscaped = do
            _ <- Parsec.char '\\'
            c <- Parsec.anyChar
            pure . pure . literalShellWordSpan $ case c of
                '\\' -> "\\"
                '"'  -> "\""
                '`'  -> "`"
                '$'  -> "$"
                '\n' -> ""
                _    -> ['\\', c]
        doublePercentEscaped = do
            _ <- Parsec.try $ Parsec.string "%%"
            pure [literalShellWordSpan "%"]
        doublePercentChar = do
            _ <- Parsec.char '%'
            pure [doubleQuotedExpandableShellWordSpan "%"]
        doubleCaretEscaped = do
            _ <- Parsec.char '^'
            c <- Parsec.anyChar
            pure [literalShellWordSpan [c]]
        escaped = do
            _ <- Parsec.char '\\'
            c <- Parsec.anyChar
            pure (False, [literalShellWordSpan $ case c of
                '\n' -> ""
                _    -> [c]
                ])
        caretEscaped = do
            _ <- Parsec.char '^'
            c <- Parsec.anyChar
            pure (False, [literalShellWordSpan [c]])
        bare = do
            text <- Parsec.many1 (Parsec.satisfy isBareNonBackslashChar)
            pure (False, [unquotedExpandableShellWordSpan text])

        buildParsedShellWord chunks =
            ParsedShellWord
                { parsedShellWordSpans =
                    coalesceShellWordSpans $ concatMap snd chunks
                , parsedShellWordPreservesEmptyField =
                    any fst chunks
                }

        isBareNonBackslashChar c =
            not (isSpace c)
                && c /= '"'
                && (hostOs /= "mingw32" || c /= '^')
                && (hostOs == "mingw32" || c /= '\'')
                && (hostOs == "mingw32" || c /= '\\')

        isDoubleBareChar c =
            c /= '"'
                && c /= '\\'
                && (hostOs /= "mingw32" || c /= '%')
                && (hostOs /= "mingw32" || c /= '^')

shellWordText :: ParsedShellWord -> String
shellWordText =
    concatMap shellWordSpanText . parsedShellWordSpans

literalShellWordSpan :: String -> ShellWordSpan
literalShellWordSpan text =
    ShellWordSpan
        { shellWordSpanText = text
        , shellWordSpanAllowsPosixExpansion = False
        , shellWordSpanAllowsPosixFieldSplitting = False
        }

doubleQuotedExpandableShellWordSpan :: String -> ShellWordSpan
doubleQuotedExpandableShellWordSpan text =
    ShellWordSpan
        { shellWordSpanText = text
        , shellWordSpanAllowsPosixExpansion = True
        , shellWordSpanAllowsPosixFieldSplitting = False
        }

unquotedExpandableShellWordSpan :: String -> ShellWordSpan
unquotedExpandableShellWordSpan text =
    ShellWordSpan
        { shellWordSpanText = text
        , shellWordSpanAllowsPosixExpansion = True
        , shellWordSpanAllowsPosixFieldSplitting = True
        }

coalesceShellWordSpans :: [ShellWordSpan] -> [ShellWordSpan]
coalesceShellWordSpans =
    foldr
        (\currentSpan accumulatedSpans ->
            case accumulatedSpans of
                nextSpan : remainingSpans
                    | shellWordSpanAllowsPosixExpansion currentSpan
                        == shellWordSpanAllowsPosixExpansion nextSpan
                        && shellWordSpanAllowsPosixFieldSplitting currentSpan
                        == shellWordSpanAllowsPosixFieldSplitting nextSpan ->
                        nextSpan
                            { shellWordSpanText =
                                shellWordSpanText currentSpan <> shellWordSpanText nextSpan
                            }
                            : remainingSpans
                _ ->
                    currentSpan : accumulatedSpans
        )
        []

dropShellWordSpanChars :: Int -> [ShellWordSpan] -> [ShellWordSpan]
dropShellWordSpanChars charsToDrop spans
    | charsToDrop <= 0 = spans
    | otherwise =
        case spans of
            [] ->
                []
            currentSpan : remainingSpans ->
                let spanText = shellWordSpanText currentSpan
                    spanLength = length spanText
                 in if charsToDrop < spanLength
                        then
                            currentSpan
                                { shellWordSpanText = drop charsToDrop spanText
                                }
                                : remainingSpans
                        else
                            dropShellWordSpanChars (charsToDrop - spanLength) remainingSpans

htccExecutableName :: FilePath
htccExecutableName
    | os == "mingw32" = "htcc.exe"
    | otherwise = "htcc"

findRepoBuiltExecutableNear :: FilePath -> FilePath -> IO (Maybe FilePath)
findRepoBuiltExecutableNear currentExecutable executableName = do
    -- Keep auto-selected test invocations pinned to this checkout when
    -- the test binary lives under the repo, but still reuse sibling
    -- build-tool-depends artifacts when Cabal places the test executable
    -- in an out-of-tree build directory. Probe each candidate before
    -- selecting it so broken or host-incompatible build artifacts do not
    -- shadow the historical stack launcher fallback.
    maybeRepoRoot <- findRepoRootDirectory $ takeDirectory currentExecutable
    findRepoBuiltExecutableNearWithPredicate
        maybeRepoRoot
        currentExecutable
        executableName
        (const $ pure True)

findFreshRepoBuiltExecutableNear :: FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
findFreshRepoBuiltExecutableNear repoRoot currentExecutable executableName =
    findRepoBuiltExecutableNearWithPredicate
        (Just repoRoot)
        currentExecutable
        executableName
        (repoBuiltCompilerIsFresh repoRoot)

findRepoBuiltExecutableNearWithPredicate ::
       Maybe FilePath
    -> FilePath
    -> FilePath
    -> (FilePath -> IO Bool)
    -> IO (Maybe FilePath)
findRepoBuiltExecutableNearWithPredicate maybeRepoRoot currentExecutable executableName isPreferredCandidate =
    firstResolved
        [ preferredLocalExecutablePath candidatePath
        | candidatePath <-
            preferredBuildArtifactCandidatePaths maybeRepoRoot currentExecutable executableName
                <> repoRootFallbackCandidatePaths maybeRepoRoot executableName
        ]
    where
        preferredLocalExecutablePath candidatePath = do
            maybeExecutablePath <- runnableLocalExecutablePath candidatePath
            case maybeExecutablePath of
                Just executablePath -> do
                    isPreferred <- isPreferredCandidate executablePath
                    pure $
                        if isPreferred
                            then Just executablePath
                            else Nothing
                Nothing ->
                    pure Nothing

preferredBuildArtifactCandidatePaths :: Maybe FilePath -> FilePath -> FilePath -> [FilePath]
preferredBuildArtifactCandidatePaths maybeRepoRoot currentExecutable executableName =
    [ ancestorDir </> relativePath
    | ancestorDir <- preferredBuildArtifactBaseDirectories maybeRepoRoot currentExecutable
    , relativePath <- preferredBuildArtifactRelativePaths maybeRepoRoot currentExecutable executableName
    ]

preferredBuildArtifactBaseDirectories :: Maybe FilePath -> FilePath -> [FilePath]
preferredBuildArtifactBaseDirectories maybeRepoRoot currentExecutable =
    case maybeRepoRoot of
        Just repoRoot ->
            let ancestorDirs = ancestorDirectoriesUntil repoRoot $ takeDirectory currentExecutable
             in if null ancestorDirs
                    then outOfTreeBuildAncestorDirectories $ takeDirectory currentExecutable
                    else ancestorDirs
        Nothing ->
            outOfTreeBuildAncestorDirectories $ takeDirectory currentExecutable

repoRootFallbackCandidatePaths :: Maybe FilePath -> FilePath -> [FilePath]
repoRootFallbackCandidatePaths maybeRepoRoot executableName =
    maybe [] (\repoRoot -> map (repoRoot </>) $ repoRootFallbackRelativePaths executableName) maybeRepoRoot

preferredBuildArtifactRelativePaths :: Maybe FilePath -> FilePath -> FilePath -> [FilePath]
preferredBuildArtifactRelativePaths maybeRepoRoot currentExecutable executableName =
    preferredBuildArtifactPaths
    where
        preferredBuildArtifactPaths
            | prefersNooptBuildArtifact maybeRepoRoot currentExecutable =
                [ "x" </> "htcc" </> "noopt" </> "build" </> "htcc" </> executableName
                , "x" </> "htcc" </> "build" </> "htcc" </> executableName
                , "noopt" </> "build" </> "htcc" </> executableName
                , "build" </> "htcc" </> executableName
                ]
            | otherwise =
                [ "x" </> "htcc" </> "build" </> "htcc" </> executableName
                , "x" </> "htcc" </> "noopt" </> "build" </> "htcc" </> executableName
                , "build" </> "htcc" </> executableName
                , "noopt" </> "build" </> "htcc" </> executableName
                ]

repoRootFallbackRelativePaths :: FilePath -> [FilePath]
repoRootFallbackRelativePaths executableName =
    [ "htcc" </> executableName
    , "build" </> executableName
    , executableName
    ]

prefersNooptBuildArtifact :: Maybe FilePath -> FilePath -> Bool
prefersNooptBuildArtifact maybeRepoRoot =
    any isNooptBuildFlavor . tails . splitDirectories . maybe id makeRelative maybeRepoRoot
    where
        isNooptBuildFlavor (targetKind : targetName : "noopt" : "build" : buildTargetName : _)
            | targetKind `elem` ["t", "x"] && targetName == buildTargetName = True
        isNooptBuildFlavor _ = False

outOfTreeBuildAncestorDirectories :: FilePath -> [FilePath]
outOfTreeBuildAncestorDirectories currentDirectory =
    filter (matchesOutOfTreeTestBuild currentDirectory) $
        allAncestorDirectories currentDirectory

matchesOutOfTreeTestBuild :: FilePath -> FilePath -> Bool
matchesOutOfTreeTestBuild currentDirectory candidateRoot =
    let relativeComponents = splitDirectories $ makeRelative candidateRoot currentDirectory
     in relativeComponents `elem`
            [ ["t", "htcc-test", "build", "htcc-test"]
            , ["t", "htcc-test", "noopt", "build", "htcc-test"]
            , ["build", "htcc-test"]
            ]

findRepoRootDirectory :: FilePath -> IO (Maybe FilePath)
findRepoRootDirectory path = do
    isRepoRoot <- doesFileExist $ path </> repoRootPackageMarkerFile
    if isRepoRoot
        then pure $ Just path
        else
            if parentPath == path
                then pure Nothing
                else findRepoRootDirectory parentPath
    where
        parentPath = takeDirectory path

ancestorDirectoriesUntil :: FilePath -> FilePath -> [FilePath]
ancestorDirectoriesUntil repoRoot path
    | path == repoRoot = [path]
    | parentPath == path = []
    | otherwise = path : ancestorDirectoriesUntil repoRoot parentPath
    where
        parentPath = takeDirectory path

allAncestorDirectories :: FilePath -> [FilePath]
allAncestorDirectories path
    | parentPath == path = [path]
    | otherwise = path : allAncestorDirectories parentPath
    where
        parentPath = takeDirectory path

repoRootPackageMarkerFile :: FilePath
repoRootPackageMarkerFile = "htcc.cabal"

hasExplicitPath :: FilePath -> Bool
hasExplicitPath = any (`elem` ['/', '\\'])

normalizeLocalExecutablePath :: FilePath -> FilePath
normalizeLocalExecutablePath cmd
    | hasExplicitPath cmd = cmd
    | otherwise = "./" <> cmd

localExecutablePath :: FilePath -> IO (Maybe FilePath)
localExecutablePath cmd = do
    isLocalFile <- doesFileExist cmd
    isLocalExec <- if isLocalFile then executable <$> getPermissions cmd else pure False
    pure $
        if isLocalExec
            then Just $ normalizeLocalExecutablePath cmd
            else Nothing

runnableLocalExecutablePath :: FilePath -> IO (Maybe FilePath)
runnableLocalExecutablePath cmd = do
    maybeExecutablePath <- localExecutablePath cmd
    case maybeExecutablePath of
        Just executablePath -> do
            isRunnable <- probeCompilerCommandAvailable $
                CompilerCommand
                    { compilerEnvOverrides = []
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = executablePath
                    , compilerArguments = []
                    }
            pure $
                if isRunnable
                    then Just executablePath
                    else Nothing
        Nothing ->
            pure Nothing

probeCompilerCommandAvailable :: CompilerCommand -> IO Bool
probeCompilerCommandAvailable compiler =
    catchIOError
        (withCompilerProbeSourceFile $ \sourcePath -> do
            (probeExitCode, stdoutOut, _) <- readCompilerProcessWithExitCode compiler [sourcePath]
            pure $
                probeExitCode == ExitSuccess
                    && compilerProbeOutputLooksValid stdoutOut
        )
        (const $ pure False)

probeCompilerShellCommandAvailable :: DT.Text -> IO Bool
probeCompilerShellCommandAvailable compilerCommand = do
    workingDir <- getCurrentDirectory
    probeCompilerShellCommandAvailableInDirectory workingDir compilerCommand

probeCompilerShellCommandAvailableInDirectory :: FilePath -> DT.Text -> IO Bool
probeCompilerShellCommandAvailableInDirectory workingDir compilerCommand =
    catchIOError
        (withCompilerProbeSourceFile $ \sourcePath -> do
            let commandLine =
                    DT.unpack $
                        compilerCommand <> " " <> shellQuoteForHost os sourcePath
            (probeExitCode, stdoutOut, _) <-
                readCreateProcessWithExitCode
                    (Process.shell commandLine)
                        { cwd = Just workingDir
                        }
                    ""
            pure $
                probeExitCode == ExitSuccess
                    && compilerProbeOutputLooksValid stdoutOut
        )
        (const $ pure False)

withCompilerProbeSourceFile :: (FilePath -> IO a) -> IO a
withCompilerProbeSourceFile action = do
    tempDir <- getTemporaryDirectory
    bracket
        (openTempFile tempDir "htcc-test-compiler-probe-.c")
        (\(path, handle) -> do
            catchIOError (hClose handle) (const $ pure ())
            catchIOError (removeFile path) (const $ pure ())
        )
        (\(path, handle) -> do
            hPutStr handle compilerProbeSource
            hClose handle
            action path
        )

compilerProbeSource :: String
compilerProbeSource = "int main(void) { return 0; }\n"

compilerProbeOutputLooksValid :: String -> Bool
compilerProbeOutputLooksValid stdoutOut =
    any (`isInfixOf` stdoutOut)
        [ "\nmain:"
        , "main:"
        , ".globl main"
        ]

firstResolved :: [IO (Maybe a)] -> IO (Maybe a)
firstResolved [] = pure Nothing
firstResolved (resolvePath : resolvePaths) = do
    resolved <- resolvePath
    maybe (firstResolved resolvePaths) (pure . Just) resolved

resolveCompilerCommand :: String -> IO CompilerCommand
resolveCompilerCommand =
    resolveCompilerCommandIn Nothing

resolveCompilerCommandIn :: Maybe FilePath -> String -> IO CompilerCommand
resolveCompilerCommandIn =
    resolveCompilerCommandInForHost os

resolveCompilerCommandInForHost :: String -> Maybe FilePath -> String -> IO CompilerCommand
resolveCompilerCommandInForHost hostOs maybeWorkingDir compiler = do
    parsedParts <- case shellWordsWithContextForHost hostOs compiler of
        Left parseErr -> ioError . userError $
            "failed to parse compiler command " <> show compiler <> ": " <> parseErr
        Right [] -> ioError . userError $
            "empty compiler command: " <> show compiler
        Right xs -> pure xs
    let (envAssignmentWords, initialCompilerWords) =
            span (isEnvironmentAssignmentWord . shellWordText) parsedParts
    let (initialEnvOverrides, initialEnvOverrideSpecs) =
            unzip $ map splitEnvironmentAssignment envAssignmentWords
    baseEnvironment <- baseProcessEnvironmentForHost hostOs maybeWorkingDir
    let expandedEnvOverrides =
            environmentFromListForHost hostOs $
                expandEnvironmentOverridesWithBaseEnvironmentForHost
                    hostOs
                    baseEnvironment
                    initialEnvOverrides
                    (Just initialEnvOverrideSpecs)
        compilerWordExpansionEnvironment
            | hostOs == "mingw32" =
                Map.union expandedEnvOverrides baseEnvironment
            | otherwise =
                baseEnvironment
        compilerResolutionEnvironment =
            Map.union expandedEnvOverrides baseEnvironment
        compilerParts =
            concatMap
                (expandParsedShellWordIntoArgumentsForHost
                    hostOs
                    compilerWordExpansionEnvironment
                )
                initialCompilerWords
        envOverrides = initialEnvOverrides
        envOverrideSpecs = initialEnvOverrideSpecs
    when (null compilerParts) . ioError . userError $
        "empty compiler command: " <> show compiler
    resolvedPrefix <-
        findExecutablePrefix
            maybeWorkingDir
            compilerResolutionEnvironment
            (hasExplicitSearchPathOverride envOverrides)
            compilerParts
    pure $
        case resolvedPrefix of
            Just (compilerLen, resolvedCompiler) ->
                CompilerCommand
                    { compilerEnvOverrides = envOverrides
                    , compilerEnvOverrideSpecs = Just envOverrideSpecs
                    , compilerExecutable = resolvedCompiler
                    , compilerArguments = drop compilerLen compilerParts
                    }
            Nothing ->
                CompilerCommand
                    { compilerEnvOverrides = envOverrides
                    , compilerEnvOverrideSpecs = Just envOverrideSpecs
                    , compilerExecutable = head compilerParts
                    , compilerArguments = tail compilerParts
                    }
    where
        isEnvironmentAssignmentWord word = case span (/= '=') word of
            ([], _) -> False
            (name, '=':_) ->
                startsLikeIdentifier (head name) && all (\c -> isAlphaNum c || c == '_') name
            _ -> False

        splitEnvironmentAssignment word =
            case span (/= '=') (shellWordText word) of
                (name, '=':value) ->
                    ( (name, value)
                    , CompilerEnvOverrideSpec $
                        dropShellWordSpanChars (length name + 1) (parsedShellWordSpans word)
                    )
                _ ->
                    error "internal compiler error"

        hasExplicitSearchPathOverride =
            any
                ((== environmentNameKeyForHost hostOs "PATH") . environmentNameKeyForHost hostOs . fst)

        findExecutablePrefix _ _ _ [] = pure Nothing
        findExecutablePrefix maybeWorkingDir' envOverrides' explicitSearchPathOverride (cmd:_) = do
            resolved <-
                resolveExecutableCommand
                    maybeWorkingDir'
                    envOverrides'
                    explicitSearchPathOverride
                    cmd
            pure $ fmap (1,) resolved

        resolveExecutableCommand maybeWorkingDir' envOverrides' explicitSearchPathOverride cmd =
            if hasExplicitPath cmd
                then resolveLocalExecutablePath maybeWorkingDir' cmd
                else
                    findExecutableInSearchPath
                        maybeWorkingDir'
                        envOverrides'
                        explicitSearchPathOverride
                        cmd

        findExecutableInSearchPath maybeWorkingDir' envOverrides' explicitSearchPathOverride cmd = do
            pathValue <- maybe
                (fromMaybe "" <$> lookupEnv "PATH")
                pure
                (environmentLookupForHost hostOs "PATH" envOverrides')
            maybeResolvedFromPath <- firstResolved $
                map (resolveLocalExecutablePath maybeWorkingDir' . searchPathCommand cmd) $
                    searchPathEntries pathValue
            case maybeResolvedFromPath of
                Just resolvedFromPath ->
                    pure $ Just resolvedFromPath
                Nothing
                    | explicitSearchPathOverride ->
                        pure Nothing
                    | otherwise ->
                        resolveLocalExecutablePath maybeWorkingDir' cmd

        searchPathCommand cmd ""  = cmd
        searchPathCommand cmd dir = dir </> cmd

        searchPathEntries pathValue = case break (== searchPathSeparator) pathValue of
            (dir, [])       -> [dir]
            (dir, _:remain) -> dir : searchPathEntries remain

        resolveLocalExecutablePath maybeWorkingDir' cmd = do
            let candidatePath =
                    normalise $
                        case maybeWorkingDir' of
                            Just workingDir
                                | not (isAbsolute cmd) ->
                                    workingDir </> cmd
                            _ ->
                                cmd
            isLocalFile <- doesFileExist candidatePath
            isLocalExec <- if isLocalFile then executable <$> getPermissions candidatePath else pure False
            pure $
                if isLocalExec
                    then Just $ normalizeResolvedLocalExecutablePath maybeWorkingDir' cmd candidatePath
                    else Nothing

        normalizeResolvedLocalExecutablePath maybeWorkingDir' cmd candidatePath
            | isJust maybeWorkingDir' && not (isAbsolute cmd) = candidatePath
            | hasExplicitPath cmd = cmd
            | otherwise = "./" <> cmd

compilerInvocationArgs :: CompilerCommand -> [String] -> [String]
compilerInvocationArgs compiler extraArgs =
    compilerArguments compiler <> extraArgs

expandEnvironmentOverrides
    :: Maybe FilePath
    -> [(String, String)]
    -> Maybe [CompilerEnvOverrideSpec]
    -> IO [(String, String)]
expandEnvironmentOverrides maybeWorkingDir overrides maybeOverrideSpecs = do
    baseEnvironment <- baseProcessEnvironment maybeWorkingDir
    pure $
        expandEnvironmentOverridesWithBaseEnvironment
            baseEnvironment
            overrides
            maybeOverrideSpecs

expandEnvironmentOverridesWithBaseEnvironment
    :: Map.Map String String
    -> [(String, String)]
    -> Maybe [CompilerEnvOverrideSpec]
    -> [(String, String)]
expandEnvironmentOverridesWithBaseEnvironment =
    expandEnvironmentOverridesWithBaseEnvironmentForHost os

expandEnvironmentOverridesWithBaseEnvironmentForHost
    :: String
    -> Map.Map String String
    -> [(String, String)]
    -> Maybe [CompilerEnvOverrideSpec]
    -> [(String, String)]
expandEnvironmentOverridesWithBaseEnvironmentForHost hostOs baseEnvironment overrides maybeOverrideSpecs =
    reverse . snd $
        foldl'
            expandOverride
            (baseEnvironment, [])
            (zipOverrideSpecs overrides maybeOverrideSpecs)
    where
        expandOverride (expansionEnvironment, expandedOverrides) ((name, value), maybeOverrideSpec) =
            let expandedValue = maybe
                    (expandEnvironmentValueForHost hostOs expansionEnvironment value)
                    (expandEnvironmentValueWithShellSpansForHost hostOs expansionEnvironment)
                    maybeOverrideSpec
             in ( environmentInsertForHost hostOs name expandedValue expansionEnvironment
                , (name, expandedValue) : expandedOverrides
                )

        zipOverrideSpecs overrides' maybeOverrideSpecs' =
            case maybeOverrideSpecs' of
                Just overrideSpecs
                    | length overrideSpecs == length overrides' ->
                        zip overrides' $ map Just overrideSpecs
                _ ->
                    map (, Nothing) overrides'

baseProcessEnvironment :: Maybe FilePath -> IO (Map.Map String String)
baseProcessEnvironment =
    baseProcessEnvironmentForHost os

baseProcessEnvironmentForHost :: String -> Maybe FilePath -> IO (Map.Map String String)
baseProcessEnvironmentForHost hostOs maybeWorkingDir = do
    baseEnvironment <- environmentFromListForHost hostOs <$> getEnvironment
    case maybeWorkingDir of
        Just workingDir ->
            pure $ environmentInsertForHost hostOs "PWD" workingDir baseEnvironment
        Nothing ->
            case environmentLookupForHost hostOs "PWD" baseEnvironment of
                Just _ ->
                    pure baseEnvironment
                Nothing -> do
                    workingDir <- getCurrentDirectory
                    pure $ environmentInsertForHost hostOs "PWD" workingDir baseEnvironment

environmentNameKeyForHost :: String -> String -> String
environmentNameKeyForHost hostOs name
    | hostOs == "mingw32" = map toLower name
    | otherwise = name

environmentFromList :: [(String, String)] -> Map.Map String String
environmentFromList =
    environmentFromListForHost os

environmentFromListForHost :: String -> [(String, String)] -> Map.Map String String
environmentFromListForHost hostOs =
    Map.fromList . map (first (environmentNameKeyForHost hostOs))

environmentInsertForHost :: String -> String -> String -> Map.Map String String -> Map.Map String String
environmentInsertForHost hostOs name =
    Map.insert (environmentNameKeyForHost hostOs name)

environmentLookup :: String -> Map.Map String String -> Maybe String
environmentLookup =
    environmentLookupForHost os

environmentLookupForHost :: String -> String -> Map.Map String String -> Maybe String
environmentLookupForHost hostOs name =
    Map.lookup (environmentNameKeyForHost hostOs name)

expandEnvironmentValueForHost :: String -> Map.Map String String -> String -> String
expandEnvironmentValueForHost hostOs expansionEnvironment =
    expandForHost
    where
        expandForHost
            | hostOs == "mingw32" =
                expandWindowsEnvironmentVariablesForHost hostOs expansionEnvironment '!'
                    . expandWindowsEnvironmentVariablesForHost hostOs expansionEnvironment '%'
            | otherwise =
                expandPosixEnvironmentVariables expansionEnvironment

expandParsedShellWordForHost :: String -> Map.Map String String -> ParsedShellWord -> String
expandParsedShellWordForHost hostOs expansionEnvironment =
    expandShellWordSpansForHost hostOs expansionEnvironment . parsedShellWordSpans

expandParsedShellWordIntoArgumentsForHost :: String -> Map.Map String String -> ParsedShellWord -> [String]
expandParsedShellWordIntoArgumentsForHost hostOs expansionEnvironment parsedWord
    | hostOs == "mingw32" =
        let expandedWord =
                expandParsedShellWordForHost hostOs expansionEnvironment parsedWord
            fields =
                if parsedShellWordAllowsWindowsRetokenization parsedWord
                    then
                        case shellWordsWithContextForHost hostOs expandedWord of
                            Right expandedWords ->
                                map shellWordText expandedWords
                            Left _ ->
                                [expandedWord]
                    else
                        [expandedWord]
         in if null fields && parsedShellWordPreservesEmptyField parsedWord
                then [""]
                else fields
    | otherwise =
        let fields =
                splitExpandedShellWord $
                    expandShellWordChars
                        PosixTildeExpansionForShellWord
                        expansionEnvironment
                        (shellWordCharsFromSpans $ parsedShellWordSpans parsedWord)
         in if null fields && parsedShellWordPreservesEmptyField parsedWord
                then [""]
                else fields

parsedShellWordAllowsWindowsRetokenization :: ParsedShellWord -> Bool
parsedShellWordAllowsWindowsRetokenization =
    all shellWordSpanAllowsPosixFieldSplitting . parsedShellWordSpans

expandShellWordSpansForHost :: String -> Map.Map String String -> [ShellWordSpan] -> String
expandShellWordSpansForHost hostOs expansionEnvironment spans
    | hostOs == "mingw32" =
        concatMap
            (expandWindowsShellWordSpanForHost hostOs expansionEnvironment)
            spans
    | otherwise =
        concatMap expandedShellWordFragmentText $
            expandShellWordChars
                PosixTildeExpansionForShellWord
                expansionEnvironment
                (shellWordCharsFromSpans spans)

expandWindowsShellWordSpanForHost :: String -> Map.Map String String -> ShellWordSpan -> String
expandWindowsShellWordSpanForHost hostOs expansionEnvironment wordSpan
    | shellWordSpanAllowsPosixExpansion wordSpan =
        expandEnvironmentValueForHost
            hostOs
            expansionEnvironment
            (shellWordSpanText wordSpan)
    | otherwise =
        shellWordSpanText wordSpan

expandEnvironmentValueWithShellSpansForHost
    :: String
    -> Map.Map String String
    -> CompilerEnvOverrideSpec
    -> String
expandEnvironmentValueWithShellSpansForHost hostOs expansionEnvironment overrideSpec
    | hostOs == "mingw32" =
        concatMap
            (expandWindowsShellWordSpanForHost hostOs expansionEnvironment)
            (compilerEnvOverrideSpans overrideSpec)
    | otherwise =
        concatMap expandedShellWordFragmentText $
            expandShellWordChars
                PosixTildeExpansionForAssignmentValue
                expansionEnvironment
                (shellWordCharsFromSpans $ compilerEnvOverrideSpans overrideSpec)

data ExpandedShellWordFragment = ExpandedShellWordFragment
    { expandedShellWordFragmentText                      :: String
    , expandedShellWordFragmentAllowsPosixFieldSplitting :: Bool
    }

data PosixTildeExpansionMode
    = PosixTildeExpansionForShellWord
    | PosixTildeExpansionForAssignmentValue
    deriving (Eq)

shellWordSpanContext :: ShellWordSpan -> PosixShellContext
shellWordSpanContext wordSpan
    | shellWordSpanAllowsPosixExpansion wordSpan =
        if shellWordSpanAllowsPosixFieldSplitting wordSpan
            then PosixShellUnquotedContext
            else PosixShellDoubleQuotedContext
    | otherwise =
        PosixShellLiteralContext

shellWordCharsFromSpans :: [ShellWordSpan] -> [ShellWordChar]
shellWordCharsFromSpans =
    concatMap $ \wordSpan ->
        shellWordCharsFromTextWithContext
            (shellWordSpanContext wordSpan)
            (shellWordSpanText wordSpan)

shellWordCharsFromTextWithContext :: PosixShellContext -> String -> [ShellWordChar]
shellWordCharsFromTextWithContext wordContext =
    map (\c -> ShellWordChar {shellWordCharText = c, shellWordCharContext = wordContext})

shellContextAllowsPosixExpansion :: PosixShellContext -> Bool
shellContextAllowsPosixExpansion context =
    context /= PosixShellLiteralContext

shellContextAllowsPosixFieldSplitting :: PosixShellContext -> Bool
shellContextAllowsPosixFieldSplitting context =
    context == PosixShellUnquotedContext

coalesceExpandedShellWordFragments :: [ExpandedShellWordFragment] -> [ExpandedShellWordFragment]
coalesceExpandedShellWordFragments =
    foldr
        (\currentFragment accumulatedFragments ->
            case accumulatedFragments of
                nextFragment : remainingFragments
                    | expandedShellWordFragmentAllowsPosixFieldSplitting currentFragment
                        == expandedShellWordFragmentAllowsPosixFieldSplitting nextFragment ->
                        nextFragment
                            { expandedShellWordFragmentText =
                                expandedShellWordFragmentText currentFragment
                                    <> expandedShellWordFragmentText nextFragment
                            }
                            : remainingFragments
                _ ->
                    currentFragment : accumulatedFragments
        )
        []

expandShellWordChars
    :: PosixTildeExpansionMode
    -> Map.Map String String
    -> [ShellWordChar]
    -> [ExpandedShellWordFragment]
expandShellWordChars tildeExpansionMode expansionEnvironment =
    coalesceExpandedShellWordFragments . go True
    where
        go _ [] = []
        go tildePrefixAllowed (currentChar:remainingChars)
            | Just tildeFragment <-
                expandPosixTildePrefix
                    tildeExpansionMode
                    expansionEnvironment
                    tildePrefixAllowed
                    currentChar
                    remainingChars =
                tildeFragment : go False remainingChars
            | shellWordCharText currentChar == '$'
                && shellContextAllowsPosixExpansion (shellWordCharContext currentChar) =
                case
                    parsePosixParameterExpansionInShellWord
                        (shellWordCharContext currentChar)
                        remainingChars
                of
                    Just (parameterExpansion, trailingChars) ->
                        expandPosixParameterExpansionInShellWord
                            tildeExpansionMode
                            expansionEnvironment
                            parameterExpansion
                            <> go False trailingChars
                    Nothing ->
                        literalFragment currentChar
                            : go (tildePrefixContinuesAfterChar tildeExpansionMode currentChar) remainingChars
            | otherwise =
                literalFragment currentChar
                    : go (tildePrefixContinuesAfterChar tildeExpansionMode currentChar) remainingChars

        literalFragment wordChar =
            ExpandedShellWordFragment
                { expandedShellWordFragmentText = [shellWordCharText wordChar]
                , expandedShellWordFragmentAllowsPosixFieldSplitting =
                    shellContextAllowsPosixFieldSplitting (shellWordCharContext wordChar)
                }

        expandPosixTildePrefix expansionMode expansionEnvironment' tildePrefixAllowed' wordChar trailingChars
            | not tildePrefixAllowed' = Nothing
            | shellWordCharContext wordChar /= PosixShellUnquotedContext = Nothing
            | shellWordCharText wordChar /= '~' = Nothing
            | not (posixTildePrefixTerminatedBy expansionMode trailingChars) = Nothing
            | otherwise =
                Just $
                    ExpandedShellWordFragment
                        { expandedShellWordFragmentText =
                            fromMaybe "~" $ environmentLookup "HOME" expansionEnvironment'
                        , expandedShellWordFragmentAllowsPosixFieldSplitting = False
                        }

        posixTildePrefixTerminatedBy _ [] = True
        posixTildePrefixTerminatedBy expansionMode (nextChar:_)
            | shellWordCharContext nextChar /= PosixShellUnquotedContext = False
            | shellWordCharText nextChar == '/' = True
            | otherwise =
                expansionMode == PosixTildeExpansionForAssignmentValue
                    && shellWordCharText nextChar == ':'

        tildePrefixContinuesAfterChar expansionMode wordChar =
            expansionMode == PosixTildeExpansionForAssignmentValue
                && shellWordCharContext wordChar == PosixShellUnquotedContext
                && shellWordCharText wordChar == ':'

splitExpandedShellWord :: [ExpandedShellWordFragment] -> [String]
splitExpandedShellWord =
    reverse . finalizeSplitState . foldl' splitFragment ([], [])
    where
        splitFragment splitState fragment =
            foldl'
                (splitCharacter $ expandedShellWordFragmentAllowsPosixFieldSplitting fragment)
                splitState
                (expandedShellWordFragmentText fragment)

        splitCharacter allowsFieldSplitting (completedFields, currentFieldReversed) c
            | allowsFieldSplitting && isSpace c =
                finalizeCurrentField (completedFields, currentFieldReversed)
            | otherwise =
                (completedFields, c : currentFieldReversed)

        finalizeSplitState =
            fst . finalizeCurrentField

        finalizeCurrentField (completedFields, currentFieldReversed)
            | null currentFieldReversed =
                (completedFields, [])
            | otherwise =
                (reverse currentFieldReversed : completedFields, [])

expandPosixEnvironmentVariables :: Map.Map String String -> String -> String
expandPosixEnvironmentVariables expansionEnvironment = go
    where
        go [] = []
        go ('$':xs) =
            case parsePosixParameterExpansion xs of
                Just (parameterExpansion, remaining) ->
                    expandPosixParameterExpansion expansionEnvironment parameterExpansion
                        <> go remaining
                Nothing ->
                    '$' : go xs
        go (x:xs) =
            x : go xs

data PosixParameterExpansionWordMode
    = PosixParameterUseDefaultWord Bool
    | PosixParameterUseAlternativeWord Bool

data PosixParameterExpansion
    = PosixSimpleParameterExpansion PosixShellContext String
    | PosixBracedParameterExpansion
        PosixShellContext
        String
        (Maybe (PosixParameterExpansionWordMode, [ShellWordChar]))

parsePosixParameterExpansionInShellWord
    :: PosixShellContext
    -> [ShellWordChar]
    -> Maybe (PosixParameterExpansion, [ShellWordChar])
parsePosixParameterExpansionInShellWord expansionContext chars =
    case chars of
        wordChar : remainingChars
            | shellWordCharText wordChar == '{' ->
                parseBracedPosixParameterExpansionInShellWord expansionContext remainingChars
            | isEnvironmentVariableName name ->
                Just
                    ( PosixSimpleParameterExpansion expansionContext name
                    , trailingChars
                    )
            where
                (nameSuffixChars, trailingChars) =
                    span (isEnvironmentVariableNameChar . shellWordCharText) remainingChars
                name =
                    shellWordCharText wordChar : map shellWordCharText nameSuffixChars
        _ ->
            Nothing

parseBracedPosixParameterExpansionInShellWord
    :: PosixShellContext
    -> [ShellWordChar]
    -> Maybe (PosixParameterExpansion, [ShellWordChar])
parseBracedPosixParameterExpansionInShellWord expansionContext chars = do
    (name, remainingChars) <- parseEnvironmentVariableNamePrefixInShellWord chars
    case remainingChars of
        wordChar : trailingChars
            | shellWordCharText wordChar == '}'
                && shellWordCharContext wordChar == expansionContext ->
                    Just
                        ( PosixBracedParameterExpansion expansionContext name Nothing
                        , trailingChars
                        )
        colonChar : operatorChar : trailingChars
            | shellWordCharText colonChar == ':'
                && shellWordCharContext colonChar == expansionContext
                && shellWordCharContext operatorChar == expansionContext
                && shellWordCharText operatorChar `elem` ['-', '+'] -> do
                    (wordChars, restChars) <-
                        takePosixParameterExpansionWordInShellWord expansionContext trailingChars
                    Just
                        ( PosixBracedParameterExpansion
                            expansionContext
                            name
                            ( Just
                                ( posixParameterExpansionWordMode
                                    True
                                    (shellWordCharText operatorChar)
                                , wordChars
                                )
                            )
                        , restChars
                        )
        operatorChar : trailingChars
            | shellWordCharContext operatorChar == expansionContext
                && shellWordCharText operatorChar `elem` ['-', '+'] -> do
                    (wordChars, restChars) <-
                        takePosixParameterExpansionWordInShellWord expansionContext trailingChars
                    Just
                        ( PosixBracedParameterExpansion
                            expansionContext
                            name
                            ( Just
                                ( posixParameterExpansionWordMode
                                    False
                                    (shellWordCharText operatorChar)
                                , wordChars
                                )
                            )
                        , restChars
                        )
        _ ->
            Nothing

parseEnvironmentVariableNamePrefixInShellWord
    :: [ShellWordChar]
    -> Maybe (String, [ShellWordChar])
parseEnvironmentVariableNamePrefixInShellWord chars =
    case chars of
        wordChar : remainingChars
            | isEnvironmentVariableName name ->
                Just (name, trailingChars)
            where
                (nameSuffixChars, trailingChars) =
                    span (isEnvironmentVariableNameChar . shellWordCharText) remainingChars
                name =
                    shellWordCharText wordChar : map shellWordCharText nameSuffixChars
        _ ->
            Nothing

takePosixParameterExpansionWordInShellWord
    :: PosixShellContext
    -> [ShellWordChar]
    -> Maybe ([ShellWordChar], [ShellWordChar])
takePosixParameterExpansionWordInShellWord expansionContext =
    go [] []
    where
        go _ _ [] =
            Nothing
        go nestedExpansionContexts accumulatedChars (currentChar:remainingChars)
            | Just (nestedContext, trailingChars) <-
                parseNestedExpansionStart currentChar remainingChars =
                let nextChar = head remainingChars
                 in go
                        (nestedContext : nestedExpansionContexts)
                        (nextChar : currentChar : accumulatedChars)
                        trailingChars
            | shellWordCharText currentChar == '}' =
                case nestedExpansionContexts of
                    nestedContext : remainingContexts
                        | shellWordCharContext currentChar == nestedContext ->
                            go remainingContexts (currentChar : accumulatedChars) remainingChars
                    []
                        | shellWordCharContext currentChar == expansionContext ->
                            Just (reverse accumulatedChars, remainingChars)
                    _ ->
                        go nestedExpansionContexts (currentChar : accumulatedChars) remainingChars
            | otherwise =
                go nestedExpansionContexts (currentChar : accumulatedChars) remainingChars

        parseNestedExpansionStart currentChar remainingChars =
            case remainingChars of
                nextChar : trailingChars
                    | shellWordCharText currentChar == '$'
                        && shellContextAllowsPosixExpansion (shellWordCharContext currentChar)
                        && shellWordCharContext nextChar == shellWordCharContext currentChar
                        && shellWordCharText nextChar == '{' ->
                            Just (shellWordCharContext currentChar, trailingChars)
                _ ->
                    Nothing

parsePosixParameterExpansion :: String -> Maybe (PosixParameterExpansion, String)
parsePosixParameterExpansion ('{':xs) =
    parseBracedPosixParameterExpansion xs
parsePosixParameterExpansion (x:xs)
    | isEnvironmentVariableName name =
        Just (PosixSimpleParameterExpansion PosixShellUnquotedContext name, remaining)
    where
        (nameSuffix, remaining) = span isEnvironmentVariableNameChar xs
        name = x : nameSuffix
parsePosixParameterExpansion _ =
    Nothing

parseBracedPosixParameterExpansion :: String -> Maybe (PosixParameterExpansion, String)
parseBracedPosixParameterExpansion xs = do
    (name, remaining) <- parseEnvironmentVariableNamePrefix xs
    case remaining of
        '}':rest ->
            Just
                ( PosixBracedParameterExpansion
                    PosixShellUnquotedContext
                    name
                    Nothing
                , rest
                )
        ':':op:rest
            | op `elem` ['-', '+'] -> do
                (word, trailing) <- takePosixParameterExpansionWord rest
                Just
                    ( PosixBracedParameterExpansion
                        PosixShellUnquotedContext
                        name
                        ( Just
                            ( posixParameterExpansionWordMode True op
                            , shellWordCharsFromTextWithContext PosixShellUnquotedContext word
                            )
                        )
                    , trailing
                    )
        op:rest
            | op `elem` ['-', '+'] -> do
                (word, trailing) <- takePosixParameterExpansionWord rest
                Just
                    ( PosixBracedParameterExpansion
                        PosixShellUnquotedContext
                        name
                        ( Just
                            ( posixParameterExpansionWordMode False op
                            , shellWordCharsFromTextWithContext PosixShellUnquotedContext word
                            )
                        )
                    , trailing
                    )
        _ ->
            Nothing

parseEnvironmentVariableNamePrefix :: String -> Maybe (String, String)
parseEnvironmentVariableNamePrefix (x:xs)
    | isEnvironmentVariableName name =
        Just (name, remaining)
    where
        (nameSuffix, remaining) = span isEnvironmentVariableNameChar xs
        name = x : nameSuffix
parseEnvironmentVariableNamePrefix _ =
    Nothing

takePosixParameterExpansionWord :: String -> Maybe (String, String)
takePosixParameterExpansionWord =
    go (0 :: Int) []
    where
        go _ _ [] =
            Nothing
        go nested acc ('$':'{':xs) =
            go (nested + 1) ('{' : '$' : acc) xs
        go 0 acc ('}':xs) =
            Just (reverse acc, xs)
        go nested acc ('}':xs) =
            go (nested - 1) ('}' : acc) xs
        go nested acc (x:xs) =
            go nested (x : acc) xs

posixParameterExpansionWordMode :: Bool -> Char -> PosixParameterExpansionWordMode
posixParameterExpansionWordMode colonSensitive operator =
    case operator of
        '-' ->
            PosixParameterUseDefaultWord colonSensitive
        '+' ->
            PosixParameterUseAlternativeWord colonSensitive
        _ ->
            error "internal compiler error"

expandPosixParameterExpansion :: Map.Map String String -> PosixParameterExpansion -> String
expandPosixParameterExpansion expansionEnvironment parameterExpansion =
    concatMap expandedShellWordFragmentText $
        expandPosixParameterExpansionInShellWord
            PosixTildeExpansionForShellWord
            expansionEnvironment
            parameterExpansion

expandPosixParameterExpansionInShellWord
    :: PosixTildeExpansionMode
    -> Map.Map String String
    -> PosixParameterExpansion
    -> [ExpandedShellWordFragment]
expandPosixParameterExpansionInShellWord
    tildeExpansionMode
    expansionEnvironment
    parameterExpansion =
    case parameterExpansion of
        PosixSimpleParameterExpansion expansionContext name ->
            renderExpandedValue expansionContext $
                fromMaybe "" (environmentLookup name expansionEnvironment)
        PosixBracedParameterExpansion expansionContext name Nothing ->
            renderExpandedValue expansionContext $
                fromMaybe "" (environmentLookup name expansionEnvironment)
        PosixBracedParameterExpansion expansionContext name (Just (wordMode, wordChars)) ->
            let maybeValue = environmentLookup name expansionEnvironment
                isSet = isJust maybeValue
                isSetAndNonEmpty = maybe False (not . null) maybeValue
                expandedWord =
                    expandShellWordChars
                        tildeExpansionMode
                        expansionEnvironment
                        wordChars
             in case wordMode of
                    PosixParameterUseDefaultWord colonSensitive
                        | posixParameterShouldUseDefaultWord colonSensitive isSet isSetAndNonEmpty ->
                            expandedWord
                        | otherwise ->
                            renderExpandedValue expansionContext $ fromMaybe "" maybeValue
                    PosixParameterUseAlternativeWord colonSensitive
                        | posixParameterShouldUseAlternativeWord colonSensitive isSet isSetAndNonEmpty ->
                            expandedWord
                        | otherwise ->
                            renderExpandedValue expansionContext ""
    where
        renderExpandedValue expansionContext value =
            [ ExpandedShellWordFragment
                { expandedShellWordFragmentText = value
                , expandedShellWordFragmentAllowsPosixFieldSplitting =
                    shellContextAllowsPosixFieldSplitting expansionContext
                }
            ]

posixParameterShouldUseDefaultWord :: Bool -> Bool -> Bool -> Bool
posixParameterShouldUseDefaultWord colonSensitive isSet isSetAndNonEmpty
    | colonSensitive =
        not isSetAndNonEmpty
    | otherwise =
        not isSet

posixParameterShouldUseAlternativeWord :: Bool -> Bool -> Bool -> Bool
posixParameterShouldUseAlternativeWord colonSensitive isSet isSetAndNonEmpty
    | colonSensitive =
        isSetAndNonEmpty
    | otherwise =
        isSet

expandWindowsEnvironmentVariablesForHost :: String -> Map.Map String String -> Char -> String -> String
expandWindowsEnvironmentVariablesForHost hostOs expansionEnvironment delimiter = go
    where
        go [] = []
        go (x:xs)
            | x /= delimiter =
                x : go xs
            | otherwise =
                case break (== delimiter) xs of
                    (name, _:rest)
                        | isWindowsEnvironmentVariableName name ->
                            fromMaybe "" (environmentLookupForHost hostOs name expansionEnvironment) <> go rest
                    _ ->
                        delimiter : go xs

isEnvironmentVariableName :: String -> Bool
isEnvironmentVariableName [] = False
isEnvironmentVariableName (x:xs) =
    startsLikeIdentifier x && all isEnvironmentVariableNameChar xs

startsLikeIdentifier :: Char -> Bool
startsLikeIdentifier c =
    isAlpha c || c == '_'

isEnvironmentVariableNameChar :: Char -> Bool
isEnvironmentVariableNameChar c =
    isAlphaNum c || c == '_'

isWindowsEnvironmentVariableName :: String -> Bool
isWindowsEnvironmentVariableName name =
    not (null name) && all isWindowsEnvironmentVariableNameChar name

isWindowsEnvironmentVariableNameChar :: Char -> Bool
isWindowsEnvironmentVariableNameChar c =
    not (isSpace c) && c /= '"' && c /= '%' && c /= '!'

expandWindowsEnvironmentVariablesForTest :: [(String, String)] -> Char -> String -> String
expandWindowsEnvironmentVariablesForTest environment =
    expandWindowsEnvironmentVariablesForHost "mingw32" (environmentFromListForHost "mingw32" environment)

compilerProcessEnvCommandIn :: Maybe FilePath -> CompilerCommand -> IO (Maybe [(String, String)])
compilerProcessEnvCommandIn maybeWorkingDir compiler = do
    baseEnvironment <- baseProcessEnvironment maybeWorkingDir
    if null (compilerEnvOverrides compiler) && isNothing maybeWorkingDir
        then pure Nothing
        else do
            expandedOverrides <-
                expandEnvironmentOverrides
                    maybeWorkingDir
                    (compilerEnvOverrides compiler)
                    (compilerEnvOverrideSpecs compiler)
            pure . Just . Map.toList $
                Map.union (environmentFromList expandedOverrides) baseEnvironment

readCompilerProcessWithExitCodeIn :: Maybe FilePath -> CompilerCommand -> [String] -> IO (ExitCode, String, String)
readCompilerProcessWithExitCodeIn maybeWorkingDir compiler extraArgs = do
    processEnv <- compilerProcessEnvCommandIn maybeWorkingDir compiler
    readCreateProcessWithExitCode
        (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
            { cwd = maybeWorkingDir
            , env = processEnv
            }
        ""

readCompilerProcessWithExitCode :: CompilerCommand -> [String] -> IO (ExitCode, String, String)
readCompilerProcessWithExitCode =
    readCompilerProcessWithExitCodeIn Nothing

pinStackLauncherToRepoRoot :: FilePath -> CompilerCommand -> CompilerCommand
pinStackLauncherToRepoRoot repoRoot compiler
    | usesStackLauncher compiler
        && not (stackYamlAlreadyPinned compiler) =
        compiler
            { compilerArguments =
                ["--stack-yaml", repoRoot </> "stack.yaml"] <> compilerArguments compiler
            }
    | otherwise =
        compiler
    where
        usesStackLauncher =
            isStackExecutableName . map toLower . takeFileName . compilerExecutable

        isStackExecutableName name =
            name == "stack" || name == "stack.exe"

        stackYamlAlreadyPinned compilerCommand =
            any ((== "STACK_YAML") . fst) (compilerEnvOverrides compilerCommand)
                || any isStackYamlArgument (compilerArguments compilerCommand)

        isStackYamlArgument argument =
            argument == "--stack-yaml" || "--stack-yaml=" `isPrefixOf` argument
