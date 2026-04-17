{-# LANGUAGE LambdaCase, TemplateHaskell, TupleSections #-}
module Main where

import           Control.Applicative                         ((<|>))
import           Control.Concurrent                          (forkIO,
                                                              threadDelay,
                                                              yield)
import           Control.Concurrent.MVar                     (MVar,
                                                              newEmptyMVar,
                                                              putMVar, takeMVar)
import           Control.Concurrent.STM                      (STM, TVar,
                                                              atomically, check,
                                                              newTVarIO, orElse,
                                                              readTVar,
                                                              writeTVar)
import           Control.Exception                           (SomeException,
                                                              bracket, evaluate,
                                                              finally, throwIO,
                                                              try)
import           Control.Monad                               (foldM, forM_,
                                                              unless, when,
                                                              (>=>))
import           Data.Bifunctor                              (first)
import           Data.Bits                                   (Bits (shiftL, (.&.), (.|.)))
import           Data.Bool                                   (bool)
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Char8                       as BC
import           Data.Char                                   (isAlpha,
                                                              isAlphaNum,
                                                              isSpace, toLower)
import           Data.Either                                 (fromRight)
import           Data.Functor                                (($>), (<&>))
import           Data.IORef                                  (modifyIORef',
                                                              newIORef,
                                                              readIORef,
                                                              writeIORef)
import           Data.List                                   (foldl',
                                                              intercalate,
                                                              isInfixOf,
                                                              isPrefixOf,
                                                              mapAccumL, sortOn,
                                                              stripPrefix)
import           Data.List.NonEmpty                          (NonEmpty (..))
import           Data.Maybe                                  (catMaybes,
                                                              fromMaybe, isJust,
                                                              mapMaybe)
import qualified Data.Sequence                               as SQ
import qualified Data.Text.IO                                as T
import           Data.Version                                (showVersion)
import           Data.Word                                   (Word64, Word8)
import           Language.Haskell.TH.Syntax                  (addDependentFile,
                                                              lift, runIO)
import qualified Options.Applicative                         as OA
import qualified Paths_htcc                                  as P

import qualified Data.Map.Strict                             as Map
import qualified Data.Set                                    as Set
import qualified Data.Text                                   as T
import           Data.Void
import           Diagrams.Prelude                            (V2)
import           Diagrams.Size                               (SizeSpec)
import           Diagrams.TwoD.Size                          (mkSizeSpec2D)
import           GHC.Conc                                    (threadWaitReadSTM)
import           GHC.IO.Exception                            (IOErrorType (NoSuchThing, PermissionDenied, ResourceExhausted))
import           GHC.IO.Handle                               (hDuplicate)
import           Htcc.Asm                                    (casmNormalized',
                                                              prepareAsmInput,
                                                              prepareVisualizableInput)
import qualified Htcc.Asm.Intrinsic.Structure.Internal       as SI
import qualified Htcc.CRules.Types                           as CT
import qualified Htcc.MegaparsecCompat                       as M
import           Htcc.Output                                 (ReplacementOutputMode (..),
                                                              creationMaskedOutputMode,
                                                              resolveReplacementOutputPath,
                                                              stagedOutputMode,
                                                              temporaryWritableMode,
                                                              withReplacementOutputPath,
                                                              withReplacementOutputPathAndResolvedPath)
import           Htcc.Parser                                 (ASTs, ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..))
import           Htcc.Parser.Combinators                     (parser, runParser)
import qualified Htcc.Parser.Combinators.ParserType          as PT
import           Htcc.Parser.ConstructionData.Core           (Warnings)
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import           Htcc.Parser.ConstructionData.Scope.Var      (GVar (..),
                                                              GVarInitData (..),
                                                              GVarInitWith (..),
                                                              GlobalVars,
                                                              Literal (..),
                                                              Literals)
import           Htcc.Utils
import           Htcc.Visualizer                             (validateVisualizationOutputPath,
                                                              writeVisualization)
import           Htcc.WarningSuppression                     (CompilerOutputChunk,
                                                              CompilerWarningFilterDecision (..),
                                                              IncrementalCompilerWarningFilter,
                                                              emptyIncrementalCompilerWarningFilter,
                                                              feedIncrementalCompilerWarningFilter,
                                                              filterCompilerOutputChunks,
                                                              finalCompilerOutputChunk,
                                                              finalizeIncrementalCompilerWarningFilter,
                                                              incompleteCompilerOutputNeedsMoreInputForWarningSuppression,
                                                              newlineByte,
                                                              normalizeCompilerOutputLine,
                                                              splitCompleteCompilerOutputChunks)
import           Numeric.Natural                             (Natural)
import           System.Directory                            (canonicalizePath,
                                                              doesFileExist,
                                                              executable,
                                                              getCurrentDirectory,
                                                              getPermissions,
                                                              getTemporaryDirectory,
                                                              makeAbsolute,
                                                              removeFile)
import           System.Environment                          (getEnvironment,
                                                              lookupEnv)
import           System.Exit                                 (ExitCode (..),
                                                              exitFailure)
import           System.FilePath                             (isAbsolute,
                                                              normalise,
                                                              searchPathSeparator,
                                                              takeFileName,
                                                              (</>))
import           System.Info                                 (os)
import           System.IO                                   (Handle,
                                                              IOMode (ReadMode, WriteMode),
                                                              hClose, hFlush,
                                                              hPutStr,
                                                              hSetBinaryMode,
                                                              openTempFile,
                                                              stderr, stdout,
                                                              withFile)
import           System.IO.Error                             (catchIOError,
                                                              ioeGetErrorString,
                                                              ioeGetErrorType,
                                                              isDoesNotExistError,
                                                              isEOFError)
import           System.Posix.Files                          (deviceID, fileID,
                                                              fileMode,
                                                              fileSize,
                                                              getFileStatus,
                                                              getSymbolicLinkStatus,
                                                              groupExecuteMode,
                                                              intersectFileModes,
                                                              isRegularFile,
                                                              otherExecuteMode,
                                                              ownerExecuteMode,
                                                              ownerReadMode,
                                                              setFileMode,
                                                              unionFileModes)
import           System.Posix.IO                             (FdOption (NonBlockingRead),
                                                              closeFd,
                                                              handleToFd,
                                                              setFdOption)
import qualified System.Posix.IO.ByteString                  as PB
import           System.Posix.Signals                        (nullSignal,
                                                              signalProcessGroup)
import           System.Posix.Types                          (FileMode,
                                                              ProcessGroupID)
import           System.Process                              (CreateProcess (..),
                                                              ProcessHandle,
                                                              StdStream (CreatePipe, Inherit),
                                                              createProcess,
                                                              getPid, proc,
                                                              readCreateProcessWithExitCode,
                                                              readProcessWithExitCode,
                                                              waitForProcess,
                                                              withCreateProcess)
import           System.Timeout                              (timeout)
import qualified Text.Parsec                                 as Parsec
import           Text.Read                                   (readMaybe)

data Opts = Opts
    { optIsRunAsm      :: !Bool
    , optIsVerbose     :: !Bool
    , optVisualizeAst  :: !Bool
    , optImgResolution :: !(Maybe String)
    , optSuppressWarns :: !Bool
    , optOutput        :: Maybe FilePath
    , optInput         :: [FilePath]
    } deriving (Read, Show)

output :: OA.Parser (Maybe String)
output = OA.optional $ outputOption <|> legacyOutputOption
    where
        outputHelp = "Place the output into <file> (legacy alias: --out)"
        outputOption = OA.strOption $ mconcat [
            OA.metavar "<file>"
          , OA.long "output"
          , OA.short 'o'
          , OA.help outputHelp
          ]
        legacyOutputOption = OA.strOption $ mconcat [
            OA.metavar "<file>"
          , OA.long "out"
          , OA.internal
          ]

visualizeAst :: OA.Parser Bool
visualizeAst = OA.switch $ mconcat [
    OA.long "visualize-ast"
  , OA.help "Visualize ASTs instead of emitting assembly"
  ]

imgResolution :: OA.Parser (Maybe String)
imgResolution = OA.optional $ OA.strOption $ mconcat [
    OA.metavar "RESOLUTION"
  , OA.long "img-resolution"
  , OA.help "Specify the output size for --visualize-ast as WIDTHxHEIGHT (default: 640x480)"
  ]

suppressWarns :: OA.Parser Bool
suppressWarns = canonicalFlag <|> legacyFlag <|> pure False
    where
        canonicalFlag = OA.flag' True $ mconcat
            [ OA.long "suppress-warns"
            , OA.short 'w'
            , OA.help "Disable all warning messages (legacy alias: --supress-warns)"
            ]
        legacyFlag = OA.flag' True $ mconcat
            [ OA.long "supress-warns"
            , OA.internal
            ]

input :: OA.Parser [String]
input = OA.some $ OA.strArgument $ mconcat [
    OA.metavar "file..."
  , OA.help "Input source files"
  ]

isRunAsm :: OA.Parser Bool
isRunAsm = OA.switch $ mconcat [
    OA.long "run-asm"
  , OA.short 'r'
  , OA.help "Generates executable binaries via the driver selected by $HTCC_ASSEMBLER"
  ]

isVerbose :: OA.Parser Bool
isVerbose = OA.switch $ mconcat [
    OA.long "verbose"
  , OA.short 'v'
  , OA.help "Show the programs invoked by the compiler"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> isRunAsm
    <*> isVerbose
    <*> visualizeAst
    <*> imgResolution
    <*> suppressWarns
    <*> output
    <*> input

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [
    OA.long "version"
  , OA.help "Show compiler version information"
  ]
    where
        vopt = concat [
            "The C Language Compiler htcc "
          , showVersion P.version
          , "\ncommit hash: "
          , gitHashValue
          ]

gitHashValue :: String
gitHashValue = $(do
    let trim = reverse . dropWhile isSpace . reverse
        readGit args =
            catchIOError
                (do
                    (exitCode, stdoutOut, _) <- readProcessWithExitCode "git" args ""
                    pure $ case exitCode of
                        ExitSuccess   -> Just $ trim stdoutOut
                        ExitFailure _ -> Nothing
                )
                (const $ pure Nothing)
        addGitDependency path = do
            exists <- runIO $ doesFileExist path
            when exists $ addDependentFile path

    gitDir <- runIO $ readGit ["rev-parse", "--git-dir"]
    case gitDir of
        Just dir -> do
            let headPath = dir </> "HEAD"
            addGitDependency headPath
            addGitDependency $ dir </> "packed-refs"
            headRef <- runIO $
                catchIOError
                    (stripPrefix "ref: " . trim <$> readFile headPath)
                    (const $ pure Nothing)
            maybe (pure ()) (addGitDependency . (dir </>)) headRef
        Nothing -> pure ()

    lift . fromMaybe "unknown" =<< runIO (readGit ["rev-parse", "HEAD"])
    )

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> versionOption <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ "The C Language Compiler htcc " ++ showVersion P.version
  ]

ignoreIOException :: IO () -> IO ()
ignoreIOException = flip catchIOError $ const $ pure ()

nonEmptyEnv :: Maybe String -> Maybe String
nonEmptyEnv (Just s) | all isSpace s = Nothing
nonEmptyEnv x         = x

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

shellWordText :: ParsedShellWord -> String
shellWordText =
    concatMap shellWordSpanText . parsedShellWordSpans

environmentAssignmentName :: ParsedShellWord -> Maybe String
environmentAssignmentName parsedWord =
    case span isAssignmentNameChar $ shellWordCharsFromSpans $ parsedShellWordSpans parsedWord of
        (firstChar:remainingNameChars, equalsChar:_)
            | startsLikeIdentifier (shellWordCharText firstChar)
                && all (isEnvironmentVariableNameChar . shellWordCharText) remainingNameChars
                && shellWordCharContext equalsChar == PosixShellUnquotedContext
                && shellWordCharText equalsChar == '=' ->
                Just $ map shellWordCharText (firstChar : remainingNameChars)
        _ ->
            Nothing
    where
        isAssignmentNameChar wordChar =
            shellWordCharContext wordChar == PosixShellUnquotedContext
                && isEnvironmentVariableNameChar (shellWordCharText wordChar)

isEnvironmentAssignmentWord :: ParsedShellWord -> Bool
isEnvironmentAssignmentWord =
    isJust . environmentAssignmentName

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

shellWordsWithContext :: String -> Either String [ParsedShellWord]
shellWordsWithContext commandLine =
    either (Left . show) Right $ Parsec.parse shellParser "<compiler>" commandLine
    where
        shellParser = skipSpaces *> Parsec.sepEndBy word spaces <* Parsec.eof
        skipSpaces = Parsec.skipMany $ Parsec.satisfy isSpace
        spaces = Parsec.skipMany1 $ Parsec.satisfy isSpace
        word = buildParsedShellWord <$> Parsec.many1 chunk
        chunk
            | os == "mingw32" =
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
            | os == "mingw32" =
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
                && (os /= "mingw32" || c /= '^')
                && (os == "mingw32" || c /= '\'')
                && (os == "mingw32" || c /= '\\')

        isDoubleBareChar c =
            c /= '"'
                && c /= '\\'
                && (os /= "mingw32" || c /= '%')
                && (os /= "mingw32" || c /= '^')

shellWords :: String -> Either String [String]
shellWords =
    fmap (map shellWordText) . shellWordsWithContext

data CompilerCommand = CompilerCommand
    { compilerEnvOverrides     :: [(String, String)]
    , compilerEnvOverrideSpecs :: Maybe [CompilerEnvOverrideSpec]
    , compilerExecutable       :: FilePath
    , compilerArguments        :: [String]
    }

resolveCompilerCommand :: String -> IO CompilerCommand
resolveCompilerCommand =
    resolveCompilerCommandIn Nothing

resolveCompilerCommandIn :: Maybe FilePath -> String -> IO CompilerCommand
resolveCompilerCommandIn maybeWorkingDir compiler = do
    parsedParts <- case shellWordsWithContext compiler of
        Left parseErr -> ioError . userError $
            "failed to parse compiler command " <> show compiler <> ": " <> parseErr
        Right [] -> ioError . userError $
            "empty compiler command: " <> show compiler
        Right xs -> pure xs
    let (envAssignmentWords, initialCompilerWords) =
            span isEnvironmentAssignmentWord parsedParts
        splitAssignments =
            map splitEnvironmentAssignment envAssignmentWords
        initialEnvOverrides =
            map fst splitAssignments
        initialEnvOverrideSpecs =
            map snd splitAssignments
    baseEnvironment <- baseProcessEnvironment maybeWorkingDir
    let expandedEnvOverrides =
            environmentFromList $
                expandEnvironmentOverridesWithBaseEnvironment
                    baseEnvironment
                    initialEnvOverrides
                    (Just initialEnvOverrideSpecs)
        compilerWordExpansionEnvironment
            | os == "mingw32" =
                Map.union expandedEnvOverrides baseEnvironment
            | otherwise =
                baseEnvironment
        compilerResolutionEnvironment =
            Map.union expandedEnvOverrides baseEnvironment
        compilerParts =
            concatMap
                (expandParsedShellWordIntoArguments compilerWordExpansionEnvironment)
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
        splitEnvironmentAssignment word =
            case environmentAssignmentName word of
                Just name ->
                    ( (name, value)
                    , CompilerEnvOverrideSpec $
                        dropShellWordSpanChars (length name + 1) (parsedShellWordSpans word)
                    )
                    where
                        value = drop (length name + 1) (shellWordText word)
                Nothing ->
                    error "internal compiler error"

        hasExplicitSearchPathOverride =
            any
                ((== environmentNameKey "PATH") . environmentNameKey . fst)

        findExecutablePrefix _ _ _ [] = pure Nothing
        findExecutablePrefix maybeWorkingDir' envOverrides' explicitSearchPathOverride (cmd:_) = do
            resolved <-
                resolveExecutableCommand
                    maybeWorkingDir'
                    envOverrides'
                    explicitSearchPathOverride
                    cmd
            pure $ fmap (1,) resolved

        resolveExecutableCommand maybeWorkingDir' envOverrides' explicitSearchPathOverride cmd = do
            if hasExplicitPath cmd
                then localExecutablePath maybeWorkingDir' cmd
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
                (environmentLookup "PATH" envOverrides')
            maybeResolvedFromPath <- firstResolved $
                map (localExecutablePath maybeWorkingDir' . searchPathCommand cmd) $
                    searchPathEntries pathValue
            case maybeResolvedFromPath of
                Just resolvedFromPath ->
                    pure $ Just resolvedFromPath
                Nothing
                    | explicitSearchPathOverride ->
                        pure Nothing
                    | otherwise ->
                        localExecutablePath maybeWorkingDir' cmd

        searchPathCommand cmd ""  = cmd
        searchPathCommand cmd dir = dir </> cmd

        searchPathEntries pathValue = case break (== searchPathSeparator) pathValue of
            (dir, [])       -> [dir]
            (dir, _:remain) -> dir : searchPathEntries remain

        localExecutablePath maybeWorkingDir' cmd = do
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
                    then Just $ normalizeLocalExecutablePath maybeWorkingDir' cmd candidatePath
                    else Nothing

        hasExplicitPath = any (`elem` ['/', '\\'])

        firstResolved [] = pure Nothing
        firstResolved (resolvePath : resolvePaths) = do
            resolved <- resolvePath
            maybe (firstResolved resolvePaths) (pure . Just) resolved

        normalizeLocalExecutablePath maybeWorkingDir' cmd candidatePath
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
expandEnvironmentOverridesWithBaseEnvironment baseEnvironment overrides maybeOverrideSpecs =
    reverse . snd $
        foldl'
            expandOverride
            (baseEnvironment, [])
            (zipOverrideSpecs overrides maybeOverrideSpecs)
    where
        expandOverride (expansionEnvironment, expandedOverrides) ((name, value), maybeOverrideSpec) =
            let expandedValue = maybe
                    (expandEnvironmentValue expansionEnvironment value)
                    (expandEnvironmentValueWithShellSpans expansionEnvironment)
                    maybeOverrideSpec
             in ( environmentInsert name expandedValue expansionEnvironment
                , (name, expandedValue) : expandedOverrides
                )

baseProcessEnvironment :: Maybe FilePath -> IO (Map.Map String String)
baseProcessEnvironment maybeWorkingDir = do
    baseEnvironment <- environmentFromList <$> getEnvironment
    case maybeWorkingDir of
        Just workingDir ->
            pure $ environmentInsert "PWD" workingDir baseEnvironment
        Nothing ->
            case environmentLookup "PWD" baseEnvironment of
                Just _ ->
                    pure baseEnvironment
                Nothing -> do
                    workingDir <- getCurrentDirectory
                    pure $ environmentInsert "PWD" workingDir baseEnvironment

environmentNameKey :: String -> String
environmentNameKey name
    | os == "mingw32" = map toLower name
    | otherwise = name

environmentFromList :: [(String, String)] -> Map.Map String String
environmentFromList =
    Map.fromList . map (first environmentNameKey)

environmentInsert :: String -> String -> Map.Map String String -> Map.Map String String
environmentInsert = Map.insert . environmentNameKey

environmentLookup :: String -> Map.Map String String -> Maybe String
environmentLookup name =
    Map.lookup (environmentNameKey name)

expandEnvironmentValue :: Map.Map String String -> String -> String
expandEnvironmentValue expansionEnvironment =
    expandForHost
    where
        expandForHost
            | os == "mingw32" =
                expandWindowsEnvironmentVariables expansionEnvironment '!'
                    . expandWindowsEnvironmentVariables expansionEnvironment '%'
            | otherwise =
                expandPosixEnvironmentVariables expansionEnvironment

expandParsedShellWord :: Map.Map String String -> ParsedShellWord -> String
expandParsedShellWord expansionEnvironment =
    expandShellWordSpans expansionEnvironment . parsedShellWordSpans

expandParsedShellWordIntoArguments :: Map.Map String String -> ParsedShellWord -> [String]
expandParsedShellWordIntoArguments expansionEnvironment parsedWord
    | os == "mingw32" =
        let expandedWord =
                expandParsedShellWord expansionEnvironment parsedWord
            fields =
                if parsedShellWordAllowsWindowsRetokenization parsedWord
                    then
                        case shellWordsWithContext expandedWord of
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

expandShellWordSpans :: Map.Map String String -> [ShellWordSpan] -> String
expandShellWordSpans expansionEnvironment spans
    | os == "mingw32" =
        concatMap
            (expandWindowsShellWordSpan expansionEnvironment)
            spans
    | otherwise =
        concatMap expandedShellWordFragmentText $
            expandShellWordChars
                PosixTildeExpansionForShellWord
                expansionEnvironment
                (shellWordCharsFromSpans spans)

expandWindowsShellWordSpan :: Map.Map String String -> ShellWordSpan -> String
expandWindowsShellWordSpan expansionEnvironment wordSpan
    | shellWordSpanAllowsPosixExpansion wordSpan =
        expandEnvironmentValue
            expansionEnvironment
            (shellWordSpanText wordSpan)
    | otherwise =
        shellWordSpanText wordSpan

expandEnvironmentValueWithShellSpans :: Map.Map String String -> CompilerEnvOverrideSpec -> String
expandEnvironmentValueWithShellSpans expansionEnvironment overrideSpec
    | os == "mingw32" =
        concatMap
            (expandWindowsShellWordSpan expansionEnvironment)
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

expandWindowsEnvironmentVariables :: Map.Map String String -> Char -> String -> String
expandWindowsEnvironmentVariables expansionEnvironment delimiter = go
    where
        go [] = []
        go (x:xs)
            | x /= delimiter =
                x : go xs
            | otherwise =
                case break (== delimiter) xs of
                    (name, _:rest)
                        | isWindowsEnvironmentVariableName name ->
                            fromMaybe "" (environmentLookup name expansionEnvironment) <> go rest
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

compilerProcessEnv :: CompilerCommand -> IO (Maybe [(String, String)])
compilerProcessEnv compiler
    | null (compilerEnvOverrides compiler) = pure Nothing
    | otherwise = do
        expandedOverrides <-
            expandEnvironmentOverrides
                Nothing
                (compilerEnvOverrides compiler)
                (compilerEnvOverrideSpecs compiler)
        Just . Map.toList . Map.union (environmentFromList expandedOverrides)
            <$> baseProcessEnvironment Nothing

showCompilerCommandForUser :: CompilerCommand -> [String] -> String
showCompilerCommandForUser =
    renderCompilerCommandForUserHost os

renderCompilerCommandForUserHost :: String -> CompilerCommand -> [String] -> String
renderCompilerCommandForUserHost hostOs compiler extraArgs
    | hostOs == "mingw32" =
        intercalate " && " $
            ["setlocal EnableDelayedExpansion" | windowsCommandNeedsDelayedExpansion]
                <> map renderWindowsEnvOverride (compilerEnvOverrides compiler)
                <> [renderCommandWords]
    | otherwise =
        unwords $
            renderPosixEnvAssignments
                <> quotedCommandWords
    where
        quoteWord = shellQuoteForHost hostOs
        renderPosixEnvAssignments =
            zipOverrideSpecs (compilerEnvOverrides compiler) (compilerEnvOverrideSpecs compiler)
                <&> uncurry renderPosixEnvAssignment
        commandWords =
            compilerExecutable compiler : compilerInvocationArgs compiler extraArgs
        windowsCommandNeedsDelayedExpansion =
            not (null (compilerEnvOverrides compiler))
                || any ('!' `elem`) commandWords
        quotedCommandWords =
            map quoteWord commandWords
        renderCommandWords =
            unwords quotedCommandWords
        renderPosixEnvAssignment (name, value) maybeOverrideSpec =
            name
                <> "="
                <> maybe
                    (quoteWord value)
                    renderPosixEnvOverrideSpec
                    maybeOverrideSpec
        renderWindowsEnvOverride (name, value) =
            "set " <> cmdExeSetAssignmentQuote (name <> "=" <> value)
        renderPosixEnvOverrideSpec (CompilerEnvOverrideSpec spans) =
            let wordChars = shellWordCharsFromSpans spans
             in maybe
                    (renderPosixShellWordChars wordChars)
                    (renderUniformPosixShellWordChars wordChars)
                    (uniformShellWordContext wordChars)

zipOverrideSpecs
    :: [(String, String)]
    -> Maybe [CompilerEnvOverrideSpec]
    -> [((String, String), Maybe CompilerEnvOverrideSpec)]
zipOverrideSpecs overrides maybeOverrideSpecs =
    case maybeOverrideSpecs of
        Just overrideSpecs
            | length overrideSpecs == length overrides ->
                zip overrides $ map Just overrideSpecs
        _ ->
            map (, Nothing) overrides

shellQuoteForHost :: String -> String -> String
shellQuoteForHost hostOs
    | hostOs == "mingw32" = cmdExeQuote
    | otherwise = shellQuote

cmdExeQuote :: String -> String
cmdExeQuote =
    quoteWindowsCommandWord . escapeWindowsDelayedExpansion . escapeWindowsPercentExpansion

cmdExeSetAssignmentQuote :: String -> String
cmdExeSetAssignmentQuote =
    (\value -> "\"" <> value <> "\"")
        . concatMap escapeWindowsSetAssignmentQuote
        . escapeWindowsSetAssignmentExpansion

escapeWindowsSetAssignmentQuote :: Char -> String
escapeWindowsSetAssignmentQuote c
    | c == '"' = "\"\""
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
            | isWindowsEnvironmentVariableName name ->
                "!" <> name <> "!" <> escapeWindowsSetAssignmentExpansion rest
        _ ->
            "%%" <> escapeWindowsSetAssignmentExpansion xs
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

uniformShellWordContext :: [ShellWordChar] -> Maybe PosixShellContext
uniformShellWordContext [] = Nothing
uniformShellWordContext (firstChar:remainingChars)
    | all ((== shellWordCharContext firstChar) . shellWordCharContext) remainingChars =
        Just $ shellWordCharContext firstChar
    | otherwise =
        Nothing

renderUniformPosixShellWordChars :: [ShellWordChar] -> PosixShellContext -> String
renderUniformPosixShellWordChars wordChars context =
    case context of
        PosixShellLiteralContext ->
            shellQuote wordText
        PosixShellDoubleQuotedContext ->
            "\"" <> renderPosixDoubleQuotedExpandableText wordText <> "\""
        PosixShellUnquotedContext ->
            renderPosixShellWordChars wordChars
    where
        wordText = map shellWordCharText wordChars

renderPosixDoubleQuotedExpandableText :: String -> String
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

renderPosixShellWordChars :: [ShellWordChar] -> String
renderPosixShellWordChars [] = shellQuote ""
renderPosixShellWordChars chars =
    renderNonEmptyPosixShellWordChars chars

renderPosixParameterExpansionWord :: [ShellWordChar] -> String
renderPosixParameterExpansionWord [] = []
renderPosixParameterExpansionWord chars =
    renderNonEmptyPosixShellWordChars chars

renderNonEmptyPosixShellWordChars :: [ShellWordChar] -> String
renderNonEmptyPosixShellWordChars [] = []
renderNonEmptyPosixShellWordChars chars@(currentChar:_)
    | shellWordCharContext currentChar == PosixShellLiteralContext =
        let (segment, remainingChars) =
                span
                    ((== PosixShellLiteralContext) . shellWordCharContext)
                    chars
         in shellQuote (map shellWordCharText segment)
                <> renderNonEmptyPosixShellWordChars remainingChars
    | shellWordCharContext currentChar == PosixShellDoubleQuotedContext =
        "\"" <> renderDoubleQuotedShellWordChars chars
    | otherwise =
        renderUnquotedShellWordChars chars

renderDoubleQuotedShellWordChars :: [ShellWordChar] -> String
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

renderUnquotedShellWordChars :: [ShellWordChar] -> String
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

escapePosixUnquotedChar :: Char -> String
escapePosixUnquotedChar c
    | isSpace c || c `elem` ['\\', '"', '\'', '#', ';', '&', '|', '<', '>', '*', '?', '[', ']', '(', ')', '{', '}'] =
        ['\\', c]
    | otherwise =
        [c]

renderPosixParameterExpansion :: PosixParameterExpansion -> String
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

renderPosixParameterExpansionWordMode :: PosixParameterExpansionWordMode -> String
renderPosixParameterExpansionWordMode wordMode =
    case wordMode of
        PosixParameterUseDefaultWord colonSensitive ->
            bool "-" ":-" colonSensitive
        PosixParameterUseAlternativeWord colonSensitive ->
            bool "+" ":+" colonSensitive

escapePosixDoubleQuotedChar :: Char -> String
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

shellQuote :: String -> String
shellQuote word =
    "'" <> concatMap escapeShellQuoteChar word <> "'"
    where
        escapeShellQuoteChar '\''
            = "'\"'\"'"
        escapeShellQuoteChar c
            = [c]

readCompilerProcessWithExitCode :: CompilerCommand -> [String] -> IO (ExitCode, String, String)
readCompilerProcessWithExitCode compiler extraArgs = do
    processEnv <- compilerProcessEnv compiler
    readCreateProcessWithExitCode
        (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
            { env = processEnv
            }
        ""

data CompilerOutputStream
    = CompilerStdout
    | CompilerStderr
    deriving (Eq, Ord, Show)

data CapturedCompilerOutputChunk = CapturedCompilerOutputChunk
    { capturedCompilerOutputStream :: CompilerOutputStream
    , capturedCompilerOutputIndex  :: Int
    , capturedCompilerOutputChunk  :: CompilerOutputChunk
    }

type CapturedCompilerOutputKey = (CompilerOutputStream, Int)

data SuppressibleCapturedCompilerOutputChunk = SuppressibleCapturedCompilerOutputChunk
    { suppressibleCapturedCompilerOutputKeys  :: [CapturedCompilerOutputKey]
    , suppressibleCapturedCompilerOutputChunk :: CompilerOutputChunk
    }

data CapturedCompilerOutputDecision
    = RetainCapturedCompilerOutput
    | SuppressCapturedCompilerOutput
    deriving (Eq)

data IncrementalStreamWarningSuppressionState = IncrementalStreamWarningSuppressionState
    { incrementalStreamWarningPendingChunk  :: Maybe ([CapturedCompilerOutputKey], [B.ByteString])
    , incrementalStreamWarningChunkFilter   :: IncrementalCompilerWarningFilter SuppressibleCapturedCompilerOutputChunk
    }

data IncrementalCompilerWarningSuppressionState = IncrementalCompilerWarningSuppressionState
    { incrementalCompilerWarningStdoutState :: IncrementalStreamWarningSuppressionState
    , incrementalCompilerWarningStderrState :: IncrementalStreamWarningSuppressionState
    , incrementalCompilerWarningChunkFilter :: IncrementalCompilerWarningFilter SuppressibleCapturedCompilerOutputChunk
    , incrementalCompilerWarningPending     :: [CapturedCompilerOutputChunk]
    , incrementalCompilerWarningDecisions   :: Map.Map CapturedCompilerOutputKey CapturedCompilerOutputDecision
    }

readCompilerProcessWithExitCodeChunks
    :: StdStream
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, [CapturedCompilerOutputChunk])
readCompilerProcessWithExitCodeChunks =
    readCompilerProcessWithExitCodeChunksUntil (\_ -> pure False)

readCompilerProcessWithExitCodeChunksUntil
    :: (IO [CapturedCompilerOutputChunk] -> IO Bool)
    -> StdStream
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, [CapturedCompilerOutputChunk])
readCompilerProcessWithExitCodeChunksUntil postExitDrainSatisfied stdinStream compiler extraArgs = do
    capturedChunksRef <- newIORef []
    let readCapturedChunks =
            reverse <$> readIORef capturedChunksRef
    (exitCode, ()) <-
        foldCompilerProcessWithExitCodeChunksUntil
            (postExitDrainSatisfied readCapturedChunks)
            stdinStream
            compiler
            extraArgs
            ()
            (\() capturedChunk -> modifyIORef' capturedChunksRef (capturedChunk :) $> ())
    capturedChunks <- readCapturedChunks
    pure (exitCode, capturedChunks)

foldCompilerProcessWithExitCodeChunks
    :: StdStream
    -> CompilerCommand
    -> [String]
    -> a
    -> (a -> CapturedCompilerOutputChunk -> IO a)
    -> IO (ExitCode, a)
foldCompilerProcessWithExitCodeChunks =
    foldCompilerProcessWithExitCodeChunksUntil (pure False)

foldCompilerProcessWithExitCodeChunksUntil
    :: IO Bool
    -> StdStream
    -> CompilerCommand
    -> [String]
    -> a
    -> (a -> CapturedCompilerOutputChunk -> IO a)
    -> IO (ExitCode, a)
foldCompilerProcessWithExitCodeChunksUntil postExitDrainSatisfied stdinStream compiler extraArgs initialAcc accumulateChunk = do
    processEnv <- compilerProcessEnv compiler
    withCreateProcess
        (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
            { env = processEnv
            , std_in = stdinStream
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            } $ \maybeInputHandle maybeStdoutHandle maybeStderrHandle processHandle -> do
                maybe (pure ()) hClose maybeInputHandle
                stdoutHandle <- requireCapturedHandle "stdout" maybeStdoutHandle
                stderrHandle <- requireCapturedHandle "stderr" maybeStderrHandle
                hSetBinaryMode stdoutHandle True
                hSetBinaryMode stderrHandle True
                processExitVar <- newTVarIO False
                processGroupId <- compilerProcessGroupIdForHandle processHandle
                capturedChunksVar <- newEmptyMVar
                _ <- forkIO $
                    putMVar capturedChunksVar =<< try
                        ( foldCompilerOutputChunks
                            processGroupId
                            postExitDrainSatisfied
                            processExitVar
                            stdoutHandle
                            stderrHandle
                            initialAcc
                            accumulateChunk
                        )
                exitCode <- waitForProcess processHandle
                atomically $ writeTVar processExitVar True
                capturedChunks <- takeCapturedResult capturedChunksVar
                pure (exitCode, capturedChunks)

readCompilerProcessWithExitCodeBytes :: CompilerCommand -> [String] -> IO (ExitCode, B.ByteString, B.ByteString)
readCompilerProcessWithExitCodeBytes =
    readCompilerProcessWithExitCodeBytesUntil (\_ -> pure False)

readCompilerProcessWithExitCodeBytesUntil
    :: (IO [CapturedCompilerOutputChunk] -> IO Bool)
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, B.ByteString, B.ByteString)
readCompilerProcessWithExitCodeBytesUntil postExitDrainSatisfied compiler extraArgs = do
    (exitCode, capturedChunks) <-
        readCompilerProcessWithExitCodeChunksUntil
            postExitDrainSatisfied
            CreatePipe
            compiler
            extraArgs
    pure
        ( exitCode
        , compilerOutputBytesForStream CompilerStdout capturedChunks
        , compilerOutputBytesForStream CompilerStderr capturedChunks
        )

requireCapturedHandle :: String -> Maybe Handle -> IO Handle
requireCapturedHandle handleName =
    maybe
        ( ioError . userError $
            "failed to capture compiler " <> handleName <> " output"
        )
        pure

compilerProcessGroupIdForHandle :: ProcessHandle -> IO (Maybe ProcessGroupID)
compilerProcessGroupIdForHandle =
    fmap (fmap fromIntegral) . getPid

compilerProcessGroupAlive :: Maybe ProcessGroupID -> IO Bool
compilerProcessGroupAlive =
    maybe
        (pure False)
        ( \processGroup ->
            catchIOError
                (signalProcessGroup nullSignal processGroup $> True)
                ( \ioErr ->
                    case ioeGetErrorType ioErr of
                        NoSuchThing      -> pure False
                        PermissionDenied -> pure True
                        _                -> ioError ioErr
                )
        )

waitForCompilerProcessPostExitCompletion :: Maybe ProcessGroupID -> IO Bool -> IO ()
waitForCompilerProcessPostExitCompletion processGroupId postExitSatisfied =
    go
    where
        go = do
            postExitCompleted <- postExitSatisfied
            processGroupStillAlive <- compilerProcessGroupAlive processGroupId
            unless (postExitCompleted || not processGroupStillAlive) $ do
                threadDelay compilerOutputDrainAfterExitPollMicros
                go

capturedCompilerTargetLineAvailableAfterExit :: IO [CapturedCompilerOutputChunk] -> IO Bool
capturedCompilerTargetLineAvailableAfterExit =
    fmap
        ( any isCompleteTargetLine
            . completeStdoutLines
            . compilerOutputBytesForStream CompilerStdout
        )
    where
        completeStdoutLines bytes
            | B.null bytes = []
            | B.last bytes == newlineByte = BC.lines bytes
            | otherwise =
                case BC.lines bytes of
                    []     -> []
                    lines' -> init lines'

        isCompleteTargetLine line =
            let trimmedLine = trimProbeLine line
             in not (B.null trimmedLine)
                    && BC.any (== '-') trimmedLine
                    && not (BC.any isSpace trimmedLine)

        trimProbeLine =
            BC.reverse . BC.dropWhile isSpace . BC.reverse . BC.dropWhile isSpace

waitForCompilerProcessGroupQuiescenceAfterExit :: IO Bool
-- Fallback for compiler invocations that do not have a more specific readiness
-- signal than process-group quiescence.
waitForCompilerProcessGroupQuiescenceAfterExit = pure False

stabilizePostExitPredicate :: IO Bool -> IO (IO Bool)
stabilizePostExitPredicate isReady = do
    wasReadyRef <- newIORef False
    pure $ do
        ready <- isReady
        wasReady <- readIORef wasReadyRef
        writeIORef wasReadyRef ready
        pure (ready && wasReady)

stabilizePostExitFingerprint :: Eq a => IO (Maybe a) -> IO (IO Bool)
stabilizePostExitFingerprint readFingerprint = do
    previousFingerprintRef <- newIORef Nothing
    pure $ do
        fingerprint <- readFingerprint
        previousFingerprint <- readIORef previousFingerprintRef
        writeIORef previousFingerprintRef fingerprint
        pure $
            case fingerprint of
                Just _  -> previousFingerprint == fingerprint
                Nothing -> False

compilerObjectOutputFingerprintAfterExit :: FilePath -> IO (Maybe (Integer, Integer, Integer))
compilerObjectOutputFingerprintAfterExit path =
    catchIOError
        (do
            status <- getSymbolicLinkStatus path
            let outputSize = fromIntegral (fileSize status) :: Integer
            pure $
                if isRegularFile status
                    && outputSize > minimumStableCompilerObjectOutputBytes
                    then
                        Just
                            ( fromIntegral (deviceID status)
                            , fromIntegral (fileID status)
                            , outputSize
                            )
                    else Nothing
        )
        (\ioErr -> if isDoesNotExistError ioErr then pure Nothing else ioError ioErr)

stabilizeCompilerObjectOutputAfterExit :: FilePath -> IO (IO Bool)
stabilizeCompilerObjectOutputAfterExit =
    stabilizePostExitFingerprint . compilerObjectOutputFingerprintAfterExit

foldCompilerOutputChunks
    :: Maybe ProcessGroupID
    -> IO Bool
    -> TVar Bool
    -> Handle
    -> Handle
    -> a
    -> (a -> CapturedCompilerOutputChunk -> IO a)
    -> IO a
foldCompilerOutputChunks processGroupId postExitDrainSatisfied processExitVar stdoutHandle stderrHandle initialAcc accumulateChunk = do
    stdoutFd <- handleToFd stdoutHandle
    stderrFd <- handleToFd stderrHandle
    -- Keep captured pipes nonblocking from the start so readiness races or
    -- HUP-only wakeups cannot strand us in a blocking read while leaked child
    -- writers still hold the pipe open.
    setFdOption stdoutFd NonBlockingRead True
    setFdOption stderrFd NonBlockingRead True
    (stdoutReady, closeStdoutWait) <- threadWaitReadSTM stdoutFd
    (stderrReady, closeStderrWait) <- threadWaitReadSTM stderrFd
    let cleanup =
            ignoreIOException closeStdoutWait
                *> ignoreIOException closeStderrWait
                *> ignoreIOException (closeFd stdoutFd)
                *> ignoreIOException (closeFd stderrFd)
    flip finally cleanup $
        go
            initialAcc
            []
            []
            B.empty
            B.empty
            True
            True
            0
            0
            CompilerStdout
            stdoutReady
            stderrReady
            stdoutFd
            stderrFd
    where
        go acc stdoutPendingChunks stderrPendingChunks stdoutTrailingBytes stderrTrailingBytes stdoutOpen stderrOpen stdoutIndex stderrIndex preferredStream stdoutReady stderrReady stdoutFd stderrFd
            | not stdoutOpen
                && not stderrOpen
                && null stdoutPendingChunks
                && null stderrPendingChunks =
                pure acc
            | otherwise = do
                shouldReadOtherStreamBeforePending <-
                    shouldReadOtherOutputStreamBeforePending
                        stdoutOpen
                        stderrOpen
                        stdoutPendingChunks
                        stderrPendingChunks
                        stdoutReady
                        stderrReady
                case
                        ( shouldReadOtherStreamBeforePending
                        , nextPendingOutputStream
                            preferredStream
                            stdoutPendingChunks
                            stderrPendingChunks
                        )
                    of
                    (False, Just outputStream) ->
                        do
                            (acc', stdoutPendingChunks', stderrPendingChunks', stdoutIndex', stderrIndex') <-
                                emitPendingCapturedCompilerOutputChunk
                                    outputStream
                                    acc
                                    stdoutPendingChunks
                                    stderrPendingChunks
                                    stdoutIndex
                                    stderrIndex
                            let preferredStream'
                                    | stdoutOpen && stderrOpen = flipCompilerOutputStream outputStream
                                    | otherwise = preferredStream
                            yield
                            go
                                acc'
                                stdoutPendingChunks'
                                stderrPendingChunks'
                                stdoutTrailingBytes
                                stderrTrailingBytes
                                stdoutOpen
                                stderrOpen
                                stdoutIndex'
                                stderrIndex'
                                preferredStream'
                                stdoutReady
                                stderrReady
                                stdoutFd
                                stderrFd
                    _ -> do
                        maybeOutputStream <-
                            waitForNextOutputStreamOrExit
                                processExitVar
                                preferredStream
                                stdoutOpen
                                stderrOpen
                                stdoutReady
                                stderrReady
                        case maybeOutputStream of
                            Nothing -> do
                                ( acc'
                                    , stdoutPendingChunks'
                                    , stderrPendingChunks'
                                    , stdoutIndex'
                                    , stderrIndex'
                                    ) <-
                                        drainCompilerOutputAfterExit
                                            acc
                                            stdoutPendingChunks
                                            stderrPendingChunks
                                            stdoutTrailingBytes
                                            stderrTrailingBytes
                                            stdoutOpen
                                            stderrOpen
                                            stdoutIndex
                                            stderrIndex
                                            preferredStream
                                            stdoutReady
                                            stderrReady
                                            stdoutFd
                                            stderrFd
                                go
                                    acc'
                                    stdoutPendingChunks'
                                    stderrPendingChunks'
                                    B.empty
                                    B.empty
                                    False
                                    False
                                    stdoutIndex'
                                    stderrIndex'
                                    preferredStream
                                    stdoutReady
                                    stderrReady
                                    stdoutFd
                                    stderrFd
                            Just outputStream -> do
                                (acc', stdoutTrailingBytes', stderrTrailingBytes', stdoutIndex', stderrIndex') <-
                                    flushPendingTrailingOutputChunkBefore
                                        outputStream
                                        acc
                                        stdoutTrailingBytes
                                        stderrTrailingBytes
                                        stdoutIndex
                                        stderrIndex
                                let (outputFd, trailingBytes) =
                                        case outputStream of
                                            CompilerStdout ->
                                                (stdoutFd, stdoutTrailingBytes')
                                            CompilerStderr ->
                                                (stderrFd, stderrTrailingBytes')
                                maybeReadResult <- readCompilerOutputByte outputFd
                                case maybeReadResult of
                                    Nothing -> case outputStream of
                                        CompilerStdout ->
                                            let (stdoutPendingChunks', stdoutIndex'') =
                                                    queuePendingCapturedCompilerOutputChunks
                                                        CompilerStdout
                                                        stdoutIndex'
                                                        (finalCompilerOutputChunk stdoutTrailingBytes')
                                                        stdoutPendingChunks
                                             in go
                                                    acc'
                                                    stdoutPendingChunks'
                                                    stderrPendingChunks
                                                    B.empty
                                                    stderrTrailingBytes'
                                                    False
                                                    stderrOpen
                                                    stdoutIndex''
                                                    stderrIndex'
                                                    preferredStream
                                                    stdoutReady
                                                    stderrReady
                                                    stdoutFd
                                                    stderrFd
                                        CompilerStderr ->
                                            let (stderrPendingChunks', stderrIndex'') =
                                                    queuePendingCapturedCompilerOutputChunks
                                                        CompilerStderr
                                                        stderrIndex'
                                                        (finalCompilerOutputChunk stderrTrailingBytes')
                                                        stderrPendingChunks
                                             in go
                                                    acc'
                                                    stdoutPendingChunks
                                                    stderrPendingChunks'
                                                    stdoutTrailingBytes'
                                                    B.empty
                                                    stdoutOpen
                                                    False
                                                    stdoutIndex'
                                                    stderrIndex''
                                                    preferredStream
                                                    stdoutReady
                                                    stderrReady
                                                    stdoutFd
                                                    stderrFd
                                    Just CompilerOutputReadEOF -> case outputStream of
                                        CompilerStdout ->
                                            let (stdoutPendingChunks', stdoutIndex'') =
                                                    queuePendingCapturedCompilerOutputChunks
                                                        CompilerStdout
                                                        stdoutIndex'
                                                        (finalCompilerOutputChunk stdoutTrailingBytes')
                                                        stdoutPendingChunks
                                             in go
                                                    acc'
                                                    stdoutPendingChunks'
                                                    stderrPendingChunks
                                                    B.empty
                                                    stderrTrailingBytes'
                                                    False
                                                    stderrOpen
                                                    stdoutIndex''
                                                    stderrIndex'
                                                    preferredStream
                                                    stdoutReady
                                                    stderrReady
                                                    stdoutFd
                                                    stderrFd
                                        CompilerStderr ->
                                            let (stderrPendingChunks', stderrIndex'') =
                                                    queuePendingCapturedCompilerOutputChunks
                                                        CompilerStderr
                                                        stderrIndex'
                                                        (finalCompilerOutputChunk stderrTrailingBytes')
                                                        stderrPendingChunks
                                             in go
                                                    acc'
                                                    stdoutPendingChunks
                                                    stderrPendingChunks'
                                                    stdoutTrailingBytes'
                                                    B.empty
                                                    stdoutOpen
                                                    False
                                                    stdoutIndex'
                                                    stderrIndex''
                                                    preferredStream
                                                    stdoutReady
                                                    stderrReady
                                                    stdoutFd
                                                    stderrFd
                                    Just CompilerOutputReadWouldBlock -> do
                                        yield
                                        go
                                            acc'
                                            stdoutPendingChunks
                                            stderrPendingChunks
                                            stdoutTrailingBytes'
                                            stderrTrailingBytes'
                                            stdoutOpen
                                            stderrOpen
                                            stdoutIndex'
                                            stderrIndex'
                                            preferredStream
                                            stdoutReady
                                            stderrReady
                                            stdoutFd
                                            stderrFd
                                    Just (CompilerOutputReadBytes bytes) -> do
                                        let (completedChunks, remainingTrailingBytes) =
                                                splitCompleteCompilerOutputChunks (trailingBytes <> bytes)
                                            readyChunks =
                                                completedChunks
                                                    <> finalCompilerOutputChunk remainingTrailingBytes
                                            readyChunkCount = length readyChunks
                                        ( acc''
                                            , stdoutPendingChunks'
                                            , stderrPendingChunks'
                                            , stdoutIndex''
                                            , stderrIndex''
                                            ) <-
                                                captureCompletedCompilerOutputChunks
                                                    outputStream
                                                    readyChunks
                                                    acc'
                                                    stdoutPendingChunks
                                                    stderrPendingChunks
                                                    stdoutIndex'
                                                    stderrIndex'
                                        let preferredStream'
                                                | stdoutOpen && stderrOpen = flipCompilerOutputStream outputStream
                                                | otherwise = preferredStream
                                        when (readyChunkCount > 0) yield
                                        go
                                            acc''
                                            stdoutPendingChunks'
                                            stderrPendingChunks'
                                            B.empty
                                            B.empty
                                            stdoutOpen
                                            stderrOpen
                                            stdoutIndex''
                                            stderrIndex''
                                            preferredStream'
                                            stdoutReady
                                            stderrReady
                                            stdoutFd
                                            stderrFd

        nextPendingOutputStream preferredStream stdoutPendingChunks stderrPendingChunks =
            case preferredStream of
                CompilerStdout
                    | not (null stdoutPendingChunks) -> Just CompilerStdout
                    | not (null stderrPendingChunks) -> Just CompilerStderr
                CompilerStderr
                    | not (null stderrPendingChunks) -> Just CompilerStderr
                    | not (null stdoutPendingChunks) -> Just CompilerStdout
                _ ->
                    Nothing

        shouldReadOtherOutputStreamBeforePending stdoutOpen stderrOpen stdoutPendingChunks stderrPendingChunks stdoutReady stderrReady =
            case (stdoutOpen, stderrOpen, stdoutPendingChunks, stderrPendingChunks) of
                (True, True, _ : _, []) ->
                    outputStreamReadyNow stderrReady
                (True, True, [], _ : _) ->
                    outputStreamReadyNow stdoutReady
                _ ->
                    pure False

        outputStreamReadyNow ready =
            atomically $ (ready >> pure True) `orElse` pure False

        captureCompletedCompilerOutputChunks outputStream completedChunks acc stdoutPendingChunks stderrPendingChunks stdoutIndex stderrIndex =
            case outputStream of
                CompilerStdout ->
                    let completedChunkCount = length completedChunks
                     in do
                        (acc', stdoutPendingChunks') <-
                            captureCompletedCompilerOutputChunksForStream
                                CompilerStdout
                                stdoutIndex
                                completedChunks
                                acc
                                stdoutPendingChunks
                        pure
                            ( acc'
                            , stdoutPendingChunks'
                            , stderrPendingChunks
                            , stdoutIndex + completedChunkCount
                            , stderrIndex
                            )
                CompilerStderr ->
                    let completedChunkCount = length completedChunks
                     in do
                        (acc', stderrPendingChunks') <-
                            captureCompletedCompilerOutputChunksForStream
                                CompilerStderr
                                stderrIndex
                                completedChunks
                                acc
                                stderrPendingChunks
                        pure
                            ( acc'
                            , stdoutPendingChunks
                            , stderrPendingChunks'
                            , stdoutIndex
                            , stderrIndex + completedChunkCount
                            )

        captureCompletedCompilerOutputChunksForStream outputStream startIndex completedChunks acc pendingChunks =
            case buildCapturedCompilerOutputChunks outputStream startIndex completedChunks of
                nextChunk:remainingChunks
                    | null pendingChunks ->
                        do
                            acc' <- accumulateChunk acc nextChunk
                            pure (acc', remainingChunks)
                    | otherwise ->
                        pure (acc, pendingChunks <> (nextChunk : remainingChunks))
                [] ->
                    pure (acc, pendingChunks)

        queuePendingCapturedCompilerOutputChunks outputStream startIndex outputChunks pendingChunks =
            let indexedChunks =
                    buildCapturedCompilerOutputChunks outputStream startIndex outputChunks
             in ( pendingChunks <> indexedChunks
                , startIndex + length indexedChunks
                )

        emitPendingCapturedCompilerOutputChunk outputStream acc stdoutPendingChunks stderrPendingChunks stdoutIndex stderrIndex =
            case outputStream of
                CompilerStdout ->
                    case stdoutPendingChunks of
                        nextChunk:remainingChunks ->
                            do
                                acc' <- accumulateChunk acc nextChunk
                                pure
                                    ( acc'
                                    , remainingChunks
                                    , stderrPendingChunks
                                    , stdoutIndex
                                    , stderrIndex
                                    )
                        [] ->
                            pure (acc, [], stderrPendingChunks, stdoutIndex, stderrIndex)
                CompilerStderr ->
                    case stderrPendingChunks of
                        nextChunk:remainingChunks ->
                            do
                                acc' <- accumulateChunk acc nextChunk
                                pure
                                    ( acc'
                                    , stdoutPendingChunks
                                    , remainingChunks
                                    , stdoutIndex
                                    , stderrIndex
                                    )
                        [] ->
                            pure (acc, stdoutPendingChunks, [], stdoutIndex, stderrIndex)

        flushPendingTrailingOutputChunkBefore outputStream acc stdoutTrailingBytes stderrTrailingBytes stdoutIndex stderrIndex =
            case outputStream of
                CompilerStdout -> do
                    acc' <- emitTrailingOutputChunk accumulateChunk CompilerStderr stderrIndex stderrTrailingBytes acc
                    pure
                        ( acc'
                        , stdoutTrailingBytes
                        , B.empty
                        , stdoutIndex
                        , stderrIndex + pendingTrailingOutputChunkCount stderrTrailingBytes
                        )
                CompilerStderr -> do
                    acc' <- emitTrailingOutputChunk accumulateChunk CompilerStdout stdoutIndex stdoutTrailingBytes acc
                    pure
                        ( acc'
                        , B.empty
                        , stderrTrailingBytes
                        , stdoutIndex + pendingTrailingOutputChunkCount stdoutTrailingBytes
                        , stderrIndex
                        )

        waitForNextOutputStreamOrExit
            :: TVar Bool
            -> CompilerOutputStream
            -> Bool
            -> Bool
            -> STM ()
            -> STM ()
            -> IO (Maybe CompilerOutputStream)
        waitForNextOutputStreamOrExit processExitVar' preferredStream stdoutOpen stderrOpen stdoutReady stderrReady =
            atomically $
                waitForNextOutputStreamStm
                    preferredStream
                    stdoutOpen
                    stderrOpen
                    stdoutReady
                    stderrReady
                    `orElse` waitForProcessExit processExitVar'

        waitForNextOutputStreamStm
            :: CompilerOutputStream
            -> Bool
            -> Bool
            -> STM ()
            -> STM ()
            -> STM (Maybe CompilerOutputStream)
        waitForNextOutputStreamStm preferredStream stdoutOpen stderrOpen stdoutReady stderrReady
            | stdoutOpen && stderrOpen =
                case preferredStream of
                    CompilerStdout ->
                        (stdoutReady >> pure (Just CompilerStdout))
                            `orElse` (stderrReady >> pure (Just CompilerStderr))
                    CompilerStderr ->
                        (stderrReady >> pure (Just CompilerStderr))
                            `orElse` (stdoutReady >> pure (Just CompilerStdout))
            | stdoutOpen =
                stdoutReady >> pure (Just CompilerStdout)
            | stderrOpen =
                stderrReady >> pure (Just CompilerStderr)
            | otherwise =
                pure Nothing

        waitForProcessExit :: TVar Bool -> STM (Maybe CompilerOutputStream)
        waitForProcessExit processExitVar' = do
            processExited <- readTVar processExitVar'
            check processExited
            pure Nothing

        -- Keep draining until the wrapper's meaningful side effects are ready.
        -- Once the caller-specific completion condition is satisfied, stop
        -- waiting for EOF from inherited pipe holders and finish with the bytes
        -- that were already observed.
        drainCompilerOutputAfterExit
            acc
            stdoutPendingChunks
            stderrPendingChunks
            stdoutTrailingBytes
            stderrTrailingBytes
            stdoutOpen
            stderrOpen
            stdoutIndex
            stderrIndex
            preferredStream
            stdoutReady
            stderrReady
            stdoutFd
            stderrFd = do
                when stdoutOpen $ setFdOption stdoutFd NonBlockingRead True
                when stderrOpen $ setFdOption stderrFd NonBlockingRead True
                drainCapturedCompilerOutputAfterExit
                    acc
                    stdoutPendingChunks
                    stderrPendingChunks
                    stdoutTrailingBytes
                    stderrTrailingBytes
                    stdoutOpen
                    stderrOpen
                    stdoutIndex
                    stderrIndex
                    preferredStream
                    stdoutReady
                    stderrReady
                    stdoutFd
                    stderrFd

        drainCapturedCompilerOutputAfterExit
            acc
            stdoutPendingChunks
            stderrPendingChunks
            stdoutTrailingBytes
            stderrTrailingBytes
            stdoutOpen
            stderrOpen
            stdoutIndex
            stderrIndex
            preferredStream
            stdoutReady
            stderrReady
            stdoutFd
            stderrFd
                | not stdoutOpen && not stderrOpen =
                    let (stdoutPendingChunks', stdoutIndex') =
                            queuePendingCapturedCompilerOutputChunks
                                CompilerStdout
                                stdoutIndex
                                (finalCompilerOutputChunk stdoutTrailingBytes)
                                stdoutPendingChunks
                        (stderrPendingChunks', stderrIndex') =
                            queuePendingCapturedCompilerOutputChunks
                                CompilerStderr
                                stderrIndex
                                (finalCompilerOutputChunk stderrTrailingBytes)
                                stderrPendingChunks
                     in pure
                            ( acc
                            , stdoutPendingChunks'
                            , stderrPendingChunks'
                            , stdoutIndex'
                            , stderrIndex'
                            )
                | otherwise =
                    case nextDrainOutputStream preferredStream stdoutOpen stderrOpen of
                        Nothing ->
                            let (stdoutPendingChunks', stdoutIndex') =
                                    queuePendingCapturedCompilerOutputChunks
                                        CompilerStdout
                                        stdoutIndex
                                        (finalCompilerOutputChunk stdoutTrailingBytes)
                                        stdoutPendingChunks
                                (stderrPendingChunks', stderrIndex') =
                                    queuePendingCapturedCompilerOutputChunks
                                        CompilerStderr
                                        stderrIndex
                                        (finalCompilerOutputChunk stderrTrailingBytes)
                                        stderrPendingChunks
                             in pure
                                    ( acc
                                    , stdoutPendingChunks'
                                    , stderrPendingChunks'
                                    , stdoutIndex'
                                    , stderrIndex'
                                    )
                        Just outputStream -> do
                            (acc', stdoutTrailingBytes', stderrTrailingBytes', stdoutIndex', stderrIndex') <-
                                flushPendingTrailingOutputChunkBefore
                                    outputStream
                                    acc
                                    stdoutTrailingBytes
                                    stderrTrailingBytes
                                    stdoutIndex
                                    stderrIndex
                            let (outputFd, trailingBytes) =
                                    case outputStream of
                                        CompilerStdout ->
                                            (stdoutFd, stdoutTrailingBytes')
                                        CompilerStderr ->
                                            (stderrFd, stderrTrailingBytes')
                            maybeBytes <- readDrainedCompilerOutputByte outputFd
                            case maybeBytes of
                                Nothing ->
                                    closeDrainedCompilerOutputStreamAfterExit
                                        outputStream
                                        acc'
                                        stdoutPendingChunks
                                        stderrPendingChunks
                                        stdoutTrailingBytes'
                                        stderrTrailingBytes'
                                        stdoutOpen
                                        stderrOpen
                                        stdoutIndex'
                                        stderrIndex'
                                        preferredStream
                                        stdoutReady
                                        stderrReady
                                        stdoutFd
                                        stderrFd
                                Just CompilerOutputReadEOF ->
                                    closeDrainedCompilerOutputStreamAfterExit
                                        outputStream
                                        acc'
                                        stdoutPendingChunks
                                        stderrPendingChunks
                                        stdoutTrailingBytes'
                                        stderrTrailingBytes'
                                        stdoutOpen
                                        stderrOpen
                                        stdoutIndex'
                                        stderrIndex'
                                        preferredStream
                                        stdoutReady
                                        stderrReady
                                        stdoutFd
                                        stderrFd
                                Just CompilerOutputReadWouldBlock -> do
                                    postExitSatisfied <- postExitDrainSatisfied
                                    processGroupStillAlive <- compilerProcessGroupAlive processGroupId
                                    if postExitSatisfied || not processGroupStillAlive
                                        then
                                            closeDrainedCompilerOutputStreamAfterExit
                                                outputStream
                                                acc'
                                                stdoutPendingChunks
                                                stderrPendingChunks
                                                stdoutTrailingBytes'
                                                stderrTrailingBytes'
                                                stdoutOpen
                                                stderrOpen
                                                stdoutIndex'
                                                stderrIndex'
                                                preferredStream
                                                stdoutReady
                                                stderrReady
                                                stdoutFd
                                                stderrFd
                                        else do
                                            maybeOutputStream <-
                                                waitForDrainedCompilerOutputStreamAfterExit
                                                    preferredStream
                                                    stdoutOpen
                                                    stderrOpen
                                                    stdoutReady
                                                    stderrReady
                                            case maybeOutputStream of
                                                Just preferredStream' ->
                                                    drainCapturedCompilerOutputAfterExit
                                                        acc'
                                                        stdoutPendingChunks
                                                        stderrPendingChunks
                                                        stdoutTrailingBytes'
                                                        stderrTrailingBytes'
                                                        stdoutOpen
                                                        stderrOpen
                                                        stdoutIndex'
                                                        stderrIndex'
                                                        preferredStream'
                                                        stdoutReady
                                                        stderrReady
                                                        stdoutFd
                                                        stderrFd
                                                Nothing ->
                                                    drainCapturedCompilerOutputAfterExit
                                                        acc'
                                                        stdoutPendingChunks
                                                        stderrPendingChunks
                                                        stdoutTrailingBytes'
                                                        stderrTrailingBytes'
                                                        stdoutOpen
                                                        stderrOpen
                                                        stdoutIndex'
                                                        stderrIndex'
                                                        preferredStream
                                                        stdoutReady
                                                        stderrReady
                                                        stdoutFd
                                                        stderrFd
                                Just (CompilerOutputReadBytes bytes) -> do
                                    let (completedChunks, remainingTrailingBytes) =
                                            splitCompleteCompilerOutputChunks (trailingBytes <> bytes)
                                        readyChunks =
                                            completedChunks
                                                <> finalCompilerOutputChunk remainingTrailingBytes
                                        readyChunkCount = length readyChunks
                                    ( acc''
                                        , stdoutPendingChunks'
                                        , stderrPendingChunks'
                                        , stdoutIndex''
                                        , stderrIndex''
                                        ) <-
                                            captureCompletedCompilerOutputChunks
                                                outputStream
                                                readyChunks
                                                acc'
                                                stdoutPendingChunks
                                                stderrPendingChunks
                                                stdoutIndex'
                                                stderrIndex'
                                    let preferredStream'
                                            | stdoutOpen && stderrOpen = flipCompilerOutputStream outputStream
                                            | otherwise = preferredStream
                                    if readyChunkCount > 0
                                        then
                                            drainCapturedCompilerOutputAfterExit
                                                acc''
                                                stdoutPendingChunks'
                                                stderrPendingChunks'
                                                B.empty
                                                B.empty
                                                stdoutOpen
                                                stderrOpen
                                                stdoutIndex''
                                                stderrIndex''
                                                preferredStream'
                                                stdoutReady
                                                stderrReady
                                                stdoutFd
                                                stderrFd
                                        else
                                            closeDrainedCompilerOutputStreamAfterExit
                                                outputStream
                                                acc''
                                                stdoutPendingChunks'
                                                stderrPendingChunks'
                                                B.empty
                                                B.empty
                                                stdoutOpen
                                                stderrOpen
                                                stdoutIndex''
                                                stderrIndex''
                                                preferredStream'
                                                stdoutReady
                                                stderrReady
                                                stdoutFd
                                                stderrFd

        closeDrainedCompilerOutputStreamAfterExit
            outputStream
            acc
            stdoutPendingChunks
            stderrPendingChunks
            stdoutTrailingBytes
            stderrTrailingBytes
            stdoutOpen
            stderrOpen
            stdoutIndex
            stderrIndex
            preferredStream
            stdoutReady
            stderrReady
            stdoutFd
            stderrFd =
                case outputStream of
                    CompilerStdout ->
                        let (stdoutPendingChunks', stdoutIndex') =
                                queuePendingCapturedCompilerOutputChunks
                                    CompilerStdout
                                    stdoutIndex
                                    (finalCompilerOutputChunk stdoutTrailingBytes)
                                    stdoutPendingChunks
                         in drainCapturedCompilerOutputAfterExit
                                acc
                                stdoutPendingChunks'
                                stderrPendingChunks
                                B.empty
                                stderrTrailingBytes
                                False
                                stderrOpen
                                stdoutIndex'
                                stderrIndex
                                preferredStream
                                stdoutReady
                                stderrReady
                                stdoutFd
                                stderrFd
                    CompilerStderr ->
                        let (stderrPendingChunks', stderrIndex') =
                                queuePendingCapturedCompilerOutputChunks
                                    CompilerStderr
                                    stderrIndex
                                    (finalCompilerOutputChunk stderrTrailingBytes)
                                    stderrPendingChunks
                         in drainCapturedCompilerOutputAfterExit
                                acc
                                stdoutPendingChunks
                                stderrPendingChunks'
                                stdoutTrailingBytes
                                B.empty
                                stdoutOpen
                                False
                                stdoutIndex
                                stderrIndex'
                                preferredStream
                                stdoutReady
                                stderrReady
                                stdoutFd
                                stderrFd

        nextDrainOutputStream preferredStream stdoutOpen stderrOpen =
            case preferredStream of
                CompilerStdout
                    | stdoutOpen -> Just CompilerStdout
                    | stderrOpen -> Just CompilerStderr
                CompilerStderr
                    | stderrOpen -> Just CompilerStderr
                    | stdoutOpen -> Just CompilerStdout
                _ ->
                    Nothing

        waitForDrainedCompilerOutputStreamAfterExit
            :: CompilerOutputStream
            -> Bool
            -> Bool
            -> STM ()
            -> STM ()
            -> IO (Maybe CompilerOutputStream)
        waitForDrainedCompilerOutputStreamAfterExit preferredStream stdoutOpen stderrOpen stdoutReady stderrReady =
            fromMaybe Nothing
                <$> timeout
                    compilerOutputDrainAfterExitPollMicros
                    (atomically $ waitForNextOutputStreamStm preferredStream stdoutOpen stderrOpen stdoutReady stderrReady)

        readCompilerOutputByte fd =
            catchIOError
                ( do
                    bytes <- PB.fdRead fd (fromIntegral compilerOutputReadChunkSize)
                    pure . Just $
                        if B.null bytes
                            then CompilerOutputReadEOF
                            else CompilerOutputReadBytes bytes
                )
                ( \ioErr ->
                    if isEOFError ioErr
                        then pure Nothing
                        else
                            if ioeGetErrorType ioErr == ResourceExhausted
                                then pure $ Just CompilerOutputReadWouldBlock
                                else ioError ioErr
                )

        readDrainedCompilerOutputByte fd =
            catchIOError
                ( do
                    bytes <- PB.fdRead fd (fromIntegral compilerOutputReadChunkSize)
                    pure . Just $
                        if B.null bytes
                            then CompilerOutputReadEOF
                            else CompilerOutputReadBytes bytes
                )
                ( \ioErr ->
                    if isEOFError ioErr
                        then pure Nothing
                        else
                            if ioeGetErrorType ioErr == ResourceExhausted
                                then pure $ Just CompilerOutputReadWouldBlock
                                else ioError ioErr
                )

compilerOutputReadChunkSize :: Int
compilerOutputReadChunkSize = 4096

compilerOutputDrainAfterExitPollMicros :: Int
compilerOutputDrainAfterExitPollMicros = 50000

minimumStableCompilerObjectOutputBytes :: Integer
minimumStableCompilerObjectOutputBytes = 20

data CompilerOutputReadResult
    = CompilerOutputReadEOF
    | CompilerOutputReadWouldBlock
    | CompilerOutputReadBytes !B.ByteString

pendingTrailingOutputChunkCount :: B.ByteString -> Int
pendingTrailingOutputChunkCount trailingBytes
    | B.null trailingBytes =
        0
    | otherwise =
        1

buildCapturedCompilerOutputChunks
    :: CompilerOutputStream
    -> Int
    -> [CompilerOutputChunk]
    -> [CapturedCompilerOutputChunk]
buildCapturedCompilerOutputChunks outputStream startIndex =
    zipWith mkCapturedChunk [startIndex ..]
    where
        mkCapturedChunk outputIndex outputChunk =
            CapturedCompilerOutputChunk
                { capturedCompilerOutputStream = outputStream
                , capturedCompilerOutputIndex = outputIndex
                , capturedCompilerOutputChunk = outputChunk
                }

emitTrailingOutputChunk
    :: (a -> CapturedCompilerOutputChunk -> IO a)
    -> CompilerOutputStream
    -> Int
    -> B.ByteString
    -> a
    -> IO a
emitTrailingOutputChunk accumulateChunk outputStream nextIndex trailingBytes acc =
    foldM
        accumulateChunk
        acc
        (buildCapturedCompilerOutputChunks outputStream nextIndex (finalCompilerOutputChunk trailingBytes))

flipCompilerOutputStream :: CompilerOutputStream -> CompilerOutputStream
flipCompilerOutputStream = \case
    CompilerStdout -> CompilerStderr
    CompilerStderr -> CompilerStdout

emptyIncrementalStreamWarningSuppressionState :: IncrementalStreamWarningSuppressionState
emptyIncrementalStreamWarningSuppressionState =
    IncrementalStreamWarningSuppressionState
        { incrementalStreamWarningPendingChunk = Nothing
        , incrementalStreamWarningChunkFilter = emptyIncrementalCompilerWarningFilter
        }

emptyIncrementalCompilerWarningSuppressionState :: IncrementalCompilerWarningSuppressionState
emptyIncrementalCompilerWarningSuppressionState =
    IncrementalCompilerWarningSuppressionState
        { incrementalCompilerWarningStdoutState = emptyIncrementalStreamWarningSuppressionState
        , incrementalCompilerWarningStderrState = emptyIncrementalStreamWarningSuppressionState
        , incrementalCompilerWarningChunkFilter = emptyIncrementalCompilerWarningFilter
        , incrementalCompilerWarningPending = []
        , incrementalCompilerWarningDecisions = Map.empty
        }

processIncrementalCompilerWarningSuppressionChunk
    :: Handle
    -> Handle
    -> IncrementalCompilerWarningSuppressionState
    -> CapturedCompilerOutputChunk
    -> IO IncrementalCompilerWarningSuppressionState
processIncrementalCompilerWarningSuppressionChunk stdoutHandle stderrHandle suppressionState capturedChunk =
    flushIncrementalCompilerWarningSuppressionState stdoutHandle stderrHandle $
        applyCompilerWarningFilterDecisions decisions suppressionState''
    where
        outputStream = capturedCompilerOutputStream capturedChunk
        streamState =
            streamWarningSuppressionState outputStream suppressionState
        (streamState', maybeCompletedChunk, localDecisions) =
            feedIncrementalStreamWarningSuppressionState streamState capturedChunk
        suppressionState' =
            setStreamWarningSuppressionState outputStream streamState' $
                suppressionState
                    { incrementalCompilerWarningPending =
                        incrementalCompilerWarningPending suppressionState <> [capturedChunk]
                    }
        (chunkFilter', completedChunkDecisions) =
            case maybeCompletedChunk of
                Nothing ->
                    (incrementalCompilerWarningChunkFilter suppressionState', [])
                Just completedChunk ->
                    feedIncrementalCompilerWarningFilter
                        (fst . suppressibleCapturedCompilerOutputChunk)
                        (snd . suppressibleCapturedCompilerOutputChunk)
                        (incrementalCompilerWarningChunkFilter suppressionState')
                        [completedChunk]
        suppressionState'' =
            suppressionState'
                { incrementalCompilerWarningChunkFilter = chunkFilter'
                }
        decisions =
            localDecisions <> completedChunkDecisions

finalizeIncrementalCompilerWarningSuppression
    :: Handle
    -> Handle
    -> IncrementalCompilerWarningSuppressionState
    -> IO IncrementalCompilerWarningSuppressionState
finalizeIncrementalCompilerWarningSuppression stdoutHandle stderrHandle suppressionState =
    flushIncrementalCompilerWarningSuppressionState stdoutHandle stderrHandle $
        applyCompilerWarningFilterDecisions finalDecisions suppressionState'
    where
        pendingPartialChunks =
            orderPendingSuppressibleChunks
                (incrementalCompilerWarningPending suppressionState)
                (mapMaybe streamPendingSuppressibleCapturedCompilerOutputChunk
                    [ incrementalCompilerWarningStdoutState suppressionState
                    , incrementalCompilerWarningStderrState suppressionState
                    ]
                )
        (chunkFilter', pendingChunkDecisions) =
            foldl'
                feedPendingSuppressibleChunk
                ( incrementalCompilerWarningChunkFilter suppressionState
                , []
                )
                pendingPartialChunks
        finalDecisions =
            pendingChunkDecisions
                <> finalizeIncrementalCompilerWarningFilter
                    (fst . suppressibleCapturedCompilerOutputChunk)
                    (snd . suppressibleCapturedCompilerOutputChunk)
                    chunkFilter'
        suppressionState' =
            suppressionState
                { incrementalCompilerWarningStdoutState = emptyIncrementalStreamWarningSuppressionState
                , incrementalCompilerWarningStderrState = emptyIncrementalStreamWarningSuppressionState
                , incrementalCompilerWarningChunkFilter = chunkFilter'
                }
        feedPendingSuppressibleChunk (chunkFilter, accumulatedDecisions) suppressibleChunk =
            let (nextChunkFilter, chunkDecisions) =
                    feedIncrementalCompilerWarningFilter
                        (fst . suppressibleCapturedCompilerOutputChunk)
                        (snd . suppressibleCapturedCompilerOutputChunk)
                        chunkFilter
                        [suppressibleChunk]
             in (nextChunkFilter, accumulatedDecisions <> chunkDecisions)

feedIncrementalStreamWarningSuppressionState
    :: IncrementalStreamWarningSuppressionState
    -> CapturedCompilerOutputChunk
    -> ( IncrementalStreamWarningSuppressionState
       , Maybe SuppressibleCapturedCompilerOutputChunk
       , [CompilerWarningFilterDecision SuppressibleCapturedCompilerOutputChunk]
       )
feedIncrementalStreamWarningSuppressionState streamState capturedChunk =
    let pendingChunk =
            appendCapturedCompilerOutputChunk
                (incrementalStreamWarningPendingChunk streamState)
                capturedChunk
        suppressibleChunk =
            buildSuppressibleCapturedCompilerOutputChunk pendingChunk
     in if capturedCompilerOutputChunkEndsLine capturedChunk
            then
                ( streamState
                    { incrementalStreamWarningPendingChunk = Nothing
                    }
                , Just suppressibleChunk
                , []
                )
            else
                if incompleteCompilerOutputNeedsMoreInputForWarningSuppression
                    (snd $ suppressibleCapturedCompilerOutputChunk suppressibleChunk)
                    then
                        -- Keep only warning-like incomplete chunks buffered so
                        -- safe interactive output still reaches the terminal
                        -- before the wrapped tool emits a newline or exits.
                        ( streamState
                            { incrementalStreamWarningPendingChunk = Just pendingChunk
                            }
                        , Nothing
                        , []
                        )
                    else
                        ( streamState
                            { incrementalStreamWarningPendingChunk = Nothing
                            }
                        , Just suppressibleChunk
                        , []
                        )

streamPendingSuppressibleCapturedCompilerOutputChunk
    :: IncrementalStreamWarningSuppressionState
    -> Maybe SuppressibleCapturedCompilerOutputChunk
streamPendingSuppressibleCapturedCompilerOutputChunk streamState =
    buildSuppressibleCapturedCompilerOutputChunk
        <$> incrementalStreamWarningPendingChunk streamState

orderPendingSuppressibleChunks
    :: [CapturedCompilerOutputChunk]
    -> [SuppressibleCapturedCompilerOutputChunk]
    -> [SuppressibleCapturedCompilerOutputChunk]
orderPendingSuppressibleChunks pendingChunks =
    sortOn earliestPendingChunkPosition
    where
        pendingChunkPositions =
            Map.fromList $
                zip (map capturedCompilerOutputKey pendingChunks) [0 :: Int ..]
        earliestPendingChunkPosition suppressibleChunk =
            minimum $
                map
                    (\outputKey -> Map.findWithDefault maxBound outputKey pendingChunkPositions)
                    (suppressibleCapturedCompilerOutputKeys suppressibleChunk)

streamWarningSuppressionState
    :: CompilerOutputStream
    -> IncrementalCompilerWarningSuppressionState
    -> IncrementalStreamWarningSuppressionState
streamWarningSuppressionState outputStream suppressionState =
    case outputStream of
        CompilerStdout ->
            incrementalCompilerWarningStdoutState suppressionState
        CompilerStderr ->
            incrementalCompilerWarningStderrState suppressionState

setStreamWarningSuppressionState
    :: CompilerOutputStream
    -> IncrementalStreamWarningSuppressionState
    -> IncrementalCompilerWarningSuppressionState
    -> IncrementalCompilerWarningSuppressionState
setStreamWarningSuppressionState outputStream streamState suppressionState =
    case outputStream of
        CompilerStdout ->
            suppressionState
                { incrementalCompilerWarningStdoutState = streamState
                }
        CompilerStderr ->
            suppressionState
                { incrementalCompilerWarningStderrState = streamState
                }

applyCompilerWarningFilterDecisions
    :: [CompilerWarningFilterDecision SuppressibleCapturedCompilerOutputChunk]
    -> IncrementalCompilerWarningSuppressionState
    -> IncrementalCompilerWarningSuppressionState
applyCompilerWarningFilterDecisions decisions suppressionState =
    suppressionState
        { incrementalCompilerWarningDecisions =
            foldl'
                applyCompilerWarningFilterDecision
                (incrementalCompilerWarningDecisions suppressionState)
                decisions
        }

applyCompilerWarningFilterDecision
    :: Map.Map CapturedCompilerOutputKey CapturedCompilerOutputDecision
    -> CompilerWarningFilterDecision SuppressibleCapturedCompilerOutputChunk
    -> Map.Map CapturedCompilerOutputKey CapturedCompilerOutputDecision
applyCompilerWarningFilterDecision decisionMap decision =
    foldl'
        (\decisionMap' outputKey -> Map.insert outputKey capturedDecision decisionMap')
        decisionMap
        outputKeys
    where
        (capturedDecision, suppressibleChunk) =
            case decision of
                RetainCompilerWarningFilterChunk chunk ->
                    (RetainCapturedCompilerOutput, chunk)
                SuppressCompilerWarningFilterChunk chunk ->
                    (SuppressCapturedCompilerOutput, chunk)
        outputKeys =
            suppressibleCapturedCompilerOutputKeys suppressibleChunk

flushIncrementalCompilerWarningSuppressionState
    :: Handle
    -> Handle
    -> IncrementalCompilerWarningSuppressionState
    -> IO IncrementalCompilerWarningSuppressionState
flushIncrementalCompilerWarningSuppressionState stdoutHandle stderrHandle suppressionState =
    go
        (incrementalCompilerWarningPending suppressionState)
        (incrementalCompilerWarningDecisions suppressionState)
        []
    where
        go [] decisionMap reversedPendingChunks =
            pure $
                suppressionState
                    { incrementalCompilerWarningPending = reverse reversedPendingChunks
                    , incrementalCompilerWarningDecisions = decisionMap
                    }
        go (capturedChunk:remainingChunks) decisionMap reversedPendingChunks =
            case Map.lookup (capturedCompilerOutputKey capturedChunk) decisionMap of
                Nothing ->
                    go remainingChunks decisionMap (capturedChunk : reversedPendingChunks)
                Just capturedDecision -> do
                    when (capturedDecision == RetainCapturedCompilerOutput) $
                        replayCapturedCompilerOutputBytes
                            (capturedCompilerOutputDestinationHandle stdoutHandle stderrHandle capturedChunk)
                            (fst $ capturedCompilerOutputChunk capturedChunk)
                    go
                        remainingChunks
                        (Map.delete (capturedCompilerOutputKey capturedChunk) decisionMap)
                        reversedPendingChunks

capturedCompilerOutputDestinationHandle :: Handle -> Handle -> CapturedCompilerOutputChunk -> Handle
capturedCompilerOutputDestinationHandle stdoutHandle stderrHandle capturedChunk =
    case capturedCompilerOutputStream capturedChunk of
        CompilerStdout -> stdoutHandle
        CompilerStderr -> stderrHandle

takeCapturedResult :: MVar (Either SomeException a) -> IO a
takeCapturedResult outputVar =
    takeMVar outputVar >>= either throwIO pure

compilerOutputBytesForStream
    :: CompilerOutputStream
    -> [CapturedCompilerOutputChunk]
    -> B.ByteString
compilerOutputBytesForStream outputStream =
    B.concat
        . map (fst . capturedCompilerOutputChunk)
        . filter ((== outputStream) . capturedCompilerOutputStream)

capturedCompilerOutputKey
    :: CapturedCompilerOutputChunk
    -> CapturedCompilerOutputKey
capturedCompilerOutputKey capturedChunk =
    ( capturedCompilerOutputStream capturedChunk
    , capturedCompilerOutputIndex capturedChunk
    )

appendCapturedCompilerOutputChunk
    :: Maybe ([CapturedCompilerOutputKey], [B.ByteString])
    -> CapturedCompilerOutputChunk
    -> ([CapturedCompilerOutputKey], [B.ByteString])
appendCapturedCompilerOutputChunk maybePending capturedChunk =
    let (pendingKeys, pendingBytes) = fromMaybe ([], []) maybePending
        (chunkBytes, _) = capturedCompilerOutputChunk capturedChunk
     in ( capturedCompilerOutputKey capturedChunk : pendingKeys
        , chunkBytes : pendingBytes
        )

buildSuppressibleCapturedCompilerOutputChunk
    :: ([CapturedCompilerOutputKey], [B.ByteString])
    -> SuppressibleCapturedCompilerOutputChunk
buildSuppressibleCapturedCompilerOutputChunk (reversedKeys, reversedBytes) =
    let chunkBytes = B.concat $ reverse reversedBytes
     in SuppressibleCapturedCompilerOutputChunk
            { suppressibleCapturedCompilerOutputKeys = reverse reversedKeys
            , suppressibleCapturedCompilerOutputChunk =
                (chunkBytes, normalizeCompilerOutputLine chunkBytes)
            }

capturedCompilerOutputChunkEndsLine :: CapturedCompilerOutputChunk -> Bool
capturedCompilerOutputChunkEndsLine capturedChunk =
    let (chunkBytes, _) = capturedCompilerOutputChunk capturedChunk
     in not (B.null chunkBytes) && B.last chunkBytes == newlineByte

groupCapturedCompilerOutputChunksForWarningSuppression
    :: [CapturedCompilerOutputChunk]
    -> [SuppressibleCapturedCompilerOutputChunk]
groupCapturedCompilerOutputChunksForWarningSuppression capturedChunks =
    orderPendingSuppressibleChunks capturedChunks (completedChunks <> pendingChunks)
    where
        (incrementalStdoutPendingChunk, incrementalStderrPendingChunk, completedChunks) =
            foldl'
                step
                (Nothing, Nothing, [])
                capturedChunks
        pendingChunks =
            mapMaybe
                (fmap buildSuppressibleCapturedCompilerOutputChunk)
                [ incrementalStdoutPendingChunk
                , incrementalStderrPendingChunk
                ]

        step (stdoutPendingChunk, stderrPendingChunk, accumulatedChunks) capturedChunk =
            let pendingChunk =
                    appendCapturedCompilerOutputChunk
                        (case capturedCompilerOutputStream capturedChunk of
                            CompilerStdout -> stdoutPendingChunk
                            CompilerStderr -> stderrPendingChunk
                        )
                        capturedChunk
                completedChunk =
                    [ buildSuppressibleCapturedCompilerOutputChunk pendingChunk
                    | capturedCompilerOutputChunkEndsLine capturedChunk
                    ]
             in case capturedCompilerOutputStream capturedChunk of
                    CompilerStdout ->
                        ( if null completedChunk then Just pendingChunk else Nothing
                        , stderrPendingChunk
                        , accumulatedChunks <> completedChunk
                        )
                    CompilerStderr ->
                        ( stdoutPendingChunk
                        , if null completedChunk then Just pendingChunk else Nothing
                        , accumulatedChunks <> completedChunk
                        )

compilerOutputBytesForSuppressibleChunks
    :: CompilerOutputStream
    -> [SuppressibleCapturedCompilerOutputChunk]
    -> B.ByteString
compilerOutputBytesForSuppressibleChunks outputStream =
    B.concat
        . map (fst . suppressibleCapturedCompilerOutputChunk)
        . filter
            ( \capturedChunk ->
                maybe False ((== outputStream) . fst) $
                    case suppressibleCapturedCompilerOutputKeys capturedChunk of
                        outputKey:_ -> Just outputKey
                        []          -> Nothing
            )

readCompilerProcessWithExitCodeProbeSuppressingWarnings
    :: CompilerCommand
    -> [String]
    -> IO (ExitCode, String, String)
readCompilerProcessWithExitCodeProbeSuppressingWarnings =
    readCompilerProcessWithExitCodeProbeSuppressingWarningsUntil (\_ -> pure False)

readCompilerProcessWithExitCodeProbeSuppressingWarningsUntil
    :: (IO [CapturedCompilerOutputChunk] -> IO Bool)
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, String, String)
readCompilerProcessWithExitCodeProbeSuppressingWarningsUntil postExitDrainSatisfied compiler extraArgs = do
    (exitCode, capturedChunks) <-
        readCompilerProcessWithExitCodeChunksUntil
            postExitDrainSatisfied
            CreatePipe
            compiler
            extraArgs
    let retainedChunks =
            filterCompilerOutputChunks
                (fst . suppressibleCapturedCompilerOutputChunk)
                (snd . suppressibleCapturedCompilerOutputChunk)
                (groupCapturedCompilerOutputChunksForWarningSuppression capturedChunks)
    pure
        ( exitCode
        , BC.unpack $
            compilerOutputBytesForSuppressibleChunks CompilerStdout retainedChunks
        , BC.unpack $
            compilerOutputBytesForSuppressibleChunks CompilerStderr retainedChunks
        )

readCompilerProcessWithExitCodeProbe
    :: Bool
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, String, String)
readCompilerProcessWithExitCodeProbe =
    readCompilerProcessWithExitCodeProbeUntil (\_ -> pure False)

readCompilerProcessWithExitCodeProbeUntil
    :: (IO [CapturedCompilerOutputChunk] -> IO Bool)
    -> Bool
    -> CompilerCommand
    -> [String]
    -> IO (ExitCode, String, String)
readCompilerProcessWithExitCodeProbeUntil postExitDrainSatisfied suppressWarnsOutput compiler extraArgs
    | suppressWarnsOutput =
        readCompilerProcessWithExitCodeProbeSuppressingWarningsUntil
            postExitDrainSatisfied
            compiler
            extraArgs
    | otherwise = do
        (exitCode, stdoutBytes, stderrBytes) <-
            readCompilerProcessWithExitCodeBytesUntil
                postExitDrainSatisfied
                compiler
                extraArgs
        pure (exitCode, BC.unpack stdoutBytes, BC.unpack stderrBytes)

callCompilerProcess :: Bool -> CompilerCommand -> [String] -> IO ()
callCompilerProcess =
    callCompilerProcessUntil (pure False)

callCompilerProcessUntil :: IO Bool -> Bool -> CompilerCommand -> [String] -> IO ()
callCompilerProcessUntil postExitDrainSatisfied suppressWarnsOutput compiler extraArgs
    | suppressWarnsOutput = do
        (exitCode, _) <-
            bracket (hDuplicate stdout) hClose $ \stdoutHandle ->
                bracket (hDuplicate stderr) hClose $ \stderrHandle -> do
                    hSetBinaryMode stdoutHandle True
                    hSetBinaryMode stderrHandle True
                    (exitCode', suppressionState') <-
                        foldCompilerProcessWithExitCodeChunksUntil
                            postExitDrainSatisfied
                            Inherit
                            compiler
                            extraArgs
                            emptyIncrementalCompilerWarningSuppressionState
                            (processIncrementalCompilerWarningSuppressionChunk stdoutHandle stderrHandle)
                    finalizedSuppressionState <-
                        finalizeIncrementalCompilerWarningSuppression
                            stdoutHandle
                            stderrHandle
                            suppressionState'
                    pure (exitCode', finalizedSuppressionState)
        handleCompilerProcessExit compiler extraArgs exitCode
    | otherwise = do
        processEnv <- compilerProcessEnv compiler
        (_, _, _, processHandle) <- createProcess
            (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
                { env = processEnv
                , create_group = True
                }
        processGroupId <- compilerProcessGroupIdForHandle processHandle
        exitCode <- waitForProcess processHandle
        when (exitCode == ExitSuccess) $
            waitForCompilerProcessPostExitCompletion processGroupId postExitDrainSatisfied
        handleCompilerProcessExit compiler extraArgs exitCode

handleCompilerProcessExit :: CompilerCommand -> [String] -> ExitCode -> IO ()
handleCompilerProcessExit compiler extraArgs = \case
    ExitSuccess -> pure ()
    exitCode ->
        ioError . userError $
            showCompilerCommandForUser compiler extraArgs
                <> " failed with "
                <> show exitCode

replayCapturedCompilerOutputBytes :: Handle -> B.ByteString -> IO ()
replayCapturedCompilerOutputBytes destination bytes
    | B.null bytes = pure ()
    | otherwise = do
        B.hPut destination bytes
        hFlush destination

withReadableFile :: FilePath -> FileMode -> IO a -> IO a
withReadableFile path originalMode action
    | intersectFileModes originalMode ownerReadMode /= 0 = action
    | otherwise = do
        setFileMode path readableMode
        action `finally` setFileMode path originalMode
    where
        readableMode = originalMode `unionFileModes` ownerReadMode

shouldValidateRunnableLinkedOutput :: FilePath -> IO Bool
shouldValidateRunnableLinkedOutput path =
    catchIOError
        (isRegularFile <$> getSymbolicLinkStatus path)
        (\ioErr -> if isDoesNotExistError ioErr then pure True else ioError ioErr)

validateRunnableLinkedOutput :: FilePath -> Maybe String -> IO Bool
validateRunnableLinkedOutput path maybeProbeMarker =
    catchIOError
        (do
            status <- getSymbolicLinkStatus path
            if isRegularFile status
                then do
                    let originalMode = fileMode status
                        hasExecuteBits =
                            intersectFileModes originalMode executableFileMode /= 0
                    if not hasExecuteBits
                        then pure False
                        else do
                            withReadableFile path originalMode $ do
                                bytes <- B.readFile path
                                let markerPresent = maybe
                                        True
                                        (`probeMarkerPresent` bytes)
                                        maybeProbeMarker
                                    linkedOutputOk =
                                        maybe
                                            False
                                            hasRunnableLinkedElfProgramHeadersAndInterpreter
                                            (parseLinkedOutputElf bytes)
                                pure $ markerPresent && linkedOutputOk
                else pure False
        )
        (\ioErr -> if isDoesNotExistError ioErr then pure False else ioError ioErr)

probeMarkerPresent :: String -> B.ByteString -> Bool
probeMarkerPresent probeMarker =
    B.isInfixOf $ B.pack $ map (fromIntegral . fromEnum) probeMarker

data LinkedOutputElf = LinkedOutputElf
    { linkedOutputElfBytes                  :: B.ByteString
    , linkedOutputElfDataEncoding           :: Word8
    , linkedOutputElfOsAbi                  :: Word8
    , linkedOutputElfType                   :: Int
    , linkedOutputElfFileSize               :: Word64
    , linkedOutputElfEntryPoint             :: Word64
    , linkedOutputElfProgramHeaderOffset    :: Word64
    , linkedOutputElfProgramHeaderEntrySize :: Int
    , linkedOutputElfProgramHeaderCount     :: Int
    }

looksRunnableLinkedOutput :: B.ByteString -> Bool
looksRunnableLinkedOutput bytes =
    maybe False hasRunnableLinkedElfProgramHeadersAndInterpreter $
        parseLinkedOutputElf bytes

linkedOutputElfHasStandaloneInterpreterDynamicSection :: LinkedOutputElf -> Bool
linkedOutputElfHasStandaloneInterpreterDynamicSection elf =
    any hasStandaloneInterpreterDynamicSection [0 .. linkedOutputElfProgramHeaderCount elf - 1]
    where
        hasStandaloneInterpreterDynamicSection headerIndex =
            let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
             in linkedOutputElfHasValidProgramHeaderBounds elf headerOffset
                    && linkedOutputElfProgramHeaderType elf headerOffset == elfProgramHeaderTypeDynamic
                    && maybe
                        False
                        (uncurry $ linkedOutputElfDynamicEntriesDescribeStandaloneInterpreter elf)
                        (linkedOutputElfProgramHeaderFileRange elf headerOffset)

linkedOutputElfDynamicEntriesDescribeStandaloneInterpreter
    :: LinkedOutputElf
    -> Word64
    -> Word64
    -> Bool
linkedOutputElfDynamicEntriesDescribeStandaloneInterpreter elf dynamicOffset dynamicSize
    | dynamicSize < fromIntegral elfDynamicEntrySize = False
    | dynamicSize `mod` fromIntegral elfDynamicEntrySize /= 0 = False
    | otherwise =
        let dynamicEnd = dynamicOffset + dynamicSize
            go entryOffset
                | entryOffset >= dynamicEnd = False
                | otherwise =
                    let entryTag = linkedOutputElfDynamicEntryTag elf entryOffset
                     in entryTag == elfDynamicTagNull
                            || entryTag /= elfDynamicTagNeeded
                                && go (entryOffset + fromIntegral elfDynamicEntrySize)
         in go dynamicOffset

parseLinkedOutputElf :: B.ByteString -> Maybe LinkedOutputElf
parseLinkedOutputElf bytes
    | B.length bytes < elfHeaderSize = Nothing
    | B.take 4 bytes /= elfMagic = Nothing
    | elfClass /= elfClass64Bit = Nothing
    | elfData `notElem` [elfDataLittleEndian, elfDataBigEndian] = Nothing
    | elfIdentVersion /= elfCurrentVersion = Nothing
    | elfMachine /= elfMachineX86_64 = Nothing
    | elfType `notElem` [elfTypeExecutable, elfTypeSharedObject] = Nothing
    | elfVersion /= fromIntegral elfCurrentVersion = Nothing
    | elfHeaderByteSize /= elfHeaderSize = Nothing
    | elfProgramHeaderEntrySize < elfProgramHeaderSize = Nothing
    | elfProgramHeaderCount == 0 = Nothing
    | not (rangeWithinFile elfProgramHeaderOffset elfProgramHeaderTableSize linkedFileSize) = Nothing
    | otherwise =
        Just LinkedOutputElf
            { linkedOutputElfBytes = bytes
            , linkedOutputElfDataEncoding = elfData
            , linkedOutputElfOsAbi = elfOsAbi
            , linkedOutputElfType = elfType
            , linkedOutputElfFileSize = linkedFileSize
            , linkedOutputElfEntryPoint = elfEntryPoint
            , linkedOutputElfProgramHeaderOffset = elfProgramHeaderOffset
            , linkedOutputElfProgramHeaderEntrySize = elfProgramHeaderEntrySize
            , linkedOutputElfProgramHeaderCount = elfProgramHeaderCount
            }
    where
        linkedFileSize = fromIntegral $ B.length bytes
        elfClass = B.index bytes 4
        elfData = B.index bytes 5
        elfIdentVersion = B.index bytes 6
        elfOsAbi = B.index bytes 7
        elfType = decodeElfHalfWord elfData (B.index bytes 16) (B.index bytes 17) :: Int
        elfMachine = decodeElfHalfWord elfData (B.index bytes 18) (B.index bytes 19) :: Int
        elfVersion =
            decodeElfWord32
                elfData
                [ B.index bytes 20
                , B.index bytes 21
                , B.index bytes 22
                , B.index bytes 23
                ] ::
                Int
        elfEntryPoint =
            decodeElfWord64
                elfData
                [ B.index bytes 24
                , B.index bytes 25
                , B.index bytes 26
                , B.index bytes 27
                , B.index bytes 28
                , B.index bytes 29
                , B.index bytes 30
                , B.index bytes 31
                ]
        elfProgramHeaderOffset =
            decodeElfWord64
                elfData
                [ B.index bytes 32
                , B.index bytes 33
                , B.index bytes 34
                , B.index bytes 35
                , B.index bytes 36
                , B.index bytes 37
                , B.index bytes 38
                , B.index bytes 39
                ]
        elfHeaderByteSize = decodeElfHalfWord elfData (B.index bytes 52) (B.index bytes 53) :: Int
        elfProgramHeaderEntrySize =
            decodeElfHalfWord elfData (B.index bytes 54) (B.index bytes 55) :: Int
        elfProgramHeaderCount =
            decodeElfHalfWord elfData (B.index bytes 56) (B.index bytes 57) :: Int
        elfProgramHeaderTableSize =
            fromIntegral elfProgramHeaderEntrySize * fromIntegral elfProgramHeaderCount

hasRunnableLinkedElfProgramHeadersAndInterpreter :: LinkedOutputElf -> Bool
hasRunnableLinkedElfProgramHeadersAndInterpreter elf =
    linkedOutputElfHasRunnableInterpreterLayout elf
        && any (linkedOutputElfHasRunnableProgramHeader elf) [0 .. linkedOutputElfProgramHeaderCount elf - 1]

linkedOutputElfHasRunnableInterpreterLayout :: LinkedOutputElf -> Bool
linkedOutputElfHasRunnableInterpreterLayout elf =
    case linkedOutputElfInterpreterPath elf of
        Just (Just _) ->
            True
        Just Nothing ->
            ( linkedOutputElfType elf == elfTypeExecutable
                && not (linkedOutputElfHasDynamicProgramHeader elf)
            )
                || linkedOutputElfHasStandaloneStaticPieLayout elf
        Nothing ->
            False

linkedOutputElfHasDynamicProgramHeader :: LinkedOutputElf -> Bool
linkedOutputElfHasDynamicProgramHeader elf =
    any hasDynamicProgramHeader [0 .. linkedOutputElfProgramHeaderCount elf - 1]
    where
        hasDynamicProgramHeader headerIndex =
            let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
             in linkedOutputElfHasValidProgramHeaderBounds elf headerOffset
                    && linkedOutputElfProgramHeaderType elf headerOffset == elfProgramHeaderTypeDynamic

linkedOutputElfInterpreterPath :: LinkedOutputElf -> Maybe (Maybe FilePath)
linkedOutputElfInterpreterPath elf =
    case filter isInterpreterProgramHeader [0 .. linkedOutputElfProgramHeaderCount elf - 1] of
        [] ->
            Just Nothing
        [headerIndex] -> do
            let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
                interpreterOffset = linkedOutputElfProgramHeaderFileOffset elf headerOffset
                interpreterSize = linkedOutputElfProgramHeaderFileSize elf headerOffset
            if not (linkedOutputElfHasValidProgramHeaderBounds elf headerOffset)
                || interpreterSize <= 1
                || interpreterSize > linkedOutputElfProgramHeaderMemorySize elf headerOffset
                || not (rangeWithinFile interpreterOffset interpreterSize (linkedOutputElfFileSize elf))
                then Nothing
                else do
                    interpreterBytes <- linkedOutputElfNullTerminatedBytes elf interpreterOffset interpreterSize
                    let interpreterPath = BC.unpack interpreterBytes
                    if null interpreterPath || head interpreterPath /= '/'
                        then Nothing
                        else Just $ Just interpreterPath
        _ ->
            Nothing
    where
        isInterpreterProgramHeader headerIndex =
            let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
             in linkedOutputElfHasValidProgramHeaderBounds elf headerOffset
                    && linkedOutputElfProgramHeaderType elf headerOffset == elfProgramHeaderTypeInterp

linkedOutputElfNullTerminatedBytes
    :: LinkedOutputElf
    -> Word64
    -> Word64
    -> Maybe B.ByteString
linkedOutputElfNullTerminatedBytes elf start size
    | size == 0 = Nothing
    | otherwise =
        let rawBytes =
                B.take (fromIntegral size) $
                    B.drop (fromIntegral start) $
                        linkedOutputElfBytes elf
         in case B.unsnoc rawBytes of
                Just (payloadBytes, trailingByte)
                    | trailingByte == 0 && not (B.null payloadBytes) && B.all (/= 0) payloadBytes ->
                        Just payloadBytes
                _ ->
                    Nothing

linkedOutputElfHasStaticPieDynamicFlags :: LinkedOutputElf -> Bool
linkedOutputElfHasStaticPieDynamicFlags elf =
    linkedOutputElfType elf == elfTypeSharedObject
        && any (linkedOutputElfProgramHeaderHasStaticPieFlag elf) [0 .. linkedOutputElfProgramHeaderCount elf - 1]

linkedOutputElfHasStandaloneStaticPieLayout :: LinkedOutputElf -> Bool
linkedOutputElfHasStandaloneStaticPieLayout elf =
    linkedOutputElfHasStaticPieDynamicFlags elf
        && linkedOutputElfHasStandaloneInterpreterDynamicSection elf

linkedOutputElfProgramHeaderHasStaticPieFlag :: LinkedOutputElf -> Int -> Bool
linkedOutputElfProgramHeaderHasStaticPieFlag elf headerIndex =
    let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
     in linkedOutputElfHasValidProgramHeaderBounds elf headerOffset
            && linkedOutputElfProgramHeaderType elf headerOffset == elfProgramHeaderTypeDynamic
            && maybe
                False
                (uncurry $ linkedOutputElfDynamicEntriesContainStaticPieFlag elf)
                (linkedOutputElfProgramHeaderFileRange elf headerOffset)

linkedOutputElfDynamicEntriesContainStaticPieFlag :: LinkedOutputElf -> Word64 -> Word64 -> Bool
linkedOutputElfDynamicEntriesContainStaticPieFlag elf dynamicOffset dynamicSize
    | dynamicSize < fromIntegral elfDynamicEntrySize = False
    | dynamicSize `mod` fromIntegral elfDynamicEntrySize /= 0 = False
    | otherwise =
        let dynamicEnd = dynamicOffset + dynamicSize
            go entryOffset
                | entryOffset >= dynamicEnd = False
                | otherwise =
                    let entryTag = linkedOutputElfDynamicEntryTag elf entryOffset
                        entryValue = linkedOutputElfDynamicEntryValue elf entryOffset
                     in entryTag /= elfDynamicTagNull
                            && ( ( entryTag == elfDynamicTagFlags1
                                    && entryValue .&. elfDynamicFlag1Pie /= 0
                                )
                                    || go (entryOffset + fromIntegral elfDynamicEntrySize)
                               )
         in go dynamicOffset

linkedOutputElfHasRunnableProgramHeader :: LinkedOutputElf -> Int -> Bool
linkedOutputElfHasRunnableProgramHeader elf headerIndex =
    let headerOffset = linkedOutputElfProgramHeaderEntryOffset elf headerIndex
     in linkedOutputElfHasValidProgramHeaderBounds elf headerOffset
            && linkedOutputElfProgramHeaderType elf headerOffset == elfProgramHeaderTypeLoad
            && linkedOutputElfProgramHeaderFileSize elf headerOffset > 0
            && linkedOutputElfProgramHeaderFileSize elf headerOffset
                <= linkedOutputElfProgramHeaderMemorySize elf headerOffset
            && rangeWithinFile
                (linkedOutputElfProgramHeaderFileOffset elf headerOffset)
                (linkedOutputElfProgramHeaderFileSize elf headerOffset)
                (linkedOutputElfFileSize elf)
            && linkedOutputElfProgramHeaderContainsEntryPoint elf headerOffset
            && linkedOutputElfProgramHeaderFlags elf headerOffset .&. elfProgramHeaderFlagExecute /= 0

linkedOutputElfHasValidProgramHeaderBounds :: LinkedOutputElf -> Word64 -> Bool
linkedOutputElfHasValidProgramHeaderBounds elf headerOffset =
    rangeWithinFile headerOffset (fromIntegral elfProgramHeaderSize) (linkedOutputElfFileSize elf)

linkedOutputElfProgramHeaderFileRange :: LinkedOutputElf -> Word64 -> Maybe (Word64, Word64)
linkedOutputElfProgramHeaderFileRange elf headerOffset =
    let fileOffset = linkedOutputElfProgramHeaderFileOffset elf headerOffset
        segmentFileSize = linkedOutputElfProgramHeaderFileSize elf headerOffset
        memorySize = linkedOutputElfProgramHeaderMemorySize elf headerOffset
     in if segmentFileSize == 0
            || segmentFileSize > memorySize
            || not (rangeWithinFile fileOffset segmentFileSize (linkedOutputElfFileSize elf))
            then Nothing
            else Just (fileOffset, segmentFileSize)

linkedOutputElfProgramHeaderEntryOffset :: LinkedOutputElf -> Int -> Word64
linkedOutputElfProgramHeaderEntryOffset elf headerIndex =
    linkedOutputElfProgramHeaderOffset elf
        + fromIntegral headerIndex * fromIntegral (linkedOutputElfProgramHeaderEntrySize elf)

linkedOutputElfProgramHeaderType :: LinkedOutputElf -> Word64 -> Int
linkedOutputElfProgramHeaderType elf headerOffset =
    decodeElfWord32
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 0
        , linkedOutputElfByteAt elf headerOffset 1
        , linkedOutputElfByteAt elf headerOffset 2
        , linkedOutputElfByteAt elf headerOffset 3
        ] ::
        Int

linkedOutputElfProgramHeaderFlags :: LinkedOutputElf -> Word64 -> Int
linkedOutputElfProgramHeaderFlags elf headerOffset =
    decodeElfWord32
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 4
        , linkedOutputElfByteAt elf headerOffset 5
        , linkedOutputElfByteAt elf headerOffset 6
        , linkedOutputElfByteAt elf headerOffset 7
        ] ::
        Int

linkedOutputElfProgramHeaderFileOffset :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfProgramHeaderFileOffset elf headerOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 8
        , linkedOutputElfByteAt elf headerOffset 9
        , linkedOutputElfByteAt elf headerOffset 10
        , linkedOutputElfByteAt elf headerOffset 11
        , linkedOutputElfByteAt elf headerOffset 12
        , linkedOutputElfByteAt elf headerOffset 13
        , linkedOutputElfByteAt elf headerOffset 14
        , linkedOutputElfByteAt elf headerOffset 15
        ]

linkedOutputElfProgramHeaderVirtualAddress :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfProgramHeaderVirtualAddress elf headerOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 16
        , linkedOutputElfByteAt elf headerOffset 17
        , linkedOutputElfByteAt elf headerOffset 18
        , linkedOutputElfByteAt elf headerOffset 19
        , linkedOutputElfByteAt elf headerOffset 20
        , linkedOutputElfByteAt elf headerOffset 21
        , linkedOutputElfByteAt elf headerOffset 22
        , linkedOutputElfByteAt elf headerOffset 23
        ]

linkedOutputElfProgramHeaderFileSize :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfProgramHeaderFileSize elf headerOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 32
        , linkedOutputElfByteAt elf headerOffset 33
        , linkedOutputElfByteAt elf headerOffset 34
        , linkedOutputElfByteAt elf headerOffset 35
        , linkedOutputElfByteAt elf headerOffset 36
        , linkedOutputElfByteAt elf headerOffset 37
        , linkedOutputElfByteAt elf headerOffset 38
        , linkedOutputElfByteAt elf headerOffset 39
        ]

linkedOutputElfProgramHeaderMemorySize :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfProgramHeaderMemorySize elf headerOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf headerOffset 40
        , linkedOutputElfByteAt elf headerOffset 41
        , linkedOutputElfByteAt elf headerOffset 42
        , linkedOutputElfByteAt elf headerOffset 43
        , linkedOutputElfByteAt elf headerOffset 44
        , linkedOutputElfByteAt elf headerOffset 45
        , linkedOutputElfByteAt elf headerOffset 46
        , linkedOutputElfByteAt elf headerOffset 47
        ]

linkedOutputElfDynamicEntryTag :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfDynamicEntryTag elf entryOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf entryOffset 0
        , linkedOutputElfByteAt elf entryOffset 1
        , linkedOutputElfByteAt elf entryOffset 2
        , linkedOutputElfByteAt elf entryOffset 3
        , linkedOutputElfByteAt elf entryOffset 4
        , linkedOutputElfByteAt elf entryOffset 5
        , linkedOutputElfByteAt elf entryOffset 6
        , linkedOutputElfByteAt elf entryOffset 7
        ]

linkedOutputElfDynamicEntryValue :: LinkedOutputElf -> Word64 -> Word64
linkedOutputElfDynamicEntryValue elf entryOffset =
    decodeElfWord64
        (linkedOutputElfDataEncoding elf)
        [ linkedOutputElfByteAt elf entryOffset 8
        , linkedOutputElfByteAt elf entryOffset 9
        , linkedOutputElfByteAt elf entryOffset 10
        , linkedOutputElfByteAt elf entryOffset 11
        , linkedOutputElfByteAt elf entryOffset 12
        , linkedOutputElfByteAt elf entryOffset 13
        , linkedOutputElfByteAt elf entryOffset 14
        , linkedOutputElfByteAt elf entryOffset 15
        ]

linkedOutputElfProgramHeaderContainsEntryPoint :: LinkedOutputElf -> Word64 -> Bool
linkedOutputElfProgramHeaderContainsEntryPoint elf headerOffset =
    rangeContainsPoint
        (linkedOutputElfProgramHeaderVirtualAddress elf headerOffset)
        (linkedOutputElfProgramHeaderMemorySize elf headerOffset)
        (linkedOutputElfEntryPoint elf)

linkedOutputElfByteAt :: LinkedOutputElf -> Word64 -> Int -> Word8
linkedOutputElfByteAt elf headerOffset relativeOffset =
    B.index
        (linkedOutputElfBytes elf)
        (fromIntegral $ headerOffset + fromIntegral relativeOffset)

decodeElfHalfWord :: (Bits a, Num a) => Word8 -> Word8 -> Word8 -> a
decodeElfHalfWord elfData byte18 byte19
    = decodeElfUnsigned elfData [byte18, byte19]

decodeElfWord32 :: (Bits a, Num a) => Word8 -> [Word8] -> a
decodeElfWord32 = decodeElfUnsigned

decodeElfWord64 :: (Bits a, Num a) => Word8 -> [Word8] -> a
decodeElfWord64 = decodeElfUnsigned

decodeElfUnsigned :: (Bits a, Num a) => Word8 -> [Word8] -> a
decodeElfUnsigned elfData =
    foldl'
        (\acc nextByte -> acc `shiftL` 8 .|. fromIntegral nextByte)
        0
        . orderedBytes
    where
        orderedBytes
            | elfData == elfDataBigEndian = id
            | otherwise = reverse

rangeWithinFile :: Word64 -> Word64 -> Word64 -> Bool
rangeWithinFile start size totalFileSize =
    start <= totalFileSize && size <= totalFileSize - start

rangeContainsPoint :: Word64 -> Word64 -> Word64 -> Bool
rangeContainsPoint start size point =
    size > 0 && point >= start && point - start < size

elfMagic :: B.ByteString
elfMagic = B.pack [0x7f, 0x45, 0x4c, 0x46]

elfClass64Bit :: Word8
elfClass64Bit = 2

elfCurrentVersion :: Word8
elfCurrentVersion = 1

elfDataLittleEndian :: Word8
elfDataLittleEndian = 1

elfDataBigEndian :: Word8
elfDataBigEndian = 2

elfOsAbiSystemV :: Word8
elfOsAbiSystemV = 0

elfOsAbiLinux :: Word8
elfOsAbiLinux = 3

elfOsAbiFreeBsd :: Word8
elfOsAbiFreeBsd = 9

elfTypeExecutable :: Int
elfTypeExecutable = 2

elfTypeSharedObject :: Int
elfTypeSharedObject = 3

elfMachineX86_64 :: Int
elfMachineX86_64 = 62

elfHeaderSize :: Int
elfHeaderSize = 64

elfProgramHeaderSize :: Int
elfProgramHeaderSize = 56

elfProgramHeaderTypeLoad :: Int
elfProgramHeaderTypeLoad = 1

elfProgramHeaderTypeDynamic :: Int
elfProgramHeaderTypeDynamic = 2

elfProgramHeaderTypeInterp :: Int
elfProgramHeaderTypeInterp = 3

elfProgramHeaderFlagExecute :: Int
elfProgramHeaderFlagExecute = 0x1

elfDynamicEntrySize :: Int
elfDynamicEntrySize = 16

elfDynamicTagNull :: Word64
elfDynamicTagNull = 0

elfDynamicTagNeeded :: Word64
elfDynamicTagNeeded = 1

elfDynamicTagFlags1 :: Word64
elfDynamicTagFlags1 = 0x6ffffffb

elfDynamicFlag1Pie :: Word64
elfDynamicFlag1Pie = 0x08000000

markerSectionAsm :: String -> String -> String
markerSectionAsm label marker = unlines
    [ ".section .rodata"
    , label <> ":"
    , "    .asciz \"" <> escapeAsmString marker <> "\""
    , ".text"
    ]

x86_64ElfRunnableLinkedOutputMarkerAsm :: String -> String
x86_64ElfRunnableLinkedOutputMarkerAsm runnableOutputMarker =
    unlines [".intel_syntax noprefix"]
        <> markerSectionAsm ".L.htcc_runnable_output_marker_payload" runnableOutputMarker
        <> unlines
            [ ".section .init_array,\"aw\",@init_array"
            , "    .quad .L.htcc_runnable_output_marker_ctor"
            , ".text"
            , ".L.htcc_runnable_output_marker_ctor:"
            , "    lea rax, [rip + .L.htcc_runnable_output_marker_payload]"
            , "    ret"
            ]

makeRunnableLinkedOutputMarker :: FilePath -> FilePath -> FilePath -> String
makeRunnableLinkedOutputMarker asmPath objPath markerObjPath =
    "htcc-output-marker:"
        <> takeFileName asmPath
        <> ":"
        <> takeFileName objPath
        <> ":"
        <> takeFileName markerObjPath

x86_64ElfProbeAsm :: String -> String
x86_64ElfProbeAsm probeMarker =
    unlines [".intel_syntax noprefix"]
        <> markerSectionAsm "htcc_probe_marker" probeMarker
        <> unlines
            [ ".global main"
            , "main:"
            , "    lea rdx, [rip + htcc_probe_marker]"
            , "    xor eax, eax"
            , "    ret"
            ]

escapeAsmString :: String -> String
escapeAsmString = concatMap $ \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    c -> [c]

asmCompiler :: Bool -> IO CompilerCommand
asmCompiler suppressWarnsOutput = do
    htccAssembler <- nonEmptyEnv <$> lookupEnv "HTCC_ASSEMBLER"
    compilerSpec <- resolveCompilerCommand $ fromMaybe "gcc" htccAssembler
    ensureX86_64ElfCompiler suppressWarnsOutput compilerSpec
    pure compilerSpec

data CompilerProbeFailure
    = CompilerAssemblyProbeFailure
    | CompilerLinkProbeFailure

ensureX86_64ElfCompiler :: Bool -> CompilerCommand -> IO ()
ensureX86_64ElfCompiler suppressWarnsOutput compilerSpec = do
    detectedTargets <- probeCompilerTargets compilerSpec
    probeResult <- probeCompilerInvocation compilerSpec
    case probeResult of
        Right target
            | isX86_64ElfTarget target -> pure ()
            | otherwise -> rejectTarget target
        Left CompilerAssemblyProbeFailure ->
            case filter isX86_64ElfTarget detectedTargets of
                _ : _ -> rejectAssemblyProbeFailure
                []    -> rejectDriverSelection
        Left CompilerLinkProbeFailure ->
            rejectLinkProbeFailure
    where
        rejectTarget target =
            ioError . userError $
                "HTCC_ASSEMBLER must target x86_64-ELF for -r (detected " <> target <> ")"

        rejectAssemblyProbeFailure =
            ioError . userError $
                "HTCC_ASSEMBLER passed target metadata probes but failed an x86_64-ELF assembly probe for -r"

        rejectDriverSelection =
            ioError . userError $
                "failed to determine an x86_64-ELF target from HTCC_ASSEMBLER; choose a compiler driver that defaults to x86_64-ELF for -r"

        rejectLinkProbeFailure =
            ioError . userError $
                "HTCC_ASSEMBLER assembled an x86_64-ELF object but failed a link probe for -r; choose a compiler driver that supports both assembly and linking for -r"

        wrapCompilerProbeIOError compilerSpec' args action =
            catchIOError
                action
                ( \ioErr ->
                    ioError . userError $
                        "failed to start HTCC_ASSEMBLER probe "
                            <> showCompilerCommandForUser compilerSpec' args
                            <> ": "
                            <> ioeGetErrorString ioErr
                )

        probeCompilerTargets compilerSpec' =
            catMaybes <$> mapM (probeCompilerTarget compilerSpec')
                [ "-dumpmachine"
                , "-print-target-triple"
                ]

        probeCompilerTarget compilerSpec' probeArg = do
            (exitCode, stdout', _) <-
                wrapCompilerProbeIOError compilerSpec' [probeArg] $
                    readCompilerProcessWithExitCodeProbeUntil
                        capturedCompilerTargetLineAvailableAfterExit
                        suppressWarnsOutput
                        compilerSpec'
                        [probeArg]
            pure $ case exitCode of
                ExitSuccess   -> nonEmptyTrimmed stdout'
                ExitFailure _ -> Nothing

        probeCompilerInvocation compilerSpec' =
            withProbeFile "htcc-probe-.s" $ \asmPath asmHandle -> do
                withProbeFile "htcc-probe-.o" $ \objPath objHandle -> do
                    withProbeFile "htcc-probe-marker-.s" $ \markerAsmPath markerAsmHandle -> do
                        withProbeFile "htcc-probe-marker-.o" $ \markerObjPath markerObjHandle -> do
                            let probeMarker =
                                    makeRunnableLinkedOutputMarker asmPath objPath markerObjPath
                            hPutStr asmHandle $ x86_64ElfProbeAsm probeMarker
                            hClose asmHandle
                            hPutStr
                                markerAsmHandle
                                (x86_64ElfRunnableLinkedOutputMarkerAsm probeMarker)
                            hClose markerAsmHandle
                            setFileMode objPath temporaryWritableMode
                            hClose objHandle
                            setFileMode markerObjPath temporaryWritableMode
                            hClose markerObjHandle
                            let assembleArgs = asmAssembleArgs objPath asmPath
                                markerAssembleArgs =
                                    asmAssembleArgs markerObjPath markerAsmPath
                            assemblePostExitDrainSatisfied <-
                                stabilizeCompilerObjectOutputAfterExit objPath
                            probeProcessResult <-
                                probeCommandExitCode
                                    compilerSpec'
                                    assembleArgs
                                    assemblePostExitDrainSatisfied
                            case probeProcessResult of
                                Just ExitSuccess -> do
                                    probeTarget <- detectProbeObjectTarget objPath
                                    case probeTarget of
                                        Just target
                                            | isX86_64ElfTarget target -> do
                                                markerAssemblePostExitDrainSatisfied <-
                                                    stabilizeCompilerObjectOutputAfterExit
                                                        markerObjPath
                                                markerProbeProcessResult <-
                                                    probeCommandExitCode
                                                        compilerSpec'
                                                        markerAssembleArgs
                                                        markerAssemblePostExitDrainSatisfied
                                                case markerProbeProcessResult of
                                                    Just ExitSuccess -> do
                                                        linkSucceeded <-
                                                            probeCompilerLink
                                                                compilerSpec'
                                                                objPath
                                                                markerObjPath
                                                                probeMarker
                                                        pure $
                                                            if linkSucceeded
                                                                then Right target
                                                                else Left CompilerLinkProbeFailure
                                                    _ ->
                                                        pure $ Left CompilerLinkProbeFailure
                                            | otherwise -> pure $ Right target
                                        Nothing ->
                                            pure $ Left CompilerAssemblyProbeFailure
                                _ ->
                                    pure $ Left CompilerAssemblyProbeFailure

        probeCompilerLink compilerSpec' objPath markerObjPath probeMarker =
            withProbeFile "htcc-probe-.out" $ \outputPath outputHandle -> do
                creationMode <- creationMaskedOutputMode
                setFileMode outputPath $
                    stagedOutputMode PreserveReplacementOutputModeKeepingExecutableBits creationMode
                hClose outputHandle
                let linkArgs = asmRunnableLinkArgs outputPath objPath markerObjPath
                linkPostExitDrainSatisfied <-
                    stabilizePostExitPredicate $
                        validateRunnableLinkedOutput outputPath (Just probeMarker)
                probeProcessResult <-
                    probeCommandExitCode
                        compilerSpec'
                        linkArgs
                        linkPostExitDrainSatisfied
                case probeProcessResult of
                    Just ExitSuccess ->
                        validateRunnableLinkedOutput outputPath (Just probeMarker)
                    _                -> pure False

        probeCommandExitCode compilerSpec' args postExitDrainSatisfied =
            Just . (\(exitCode, _, _) -> exitCode)
                <$> wrapCompilerProbeIOError compilerSpec' args
                    ( readCompilerProcessWithExitCodeProbeUntil
                        (const postExitDrainSatisfied)
                        suppressWarnsOutput
                        compilerSpec'
                        args
                    )

        withProbeFile prefix action = do
            tmpDir <- getTemporaryDirectory
            (path, handle) <- openTempFile tmpDir prefix
            finally
                (action path handle)
                ( ignoreIOException (hClose handle)
                    *> ignoreIOException (removeFile path)
                )

        nonEmptyTrimmed outputText =
            case trim outputText of
                "" -> Nothing
                xs -> Just xs

        detectProbeObjectTarget path =
            catchIOError
                (do
                    status <- getSymbolicLinkStatus path
                    if isRegularFile status
                        then describeProbeObject <$> B.readFile path
                        else pure Nothing
                )
                (const $ pure Nothing)

        describeProbeObject bytes
            | B.length bytes < 4 = Nothing
            | B.take 4 bytes /= elfMagic = Just "non-ELF object file"
            | B.length bytes < 20 = Just "truncated ELF object file"
            | otherwise =
                Just $
                    case (elfClass == 2, elfMachine == 62, elfType == 1) of
                        (True, True, True)  -> "x86_64-unknown-elf object"
                        (True, True, False) -> "non-relocatable x86_64-ELF file"
                        (_, _, True)        -> "ELF object file"
                        _                   -> "non-relocatable ELF file"
            where
                elfClass = B.index bytes 4
                elfData = B.index bytes 5
                elfType = decodeElfHalfWord elfData (B.index bytes 16) (B.index bytes 17) :: Int
                elfMachine = decodeElfHalfWord elfData (B.index bytes 18) (B.index bytes 19) :: Int

        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isX86_64ElfTarget :: String -> Bool
isX86_64ElfTarget target =
    let normalizedTarget = map toLower target
     in isX86_64Target normalizedTarget && not (isKnownNonElfTarget normalizedTarget)
    where
        isX86_64Target normalizedTarget =
            "x86_64" `isPrefixOf` normalizedTarget || "amd64" `isPrefixOf` normalizedTarget

        -- Most x86_64 Unix triples are ELF; reject only known Mach-O/PE families.
        isKnownNonElfTarget normalizedTarget =
            any (`isInfixOf` normalizedTarget)
                [ "apple"
                , "cygwin"
                , "darwin"
                , "mingw"
                , "msvc"
                , "windows"
                ]

asmOutputPath :: Opts -> FilePath
asmOutputPath = fromMaybe "a.out" . optOutput

asmAssembleArgs :: FilePath -> FilePath -> [String]
asmAssembleArgs objPath asmPath =
    [ "-x"
    , "assembler"
    , "-c"
    , "-o"
    , objPath
    , asmPath
    ]

asmLinkArgs :: FilePath -> [String] -> [String]
asmLinkArgs outputPath objPaths =
    [ "-no-pie"
    , "-o"
    , outputPath
    ]
        <> objPaths

asmRunnableLinkArgs :: FilePath -> FilePath -> FilePath -> [String]
asmRunnableLinkArgs outputPath objPath markerObjPath =
    asmLinkArgs outputPath [objPath, markerObjPath]

normalizeComparablePath :: FilePath -> IO FilePath
normalizeComparablePath path = do
    exists <- doesFileExist path
    normalise <$> if exists then canonicalizePath path else makeAbsolute path

fileIdentity :: FilePath -> IO (Maybe (FilePath, FilePath))
fileIdentity path = do
    exists <- doesFileExist path
    if exists
        then do
            status <- getFileStatus path
            pure . Just $
                ( show $ deviceID status
                , show $ fileID status
                )
        else pure Nothing

sameFileAs :: FilePath -> FilePath -> IO Bool
sameFileAs lhs rhs = do
    normalizedLhs <- normalizeComparablePath lhs
    normalizedRhs <- normalizeComparablePath rhs
    if normalizedLhs == normalizedRhs
        then pure True
        else do
            lhsIdentity <- fileIdentity lhs
            rhsIdentity <- fileIdentity rhs
            pure $ fromMaybe False $ (==) <$> lhsIdentity <*> rhsIdentity

existingInputAliasesPath :: FilePath -> FilePath -> IO Bool
existingInputAliasesPath outputPath inputPath = do
    inputExists <- doesFileExist inputPath
    if inputExists
        then sameFileAs outputPath inputPath
        else pure False

runAsmOutputAliasesInput :: Opts -> IO Bool
runAsmOutputAliasesInput opts = do
    anyM (existingInputAliasesPath $ asmOutputPath opts) $ optInput opts

plainOutputAliasesInput :: Opts -> IO Bool
plainOutputAliasesInput opts = maybe
    (pure False)
    (\path -> anyM (existingInputAliasesPath path) $ optInput opts)
    (optOutput opts)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x : xs) = do
    matched <- p x
    if matched then pure True else anyM p xs

executableFileMode :: FileMode
executableFileMode = foldr1 unionFileModes
    [ ownerExecuteMode
    , groupExecuteMode
    , otherExecuteMode
    ]

defaultVisualizeResolution :: (Double, Double)
defaultVisualizeResolution = (640, 480)

visualizeOutputPath :: Opts -> FilePath
visualizeOutputPath = fromMaybe "./out.svg" . optOutput

parseImageResolution :: String -> Maybe (Double, Double)
parseImageResolution inputValue = case break (== 'x') inputValue of
    (width, 'x' : height) -> case (readMaybe width, readMaybe height) of
        (Just parsedWidth, Just parsedHeight)
            | isPositiveFiniteResolution parsedWidth && isPositiveFiniteResolution parsedHeight ->
                Just (parsedWidth, parsedHeight)
        _ ->
            Nothing
    _                     -> Nothing

isPositiveFiniteResolution :: Double -> Bool
isPositiveFiniteResolution value =
    value > 0 && not (isInfinite value || isNaN value)

visualizeSizeSpec :: Opts -> IO (SizeSpec V2 Double)
visualizeSizeSpec opts = case optImgResolution opts of
    Just resolutionValue -> case parseImageResolution resolutionValue of
        Just (width, height) ->
            pure $ mkSizeSpec2D (Just width) (Just height)
        Nothing -> do
            unless (optSuppressWarns opts) $
                hPutStr stderr "warning: the specified resolution is invalid, so using default resolution.\n"
            let (width, height) = defaultVisualizeResolution
            pure $ mkSizeSpec2D (Just width) (Just height)
    Nothing -> do
        let (width, height) = defaultVisualizeResolution
        pure $ mkSizeSpec2D (Just width) (Just height)

emitWarningsIfEnabled :: Foldable f => Opts -> f (M.ParseErrorBundle T.Text Void) -> IO ()
emitWarningsIfEnabled opts warnings =
    unless (optSuppressWarns opts) $
        emitWarnings warnings

validateOpts :: Opts -> IO ()
validateOpts opts
    | optVisualizeAst opts && optIsRunAsm opts =
        hPutStr stderr "--visualize-ast cannot be combined with -r\n" *> exitFailure
    | optVisualizeAst opts && length (optInput opts) /= 1 =
        hPutStr stderr "--visualize-ast expects exactly one input file\n" *> exitFailure
    | optVisualizeAst opts = do
        resolvedVisualizeOutputPath <- resolveReplacementOutputPath $ visualizeOutputPath opts
        either
            (\msg -> hPutStr stderr (msg <> "\n") *> exitFailure)
            pure
            (validateVisualizationOutputPath resolvedVisualizeOutputPath)
        outputAliasesInput <- anyM (existingInputAliasesPath $ visualizeOutputPath opts) $ optInput opts
        when outputAliasesInput $
            hPutStr stderr ("--visualize-ast output path must not overwrite an input file: " <> visualizeOutputPath opts <> "\n")
                *> exitFailure
    | isJust (optImgResolution opts) =
        hPutStr stderr "--img-resolution requires --visualize-ast\n" *> exitFailure
    | length (optInput opts) > 1 && optIsRunAsm opts =
        hPutStr stderr "multiple input files are not supported with -r\n" *> exitFailure
    | optIsRunAsm opts = do
        outputAliasesInput <- runAsmOutputAliasesInput opts
        when outputAliasesInput $
            hPutStr stderr ("-r output path must not overwrite an input file: " <> asmOutputPath opts <> "\n")
                *> exitFailure
    | otherwise = do
        outputAliasesInput <- plainOutputAliasesInput opts
        when outputAliasesInput $
            hPutStr stderr ("-o output path must not overwrite an input file: " <> fromMaybe "" (optOutput opts) <> "\n")
                *> exitFailure

type ParsedInput =
    ( ASTs Integer
    , GlobalVars Integer
    , GlobalVars Integer
    , Literals Integer
    , PF.Functions Integer
    , PF.Functions Integer
    )
type ParsedInputWithWarnings = (Warnings, ParsedInput)

emitWarnings :: Foldable f => f (M.ParseErrorBundle T.Text Void) -> IO ()
emitWarnings =
    mapM_ (hPutStr stderr . M.errorBundlePretty)

collectedWarnings :: Foldable f => f ParsedInputWithWarnings -> Warnings
collectedWarnings =
    foldMap fst

collectedWarningsInInputOrder :: [ParsedInputWithWarnings] -> Warnings
collectedWarningsInInputOrder =
    collectedWarnings . reverse

implicitFunctionWarningName :: M.ParseErrorBundle T.Text Void -> Maybe T.Text
implicitFunctionWarningName M.ParseErrorBundle { M.bundleErrors = bundledError :| [] } = do
    msg <- case bundledError of
        M.FancyError _ fancyErrors -> case Set.toList fancyErrors of
            [M.ErrorFail errMsg] ->
                Just $ T.pack errMsg
            _ ->
                Nothing
        _ ->
            Nothing
    T.stripPrefix (T.pack "warning: the function '") msg
        >>= T.stripSuffix (T.pack "' is not declared.")
implicitFunctionWarningName _ = Nothing

originatingInputDeclaresFunction :: ParsedInput -> T.Text -> Bool
originatingInputDeclaresFunction (_, _, _, _, _, funcs) name =
    maybe False (not . PF.fnImplicit) $
        Map.lookup name funcs

implicitFunctionResolvedAfterMerge :: ParsedInput -> T.Text -> Bool
implicitFunctionResolvedAfterMerge (_, _, _, _, funcs, _) name =
    maybe
        False
        (\func -> not (CT.isSCStatic $ PF.fntype func) && not (PF.fnImplicit func))
        $ Map.lookup name funcs

shouldEmitMergedWarning :: ParsedInput -> ParsedInput -> M.ParseErrorBundle T.Text Void -> Bool
shouldEmitMergedWarning originatingInput mergedInput warning =
    maybe
        True
        ( \name ->
            originatingInputDeclaresFunction originatingInput name
                || not (implicitFunctionResolvedAfterMerge mergedInput name)
        )
        (implicitFunctionWarningName warning)

literalLabelPrefix :: T.Text
literalLabelPrefix = T.pack ".L.data."

literalLabel :: Natural -> T.Text
literalLabel n = literalLabelPrefix <> tshow n

shiftLiteralLabelName :: Natural -> T.Text -> T.Text
shiftLiteralLabelName offset name = maybe name (literalLabel . (+ offset)) literalIndex
    where
        literalIndex = T.stripPrefix literalLabelPrefix name >>= readMaybe . T.unpack

shiftLiteralLabelsInGVar :: Natural -> GVar Integer -> GVar Integer
shiftLiteralLabelsInGVar offset gvar = gvar
    { initWith = case initWith gvar of
        GVarInitWithOG ref -> GVarInitWithOG $ shiftLiteralLabelName offset ref
        GVarInitWithData dats -> GVarInitWithData $ map shiftLiteralLabelsInGVarData dats
        GVarInitWithAST ast -> GVarInitWithAST $ shiftLiteralLabelsInATree offset ast
        other              -> other
    }
    where
        shiftLiteralLabelsInGVarData dat = case dat of
            GVarInitReloc sz ref addend -> GVarInitReloc sz (shiftLiteralLabelName offset ref) addend
            other                -> other

shiftLiteralLabelsInATKindFor :: Natural -> ATKindFor Integer -> ATKindFor Integer
shiftLiteralLabelsInATKindFor offset kind = case kind of
    ATForkw      -> ATForkw
    ATForInit at -> ATForInit $ shiftLiteralLabelsInATree offset at
    ATForCond at -> ATForCond $ shiftLiteralLabelsInATree offset at
    ATForIncr at -> ATForIncr $ shiftLiteralLabelsInATree offset at
    ATForStmt at -> ATForStmt $ shiftLiteralLabelsInATree offset at

shiftLiteralLabelsInATKind :: Natural -> ATKind Integer -> ATKind Integer
shiftLiteralLabelsInATKind offset kind = case kind of
    ATConditional cond tr fl ->
        ATConditional
            (shiftLiteralLabelsInATree offset cond)
            (shiftLiteralLabelsInATree offset tr)
            (shiftLiteralLabelsInATree offset fl)
    ATSwitch cond cases ->
        ATSwitch
            (shiftLiteralLabelsInATree offset cond)
            (map (shiftLiteralLabelsInATree offset) cases)
    ATFor kinds ->
        ATFor $ map (shiftLiteralLabelsInATKindFor offset) kinds
    ATBlock ats ->
        ATBlock $ map (shiftLiteralLabelsInATree offset) ats
    ATStmtExpr ats ->
        ATStmtExpr $ map (shiftLiteralLabelsInATree offset) ats
    ATNull at ->
        ATNull $ shiftLiteralLabelsInATree offset at
    ATDefFunc name args ->
        ATDefFunc name $ map (shiftLiteralLabelsInATree offset) <$> args
    ATCallFunc name args ->
        ATCallFunc name $ map (shiftLiteralLabelsInATree offset) <$> args
    ATCallPtr args ->
        ATCallPtr $ map (shiftLiteralLabelsInATree offset) <$> args
    ATGVar ty name ->
        ATGVar ty $ shiftLiteralLabelName offset name
    _ ->
        kind

shiftLiteralLabelsInATree :: Natural -> ATree Integer -> ATree Integer
shiftLiteralLabelsInATree _ ATEmpty = ATEmpty
shiftLiteralLabelsInATree offset (ATNode kind ty lhs rhs) =
    ATNode
        (shiftLiteralLabelsInATKind offset kind)
        ty
        (shiftLiteralLabelsInATree offset lhs)
        (shiftLiteralLabelsInATree offset rhs)

shiftLiteralLabels :: Natural -> ParsedInput -> ParsedInput
shiftLiteralLabels offset (asts, gvars, mergeGVars, lits, funcs, mergeFuncs) =
    ( map (shiftLiteralLabelsInATree offset) asts
    , Map.map (shiftLiteralLabelsInGVar offset) gvars
    , Map.map (shiftLiteralLabelsInGVar offset) mergeGVars
    , map (\lit -> lit { ln = ln lit + offset }) lits
    , funcs
    , mergeFuncs
    )

namespaceInternalSymbol :: Natural -> T.Text -> T.Text
namespaceInternalSymbol inputIndex name =
    namespaceInternalSymbolPrefix inputIndex <> name

namespaceInternalSymbolPrefix :: Natural -> T.Text
namespaceInternalSymbolPrefix inputIndex =
    T.pack ".L.internal."
        <> tshow inputIndex
        <> T.pack "."

denamespaceInternalSymbol :: Natural -> T.Text -> T.Text
denamespaceInternalSymbol inputIndex name =
    fromMaybe name $
        T.stripPrefix (namespaceInternalSymbolPrefix inputIndex) name

renameInputSymbol :: Map.Map T.Text T.Text -> T.Text -> T.Text
renameInputSymbol renames name = Map.findWithDefault name name renames

data InternalSymbolRenames = InternalSymbolRenames
    { functionSymbolRenames :: Map.Map T.Text T.Text
    , objectSymbolRenames   :: Map.Map T.Text T.Text
    }

renameFunctionSymbol :: InternalSymbolRenames -> T.Text -> T.Text
renameFunctionSymbol renames = renameInputSymbol $ functionSymbolRenames renames

renameObjectSymbol :: InternalSymbolRenames -> T.Text -> T.Text
renameObjectSymbol renames = renameInputSymbol $ objectSymbolRenames renames

renameKnownInternalSymbol :: InternalSymbolRenames -> T.Text -> T.Text
renameKnownInternalSymbol renames name =
    renameFunctionSymbol
        renames
        (renameObjectSymbol renames name)

renameInternalSymbolsInGVar :: InternalSymbolRenames -> GVar Integer -> GVar Integer
renameInternalSymbolsInGVar renames gvar = gvar
    { initWith = case initWith gvar of
        GVarInitWithOG ref -> GVarInitWithOG $ renameKnownInternalSymbol renames ref
        GVarInitWithData dats -> GVarInitWithData $ map renameInternalSymbolsInGVarData dats
        GVarInitWithAST ast -> GVarInitWithAST $ renameInternalSymbolsInATree renames ast
        other              -> other
    }
    where
        renameInternalSymbolsInGVarData dat = case dat of
            GVarInitReloc sz ref addend -> GVarInitReloc sz (renameKnownInternalSymbol renames ref) addend
            other                -> other

renameInternalSymbolsInATKind :: InternalSymbolRenames -> ATKind Integer -> ATKind Integer
renameInternalSymbolsInATKind renames kind = case kind of
    ATConditional cond tr fl ->
        ATConditional
            (renameInternalSymbolsInATree renames cond)
            (renameInternalSymbolsInATree renames tr)
            (renameInternalSymbolsInATree renames fl)
    ATSwitch cond cases ->
        ATSwitch
            (renameInternalSymbolsInATree renames cond)
            (map (renameInternalSymbolsInATree renames) cases)
    ATFor kinds ->
        ATFor $ map (renameInternalSymbolsInATKindFor renames) kinds
    ATBlock ats ->
        ATBlock $ map (renameInternalSymbolsInATree renames) ats
    ATStmtExpr ats ->
        ATStmtExpr $ map (renameInternalSymbolsInATree renames) ats
    ATNull at ->
        ATNull $ renameInternalSymbolsInATree renames at
    ATDefFunc name args ->
        ATDefFunc
            (renameFunctionSymbol renames name)
            (map (renameInternalSymbolsInATree renames) <$> args)
    ATCallFunc name args ->
        ATCallFunc
            (renameFunctionSymbol renames name)
            (map (renameInternalSymbolsInATree renames) <$> args)
    ATCallPtr args ->
        ATCallPtr $ map (renameInternalSymbolsInATree renames) <$> args
    ATFuncPtr name ->
        ATFuncPtr $ renameFunctionSymbol renames name
    ATGVar ty name ->
        ATGVar ty $ renameObjectSymbol renames name
    _ ->
        kind

renameInternalSymbolsInATKindFor :: InternalSymbolRenames -> ATKindFor Integer -> ATKindFor Integer
renameInternalSymbolsInATKindFor renames kind = case kind of
    ATForkw      -> ATForkw
    ATForInit at -> ATForInit $ renameInternalSymbolsInATree renames at
    ATForCond at -> ATForCond $ renameInternalSymbolsInATree renames at
    ATForIncr at -> ATForIncr $ renameInternalSymbolsInATree renames at
    ATForStmt at -> ATForStmt $ renameInternalSymbolsInATree renames at

renameInternalSymbolsInATree :: InternalSymbolRenames -> ATree Integer -> ATree Integer
renameInternalSymbolsInATree _ ATEmpty = ATEmpty
renameInternalSymbolsInATree renames (ATNode kind ty lhs rhs) =
    ATNode
        (renameInternalSymbolsInATKind renames kind)
        ty
        (renameInternalSymbolsInATree renames lhs)
        (renameInternalSymbolsInATree renames rhs)

internalSymbolRenames :: Natural -> ParsedInput -> InternalSymbolRenames
internalSymbolRenames inputIndex (_, gvars, _, _, funcs, _) =
    InternalSymbolRenames
        { functionSymbolRenames = Map.fromList
            [ (name, namespaceInternalSymbol inputIndex name)
            | (name, func) <- Map.toList funcs
            , CT.isSCStatic (PF.fntype func)
            ]
        , objectSymbolRenames = Map.fromList
            [ (name, namespaceInternalSymbol inputIndex name)
            | (name, gvar) <- Map.toList gvars
            , CT.isSCStatic (gvtype gvar)
            ]
        }

renameInternalSymbols :: Natural -> ParsedInput -> ParsedInput
renameInternalSymbols inputIndex (asts, gvars, mergeGVars, lits, funcs, mergeFuncs) =
    ( map (renameInternalSymbolsInATree renames) asts
    , Map.mapKeys (renameObjectSymbol renames) $ Map.map (renameInternalSymbolsInGVar renames) gvars
    , Map.mapKeys (renameObjectSymbol renames) $ Map.map (renameInternalSymbolsInGVar renames) mergeGVars
    , lits
    , Map.mapKeys (renameFunctionSymbol renames) funcs
    , Map.mapKeys (renameFunctionSymbol renames) mergeFuncs
    )
    where
        renames = internalSymbolRenames inputIndex (asts, gvars, mergeGVars, lits, funcs, mergeFuncs)

shiftLiteralLabelsInInputs :: [ParsedInput] -> [ParsedInput]
shiftLiteralLabelsInInputs parsedInputs = snd $ mapAccumL step (0, 0) parsedInputs
    where
        shouldNamespaceInternalSymbols = length parsedInputs > 1
        step (inputIndex, offset) parsed@(_, _, _, lits, _, _) =
            let renamed = bool parsed (renameInternalSymbols inputIndex parsed) shouldNamespaceInternalSymbols
                shifted = shiftLiteralLabels offset renamed
             in ((succ inputIndex, offset + fromIntegral (length lits)), shifted)

data ExternalSymbol
    = ExternalFunction (PF.Function Integer) Bool
    | ExternalGlobal (GVar Integer)
    | ExternalImplicitFunction

type ExternalFunctionInfo = (PF.Function Integer, Bool)
type ExternalSymbolOrigin = Int
type TaggedExternalSymbol = (ExternalSymbolOrigin, ExternalSymbol)
type StaticSymbolKey = (ExternalSymbolOrigin, T.Text)
type StaticSymbols = Map.Map StaticSymbolKey ExternalSymbol

duplicateExternalSymbolError :: T.Text -> String
duplicateExternalSymbolError name =
    T.unpack $ T.pack "multiple external definitions in multi-input -o mode: " <> name

conflictingExternalDeclarationError :: T.Text -> GVar Integer -> GVar Integer -> String
conflictingExternalDeclarationError name lhs rhs =
    T.unpack $ mconcat
        [ T.pack "conflicting external declarations in multi-input -o mode: "
        , name
        , T.pack " ("
        , tshow $ CT.toTypeKind $ gvtype lhs
        , T.pack " vs "
        , tshow $ CT.toTypeKind $ gvtype rhs
        , T.pack ")"
        ]

conflictingExternalFunctionDeclarationError :: T.Text -> PF.Function Integer -> PF.Function Integer -> String
conflictingExternalFunctionDeclarationError name lhs rhs =
    T.unpack $ mconcat
        [ T.pack "conflicting external function declarations in multi-input -o mode: "
        , name
        , T.pack " ("
        , tshow $ CT.toTypeKind $ PF.fntype lhs
        , T.pack " vs "
        , tshow $ CT.toTypeKind $ PF.fntype rhs
        , T.pack ")"
        ]

isTentativeExternalGlobal :: GVar Integer -> Bool
isTentativeExternalGlobal gvar =
    not (CT.isSCStatic $ gvtype gvar)
        && case initWith gvar of
            GVarInitWithZero -> True
            _                -> False

isExternOnlyExternalGlobal :: GVar Integer -> Bool
isExternOnlyExternalGlobal gvar =
    not (CT.isSCStatic $ gvtype gvar)
        && case initWith gvar of
            GVarInitWithExternDecl -> True
            _                      -> False

mergeCrossInputTypeKinds :: CT.TypeKind Integer -> CT.TypeKind Integer -> Maybe (CT.TypeKind Integer)
mergeCrossInputTypeKinds lhs rhs =
    CT.mergeCompatibleTypeKinds
        (normalizeCrossInputTypeScopes lhs)
        (normalizeCrossInputTypeScopes rhs)
    where
        -- `ScopeId` is allocated per parser run, so it cannot participate in
        -- cross-input compatibility checks.
        normalizeCrossInputTypeScopes = \case
            CT.CTSigned ty ->
                CT.CTSigned $ normalizeCrossInputTypeScopes ty
            CT.CTShort ty ->
                CT.CTShort $ normalizeCrossInputTypeScopes ty
            CT.CTLong ty ->
                CT.CTLong $ normalizeCrossInputTypeScopes ty
            CT.CTPtr ty ->
                CT.CTPtr $ normalizeCrossInputTypeScopes ty
            CT.CTArray len ty ->
                CT.CTArray len $ normalizeCrossInputTypeScopes ty
            CT.CTFunc ret params ->
                CT.CTFunc
                    (normalizeCrossInputTypeScopes ret)
                    (map (first normalizeCrossInputTypeScopes) params)
            CT.CTIncomplete incompleteTy ->
                CT.CTIncomplete $ case incompleteTy of
                    CT.IncompleteArray elemTy ->
                        CT.IncompleteArray $ normalizeCrossInputTypeScopes elemTy
                    CT.IncompleteStruct tag _ ->
                        CT.IncompleteStruct tag (CT.ScopeId 0)
            CT.CTStruct members ->
                CT.CTStruct $ fmap normalizeCrossInputStructMember members
            CT.CTNamedStruct tag _ members ->
                CT.CTNamedStruct tag (CT.ScopeId 0) $ fmap normalizeCrossInputStructMember members
            CT.CTEnum underlyingTy members ->
                CT.CTEnum (normalizeCrossInputTypeScopes underlyingTy) members
            ty ->
                ty

        normalizeCrossInputStructMember member =
            member { CT.smType = normalizeCrossInputTypeScopes $ CT.smType member }

mergeExternalGlobalTypes
    :: CT.StorageClass Integer
    -> CT.StorageClass Integer
    -> Maybe (CT.StorageClass Integer)
mergeExternalGlobalTypes (CT.SCAuto lhs) (CT.SCAuto rhs) =
    CT.SCAuto <$> mergeCrossInputTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCStatic lhs) (CT.SCStatic rhs) =
    CT.SCStatic <$> mergeCrossInputTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCRegister lhs) (CT.SCRegister rhs) =
    CT.SCRegister <$> mergeCrossInputTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCUndef lhs) (CT.SCUndef rhs) =
    CT.SCUndef <$> mergeCrossInputTypeKinds lhs rhs
mergeExternalGlobalTypes _ _ = Nothing

mergeExternalGlobals :: T.Text -> GVar Integer -> GVar Integer -> Either String (GVar Integer)
mergeExternalGlobals name lhs rhs = case mergeExternalGlobalTypes (gvtype lhs) (gvtype rhs) of
    Nothing ->
        Left $ conflictingExternalDeclarationError name lhs rhs
    Just mergedType
        | isExternOnlyExternalGlobal lhs && isExternOnlyExternalGlobal rhs ->
            Right $ lhs { gvtype = mergedType }
        | isExternOnlyExternalGlobal lhs ->
            Right $ rhs { gvtype = mergedType }
        | isExternOnlyExternalGlobal rhs ->
            Right $ lhs { gvtype = mergedType }
        | isTentativeExternalGlobal lhs && isTentativeExternalGlobal rhs ->
            Right $ lhs { gvtype = mergedType }
        | isTentativeExternalGlobal lhs ->
            Right $ rhs { gvtype = mergedType }
        | isTentativeExternalGlobal rhs ->
            Right $ lhs { gvtype = mergedType }
        | otherwise ->
            Left $ duplicateExternalSymbolError name

mergeExternalFunctions :: T.Text -> ExternalFunctionInfo -> ExternalFunctionInfo -> Either String ExternalFunctionInfo
mergeExternalFunctions name (lhs, lhsDefined) (rhs, rhsDefined)
    | lhsDefined && rhsDefined =
        Left $ duplicateExternalSymbolError name
    | otherwise = case mergeExternalFunctionTypes preferred fallback of
        Nothing ->
            Left $ conflictingExternalFunctionDeclarationError name lhs rhs
        Just merged ->
            Right (merged, lhsDefined || rhsDefined)
    where
        (preferred, fallback)
            | rhsDefined = (rhs, lhs)
            | otherwise = (lhs, rhs)

mergeExternalFunctionTypes
    :: PF.Function Integer
    -> PF.Function Integer
    -> Maybe (PF.Function Integer)
mergeExternalFunctionTypes preferred fallback = do
    mergedType <- mergeExternalGlobalTypes (PF.fntype preferred) (PF.fntype fallback)
    pure $
        preferred
            { PF.fntype = mergedType
            , PF.fnDefined = PF.fnDefined preferred || PF.fnDefined fallback
            , PF.fnImplicit = PF.fnImplicit preferred && PF.fnImplicit fallback
            }

implicitExternalFunction :: PF.Function Integer
implicitExternalFunction =
    PF.Function
        { PF.fntype = CT.SCAuto $ CT.CTFunc CT.CTInt []
        , PF.fnDefined = False
        , PF.fnImplicit = True
        , PF.fnNestDepth = 0
        }

definedFunctions :: ParsedInput -> Set.Set T.Text
definedFunctions (asts, _, _, _, _, _) =
    Set.fromList
        [ name
        | ATNode (ATDefFunc name _) _ _ _ <- asts
        ]

implicitFunctionCalls :: ParsedInput -> Set.Set T.Text
implicitFunctionCalls (asts, _, _, _, _, mergeFuncs) =
    foldMap implicitFunctionCallsInATree asts
        `Set.difference` Set.fromList (Map.keys mergeFuncs)

implicitFunctionCallsInATKindFor :: ATKindFor Integer -> Set.Set T.Text
implicitFunctionCallsInATKindFor kind = case kind of
    ATForkw      -> Set.empty
    ATForInit at -> implicitFunctionCallsInATree at
    ATForCond at -> implicitFunctionCallsInATree at
    ATForIncr at -> implicitFunctionCallsInATree at
    ATForStmt at -> implicitFunctionCallsInATree at

implicitFunctionCallsInATKind :: ATKind Integer -> Set.Set T.Text
implicitFunctionCallsInATKind kind = case kind of
    ATConditional cond tr fl ->
        implicitFunctionCallsInATree cond
            <> implicitFunctionCallsInATree tr
            <> implicitFunctionCallsInATree fl
    ATSwitch cond cases ->
        implicitFunctionCallsInATree cond
            <> foldMap implicitFunctionCallsInATree cases
    ATFor kinds ->
        foldMap implicitFunctionCallsInATKindFor kinds
    ATBlock ats ->
        foldMap implicitFunctionCallsInATree ats
    ATStmtExpr ats ->
        foldMap implicitFunctionCallsInATree ats
    ATNull at ->
        implicitFunctionCallsInATree at
    ATDefFunc _ args ->
        foldMap implicitFunctionCallsInATree $ fromMaybe [] args
    ATCallFunc name args ->
        Set.insert name $ foldMap implicitFunctionCallsInATree $ fromMaybe [] args
    ATCallPtr args ->
        foldMap implicitFunctionCallsInATree $ fromMaybe [] args
    _ ->
        Set.empty

implicitFunctionCallsInATree :: ATree Integer -> Set.Set T.Text
implicitFunctionCallsInATree ATEmpty = Set.empty
implicitFunctionCallsInATree (ATNode kind _ lhs rhs) =
    implicitFunctionCallsInATKind kind
        <> implicitFunctionCallsInATree lhs
        <> implicitFunctionCallsInATree rhs

mergeOutputInputs :: [ParsedInput] -> Either String ParsedInput
mergeOutputInputs =
    mergePreparedInputs prepareAsmInput

mergeVisualizableInputs :: [ParsedInput] -> Either String ParsedInput
mergeVisualizableInputs =
    mergePreparedInputs prepareVisualizableInput

mergePreparedInputs
    :: (PF.Functions Integer -> ASTs Integer -> GlobalVars Integer -> Either String (ASTs Integer, GlobalVars Integer))
    -> [ParsedInput]
    -> Either String ParsedInput
mergePreparedInputs prepareMergedInput =
    mergeParsedInputs finalize
    where
        finalize (asts, gvars, lits, funcs, _, _) = do
            (preparedAsts, preparedGVars) <- prepareMergedInput (fmap fst funcs) asts gvars
            let visibleFuncs = fmap fst funcs
            pure (preparedAsts, preparedGVars, preparedGVars, lits, visibleFuncs, visibleFuncs)

mergeParsedInputs
    :: (( ASTs Integer
        , GlobalVars Integer
        , Literals Integer
        , Map.Map T.Text (PF.Function Integer, Bool)
        , Map.Map T.Text TaggedExternalSymbol
        , StaticSymbols
        )
        -> Either String ParsedInput
       )
    -> [ParsedInput]
    -> Either String ParsedInput
mergeParsedInputs finalize parsedInputs =
    foldM mergeInput ([], Map.empty, [], Map.empty, Map.empty, Map.empty) (zip [0 :: Int ..] parsedInputs) >>= finalize
    where
        mergeInput (astsAcc, gvarsAcc, litsAcc, funcsAcc, symbolsAcc, staticSymbolsAcc) (inputIndex, (asts, visibleGVars, mergeGVars, lits, visibleFuncs, mergeFuncs)) = do
            let parsedInput = (asts, visibleGVars, mergeGVars, lits, visibleFuncs, mergeFuncs)
                actualDefinitions = definedFunctions parsedInput
            symbolsAcc' <- foldM (registerImplicitFunction inputIndex staticSymbolsAcc) symbolsAcc $ Set.toList $ implicitFunctionCalls parsedInput
            (symbolsAcc'', staticSymbolsAcc', funcsAcc') <-
                foldM
                    (registerFunction inputIndex actualDefinitions visibleFuncs)
                    (symbolsAcc', staticSymbolsAcc, funcsAcc)
                    $ Map.toList mergeFuncs
            (symbolsAcc''', staticSymbolsAcc'', gvarsAcc') <-
                foldM
                    (registerGlobal inputIndex visibleGVars)
                    (symbolsAcc'', staticSymbolsAcc', gvarsAcc)
                    $ Map.toList mergeGVars
            pure
                ( astsAcc <> asts
                , gvarsAcc'
                , litsAcc <> lits
                , funcsAcc'
                , symbolsAcc'''
                , staticSymbolsAcc''
                )

        registerImplicitFunction origin staticSymbols symbols name = do
            rejectStaticSymbolConflict origin name ExternalImplicitFunction staticSymbols
            case Map.lookup name symbols of
                Nothing ->
                    pure $ insertSymbol origin name ExternalImplicitFunction symbols
                Just (existingOrigin, existingSymbol)
                    | existingOrigin == origin ->
                        (\merged -> insertSymbol origin name merged symbols)
                            <$> mergeSameOriginExternalSymbol name existingSymbol ExternalImplicitFunction
                    | otherwise -> case existingSymbol of
                        ExternalGlobal _ ->
                            Left $ duplicateExternalSymbolError name
                        ExternalFunction existing existingHasBody ->
                            mergeExternalFunctions name (existing, existingHasBody) (implicitExternalFunction, False)
                                $> symbols
                        _ ->
                            pure symbols

        registerFunction origin actualDefinitions visibleFuncs (symbols, staticSymbols, funcsAcc) (name, func)
            | CT.isSCStatic (PF.fntype func) = do
                rejectExternalSymbolConflict origin semanticName newSymbol symbols
                staticSymbols' <- registerStaticSymbol origin semanticName newSymbol staticSymbols
                pure
                    ( symbols
                    , staticSymbols'
                    , insertVisibleFunction name (func, hasBody) visibleFuncs funcsAcc
                    )
            | otherwise = do
                rejectStaticSymbolConflict origin semanticName newSymbol staticSymbols
                case Map.lookup semanticName symbols of
                    Nothing ->
                        pure
                            ( insertSymbol origin semanticName newSymbol symbols
                            , staticSymbols
                            , insertVisibleFunction name (func, hasBody) visibleFuncs funcsAcc
                            )
                    Just (existingOrigin, existingSymbol)
                        | existingOrigin == origin ->
                            case mergeSameOriginExternalSymbol name existingSymbol (ExternalFunction func hasBody) of
                                Left mergeErr ->
                                    Left mergeErr
                                Right (ExternalFunction mergedFunc mergedHasBody) ->
                                    pure
                                        ( insertSymbol origin semanticName (ExternalFunction mergedFunc mergedHasBody) symbols
                                        , staticSymbols
                                        , insertVisibleFunction name (mergedFunc, mergedHasBody) visibleFuncs funcsAcc
                                        )
                                Right _ ->
                                    Left "internal compiler error: unexpected same-input symbol merge result"
                        | otherwise -> case existingSymbol of
                            ExternalImplicitFunction ->
                                mergeExternalFunctions name (implicitExternalFunction, False) (func, hasBody) >>
                                    pure
                                        ( insertSymbol origin semanticName newSymbol symbols
                                        , staticSymbols
                                        , insertVisibleFunction name (func, hasBody) visibleFuncs funcsAcc
                                        )
                            ExternalFunction existing existingHasBody -> do
                                merged <- mergeExternalFunctions name (existing, existingHasBody) (func, hasBody)
                                pure
                                    ( insertSymbol origin semanticName (uncurry ExternalFunction merged) symbols
                                    , staticSymbols
                                    , insertVisibleFunction name merged visibleFuncs funcsAcc
                                    )
                            ExternalGlobal _ ->
                                Left $ duplicateExternalSymbolError name
            where
                hasBody = Set.member name actualDefinitions
                semanticName = emittedSymbolName origin (CT.isSCStatic $ PF.fntype func) name
                newSymbol = ExternalFunction func hasBody

        registerGlobal origin visibleGVars (symbols, staticSymbols, gvarsAcc) (name, gvar)
            | CT.isSCStatic (gvtype gvar) = do
                rejectExternalSymbolConflict origin semanticName newSymbol symbols
                staticSymbols' <- registerStaticSymbol origin semanticName newSymbol staticSymbols
                pure
                    ( symbols
                    , staticSymbols'
                    , insertVisibleGlobal name visibleGVars gvarsAcc
                    )
            | otherwise = do
                rejectStaticSymbolConflict origin semanticName newSymbol staticSymbols
                case Map.lookup semanticName symbols of
                    Nothing ->
                        pure
                            ( insertSymbol origin semanticName newSymbol symbols
                            , staticSymbols
                            , insertVisibleGlobal name visibleGVars gvarsAcc
                            )
                    Just (existingOrigin, existingSymbol)
                        | existingOrigin == origin ->
                            case mergeSameOriginExternalSymbol name existingSymbol (ExternalGlobal gvar) of
                                Left mergeErr ->
                                    Left mergeErr
                                Right (ExternalGlobal mergedGVar) ->
                                    pure
                                        ( insertSymbol origin semanticName (ExternalGlobal mergedGVar) symbols
                                        , staticSymbols
                                        , insertVisibleGlobal name visibleGVars gvarsAcc
                                        )
                                Right _ ->
                                    Left "internal compiler error: unexpected same-input symbol merge result"
                        | otherwise -> case existingSymbol of
                            ExternalFunction _ _ ->
                                Left $ duplicateExternalSymbolError name
                            ExternalImplicitFunction ->
                                Left $ duplicateExternalSymbolError name
                            ExternalGlobal existing -> do
                                merged <- mergeExternalGlobals name existing gvar
                                pure
                                    ( insertSymbol origin semanticName (ExternalGlobal merged) symbols
                                    , staticSymbols
                                    , insertVisibleGlobal name visibleGVars gvarsAcc
                                    )
            where
                semanticName = emittedSymbolName origin (CT.isSCStatic $ gvtype gvar) name
                newSymbol = ExternalGlobal gvar

        insertGlobal name = Map.insertWith (preserveMergedGlobalType name) name
        insertFunction = Map.insertWith preserveMergedFunctionType
        insertVisibleGlobal name visibleGVars gvarsAcc =
            maybe gvarsAcc (\gvar -> insertGlobal name gvar gvarsAcc) $
                Map.lookup name visibleGVars
        insertVisibleFunction name func visibleFuncs funcsAcc =
            maybe funcsAcc (\visibleFunc -> insertFunction name (visibleFunc, snd func) funcsAcc) $
                Map.lookup name visibleFuncs
        insertSymbol origin name symbol = Map.insert name (origin, symbol)
        insertStaticSymbol origin name = Map.insert (origin, name)

        emittedSymbolName origin isInternal name
            | isInternal =
                denamespaceInternalSymbol (fromIntegral origin) name
            | otherwise =
                name

        rejectExternalSymbolConflict origin name newSymbol symbols = case Map.lookup name symbols of
            Just (existingOrigin, existingSymbol)
                | existingOrigin == origin ->
                    mergeSameOriginExternalSymbol name existingSymbol newSymbol $> ()
            _ ->
                pure ()

        rejectStaticSymbolConflict origin name newSymbol staticSymbols = case Map.lookup (origin, name) staticSymbols of
            Just existingSymbol ->
                mergeSameOriginExternalSymbol name existingSymbol newSymbol $> ()
            Nothing ->
                pure ()

        registerStaticSymbol origin name newSymbol staticSymbols = case Map.lookup (origin, name) staticSymbols of
            Just existingSymbol ->
                (\merged -> insertStaticSymbol origin name merged staticSymbols)
                    <$> mergeSameOriginExternalSymbol name existingSymbol newSymbol
            Nothing ->
                pure $ insertStaticSymbol origin name newSymbol staticSymbols

        mergeSameOriginExternalSymbol name existingSymbol newSymbol = case (existingSymbol, newSymbol) of
            (ExternalImplicitFunction, ExternalImplicitFunction) ->
                pure ExternalImplicitFunction
            (ExternalImplicitFunction, ExternalFunction func hasBody) ->
                pure $ ExternalFunction func hasBody
            (ExternalFunction func hasBody, ExternalImplicitFunction) ->
                pure $ ExternalFunction func hasBody
            (ExternalFunction existing existingHasBody, ExternalFunction func hasBody) ->
                uncurry ExternalFunction
                    <$> mergeExternalFunctions name (existing, existingHasBody) (func, hasBody)
            (ExternalGlobal existing, ExternalGlobal gvar) ->
                ExternalGlobal <$> mergeExternalGlobals name existing gvar
            _ ->
                Left $ duplicateExternalSymbolError name

        preserveMergedFunctionType new old =
            ( fromMaybe (fst new) $ mergeExternalFunctionTypes (fst new) (fst old)
            , snd new || snd old
            )
        preserveMergedGlobalType name new old =
            fromRight old $
                mergeExternalGlobals name old new

runAsm :: Maybe Handle -> Opts -> SI.Asm SI.AsmCodeCtx Integer a -> IO a
runAsm outputHandle opts asm
    | optIsRunAsm opts = do
        resolvedOutputPath <- resolveReplacementOutputPath $ asmOutputPath opts
        shouldValidateOutput <- shouldValidateRunnableLinkedOutput resolvedOutputPath
        snd <$>
            withReplacementOutputPathAndResolvedPath PreserveReplacementOutputModeKeepingExecutableBits (asmOutputPath opts) (\tmpOutputPath -> do
                compilerSpec <- asmCompiler $ optSuppressWarns opts
                tmpDir <- getTemporaryDirectory
                (asmPath, tmpHandle) <- openTempFile tmpDir "htcc-.s"
                finally
                    ( do
                        (objPath, objHandle) <- openTempFile tmpDir "htcc-.o"
                        let cleanupObj =
                                ignoreIOException (hClose objHandle)
                                    *> ignoreIOException (removeFile objPath)
                        finally
                            ( do
                                (markerAsmPath, markerAsmHandle) <- openTempFile tmpDir "htcc-marker-.s"
                                let cleanupMarkerAsm =
                                        ignoreIOException (hClose markerAsmHandle)
                                            *> ignoreIOException (removeFile markerAsmPath)
                                finally
                                    ( do
                                        (markerObjPath, markerObjHandle) <- openTempFile tmpDir "htcc-marker-.o"
                                        let cleanupMarkerObj =
                                                ignoreIOException (hClose markerObjHandle)
                                                    *> ignoreIOException (removeFile markerObjPath)
                                        finally
                                            ( do
                                                let runnableOutputMarker =
                                                        makeRunnableLinkedOutputMarker
                                                            asmPath
                                                            objPath
                                                            markerObjPath
                                                hPutStr
                                                    markerAsmHandle
                                                    (x86_64ElfRunnableLinkedOutputMarkerAsm runnableOutputMarker)
                                                hClose markerAsmHandle
                                                setFileMode markerObjPath temporaryWritableMode
                                                hClose markerObjHandle
                                                let markerAssembleArgs = asmAssembleArgs markerObjPath markerAsmPath
                                                when (optIsVerbose opts) $
                                                    hPutStr stderr $
                                                        showCompilerCommandForUser compilerSpec markerAssembleArgs
                                                            <> "\n"
                                                markerAssemblePostExitDrainSatisfied <-
                                                    stabilizeCompilerObjectOutputAfterExit
                                                        markerObjPath
                                                callCompilerProcessUntil
                                                    markerAssemblePostExitDrainSatisfied
                                                    (optSuppressWarns opts)
                                                    compilerSpec
                                                    markerAssembleArgs
                                                setFileMode objPath temporaryWritableMode
                                                hClose objHandle
                                                let assembleArgs = asmAssembleArgs objPath asmPath
                                                    linkArgs =
                                                        asmRunnableLinkArgs tmpOutputPath objPath markerObjPath
                                                result' <- SI.runAsmWithHandle tmpHandle asm
                                                hClose tmpHandle
                                                when (optIsVerbose opts) $
                                                    hPutStr stderr $
                                                        showCompilerCommandForUser compilerSpec assembleArgs <> "\n"
                                                assemblePostExitDrainSatisfied <-
                                                    stabilizeCompilerObjectOutputAfterExit
                                                        objPath
                                                callCompilerProcessUntil
                                                    assemblePostExitDrainSatisfied
                                                    (optSuppressWarns opts)
                                                    compilerSpec
                                                    assembleArgs
                                                linkPostExitDrainSatisfied <-
                                                    if shouldValidateOutput
                                                        then
                                                            stabilizePostExitPredicate $
                                                                validateRunnableLinkedOutput
                                                                    tmpOutputPath
                                                                    (Just runnableOutputMarker)
                                                        else pure waitForCompilerProcessGroupQuiescenceAfterExit
                                                when (optIsVerbose opts) $
                                                    hPutStr stderr $
                                                        showCompilerCommandForUser compilerSpec linkArgs <> "\n"
                                                callCompilerProcessUntil
                                                    linkPostExitDrainSatisfied
                                                    (optSuppressWarns opts)
                                                    compilerSpec
                                                    linkArgs
                                                when shouldValidateOutput $ do
                                                    linkedOutputOk <-
                                                        validateRunnableLinkedOutput
                                                            tmpOutputPath
                                                            (Just runnableOutputMarker)
                                                    unless linkedOutputOk $
                                                        ioError . userError $
                                                            "HTCC_ASSEMBLER produced a non-runnable final output for -r: "
                                                                <> asmOutputPath opts
                                                pure result'
                                            )
                                            cleanupMarkerObj
                                    )
                                    cleanupMarkerAsm
                            )
                            cleanupObj
                    )
                    ( ignoreIOException (hClose tmpHandle)
                        *> ignoreIOException (removeFile asmPath)
                    )
            )
    | otherwise = maybe
        (SI.runAsm asm)
        (`SI.runAsmWithHandle` asm)
        outputHandle

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    validateOpts opts
    let allowSameInputExternalCollisions =
            not (optIsRunAsm opts) && length (optInput opts) > 1
        emitWarnings' =
            emitWarningsIfEnabled opts
        parseInputRawEitherSingleInput fname txt =
            case runParser parser fname txt
                :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer) of
                Left x  -> Left x
                Right (warns, asts, gvars, lits, funcs) ->
                    Right (warns, (asts, gvars, gvars, lits, funcs, funcs))
        parseInputRawEitherAllowingExternalCollisions fname txt =
            case PT.runParserAllowSameInputExternalCollisionsDetailed parser fname txt of
                Left x  -> Left x
                Right (warns, asts, gvars, mergeGVars, lits, funcs, mergeFuncs) ->
                    Right (warns, (asts, gvars, mergeGVars, lits, funcs, mergeFuncs))
        parseInputRawEither fname txt =
            if allowSameInputExternalCollisions
                then parseInputRawEitherAllowingExternalCollisions fname txt
                else parseInputRawEitherSingleInput fname txt
        parseInputRaw fname txt =
            either
                (\x -> hPutStr stderr (M.errorBundlePretty x) *> exitFailure)
                pure
                (parseInputRawEither fname txt)
        readParsedInputRaw fname =
            readInput fname >>= uncurry parseInputRaw
        readParsedInput fname = do
            (warns, parsedInput) <- readParsedInputRaw fname
            case mergeParsedInputsEither [parsedInput] of
                Left msg -> do
                    emitWarnings' warns
                    hPutStr stderr (msg <> "\n")
                    exitFailure
                Right mergedInput -> do
                    emitWarnings' warns
                    pure mergedInput
        readVisualizableInput fname = do
            (warns, parsedInput) <- readParsedInputRaw fname
            case mergeVisualizableInputsEither [parsedInput] of
                Left msg -> do
                    emitWarnings' warns
                    hPutStr stderr (msg <> "\n")
                    exitFailure
                Right mergedInput -> do
                    emitWarnings' warns
                    pure mergedInput
        readMergedInputRaw parsedInputs [] =
            pure $ reverse parsedInputs
        readMergedInputRaw parsedInputs (fname:fnames) =
            catchIOError
                ( do
                    (inputName, txt) <- readInput fname
                    case parseInputRawEither inputName txt of
                        Left parseErr -> do
                            emitWarnings' $ collectedWarningsInInputOrder parsedInputs
                            hPutStr stderr (M.errorBundlePretty parseErr)
                            exitFailure
                        Right (warns, parsedInput) ->
                            readMergedInputRaw
                                ((warns, parsedInput) : parsedInputs)
                                fnames
                )
                (\ioErr -> emitWarnings' (collectedWarningsInInputOrder parsedInputs) *> ioError ioErr)
        mergeParsedInputsEither parsedInputs =
            mergeOutputInputs $ shiftLiteralLabelsInInputs parsedInputs
        mergeVisualizableInputsEither parsedInputs =
            mergeVisualizableInputs $ shiftLiteralLabelsInInputs parsedInputs
        readMergedInput = do
            parsedInputsWithWarnings <- readMergedInputRaw [] (optInput opts)
            let parsedInputs = map snd parsedInputsWithWarnings
            case mergeParsedInputsEither parsedInputs of
                Left msg -> do
                    emitWarnings' $ collectedWarnings parsedInputsWithWarnings
                    hPutStr stderr (msg <> "\n")
                    exitFailure
                Right parsedInput -> do
                    emitWarnings'
                        (SQ.fromList
                            [ warning
                            | (warnings, originatingInput) <- parsedInputsWithWarnings
                            , warning <- foldMap pure warnings
                            , shouldEmitMergedWarning originatingInput parsedInput warning
                            ]
                        )
                    pure parsedInput
        runParsed outputHandle (asts, gvars, _, lits, _, _) =
            runAsm outputHandle opts $ casmNormalized' asts gvars lits
        readInput fname = do
            txt <- withFile fname ReadMode $ \h -> do
                txt' <- T.hGetContents h
                _ <- evaluate $ T.foldl' (\n _ -> succ n) (0 :: Int) txt'
                pure txt'
            pure (fname, txt)
        runVisualize fname = do
            (asts, _, _, _, _, _) <- readVisualizableInput fname
            sizeSpec <- visualizeSizeSpec opts
            writeVisualization asts sizeSpec (visualizeOutputPath opts)
    if optVisualizeAst opts
        then case optInput opts of
            [fname] -> runVisualize fname
            _       -> hPutStr stderr "internal compiler error: invalid visualize inputs\n" *> exitFailure
        else if optIsRunAsm opts
        then forM_ (optInput opts) $
            readParsedInput >=> runParsed Nothing
        else maybe
            (case optInput opts of
                [_] ->
                    forM_ (optInput opts) $
                        readParsedInput >=> runParsed Nothing
                _ ->
                    readMergedInput >>= runParsed Nothing
            )
            (\path -> case optInput opts of
                [fname] -> do
                    parsedInput <- readParsedInput fname
                    withReplacementOutputPath PreserveReplacementOutputMode path $ \tmpOutputPath ->
                        withFile tmpOutputPath WriteMode $ \h ->
                            runParsed (Just h) parsedInput
                _ -> do
                    mergedInput <- readMergedInput
                    withReplacementOutputPath PreserveReplacementOutputMode path $ \tmpOutputPath ->
                        withFile tmpOutputPath WriteMode $ \h ->
                            runParsed (Just h) mergedInput
            )
            (optOutput opts)
