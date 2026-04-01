{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Main where

import           Control.Exception                           (evaluate, finally)
import           Control.Monad                               (foldM, forM_,
                                                              when)
import           Data.Bool                                   (bool)
import qualified Data.ByteString                             as B
import           Data.Char                                   (isAlpha,
                                                              isAlphaNum,
                                                              isSpace, toLower)
import           Data.Foldable                               (toList)
import           Data.List                                   (isInfixOf,
                                                              isPrefixOf,
                                                              mapAccumL,
                                                              stripPrefix)
import           Data.List.NonEmpty                          (NonEmpty (..))
import           Data.Maybe                                  (fromMaybe,
                                                              mapMaybe)
import qualified Data.Text.IO                                as T
import           Data.Version                                (showVersion)
import           Data.Word                                   (Word8)
import           Language.Haskell.TH.Syntax                  (addDependentFile,
                                                              lift, runIO)
import qualified Options.Applicative                         as OA
import qualified Paths_htcc                                  as P

import qualified Data.Map.Strict                             as Map
import qualified Data.Set                                    as Set
import qualified Data.Text                                   as T
import           Data.Void
import           Htcc.Asm                                    (casmNormalized',
                                                              prepareAsmInput)
import qualified Htcc.Asm.Intrinsic.Structure.Internal       as SI
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Output                                 (ReplacementOutputMode (..),
                                                              creationMaskedOutputMode,
                                                              stagedOutputMode,
                                                              temporaryWritableMode,
                                                              withReplacementOutputPath)
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
import           Numeric.Natural                             (Natural)
import           System.Directory                            (canonicalizePath,
                                                              doesFileExist,
                                                              executable,
                                                              getPermissions,
                                                              getTemporaryDirectory,
                                                              makeAbsolute,
                                                              removeFile)
import           System.Environment                          (getEnvironment,
                                                              lookupEnv)
import           System.Exit                                 (ExitCode (..),
                                                              exitFailure)
import           System.FilePath                             (normalise,
                                                              searchPathSeparator,
                                                              takeFileName,
                                                              (</>))
import           System.IO                                   (Handle,
                                                              IOMode (ReadMode, WriteMode),
                                                              hClose, hPutStr,
                                                              openTempFile,
                                                              stderr, withFile)
import           System.IO.Error                             (catchIOError,
                                                              isDoesNotExistError)
import           System.Posix.Files                          (deviceID, fileID,
                                                              fileMode,
                                                              getFileStatus,
                                                              getSymbolicLinkStatus,
                                                              groupExecuteMode,
                                                              intersectFileModes,
                                                              isRegularFile,
                                                              isSymbolicLink,
                                                              otherExecuteMode,
                                                              ownerExecuteMode,
                                                              ownerReadMode,
                                                              setFileMode,
                                                              unionFileModes)
import           System.Posix.Types                          (FileMode)
import           System.Process                              (CreateProcess (..),
                                                              createProcess,
                                                              proc,
                                                              readCreateProcessWithExitCode,
                                                              readProcessWithExitCode,
                                                              showCommandForUser,
                                                              waitForProcess)
import qualified Text.Megaparsec                             as M
import qualified Text.Parsec                                 as Parsec
import           Text.Read                                   (readMaybe)

data Opts = Opts
    { optIsRunAsm  :: !Bool
    , optIsVerbose :: !Bool
    , optOutput    :: Maybe FilePath
    , optInput     :: [FilePath]
    } deriving (Read, Show)

output :: OA.Parser (Maybe String)
output = OA.optional $ OA.strOption $ mconcat [
    OA.metavar "<file>"
  , OA.long "output"
  , OA.short 'o'
  , OA.help "Place the output into <file>"
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
  , OA.progDesc $ concat [
        "The C Language Compiler htcc "
      , showVersion P.version
    ]
  ]

ignoreIOException :: IO () -> IO ()
ignoreIOException = flip catchIOError $ const $ pure ()

nonEmptyEnv :: Maybe String -> Maybe String
nonEmptyEnv (Just s) | all isSpace s = Nothing
nonEmptyEnv x         = x

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

data CompilerCommand = CompilerCommand
    { compilerEnvOverrides :: [(String, String)]
    , compilerExecutable   :: FilePath
    , compilerArguments    :: [String]
    }

resolveCompilerCommand :: String -> IO CompilerCommand
resolveCompilerCommand compiler = do
    parts <- case shellWords compiler of
        Left parseErr -> ioError . userError $
            "failed to parse compiler command " <> show compiler <> ": " <> parseErr
        Right [] -> ioError . userError $
            "empty compiler command: " <> show compiler
        Right xs -> pure xs
    let (envAssignments, compilerParts) = span isEnvironmentAssignmentWord parts
        envOverrides = Map.fromList $ map splitEnvironmentAssignment envAssignments
    when (null compilerParts) . ioError . userError $
        "empty compiler command: " <> show compiler
    resolvedPrefix <- findExecutablePrefix envOverrides compilerParts
    pure $
        case resolvedPrefix of
            Just (compilerLen, resolvedCompiler) ->
                CompilerCommand
                    { compilerEnvOverrides = map splitEnvironmentAssignment envAssignments
                    , compilerExecutable = resolvedCompiler
                    , compilerArguments = drop compilerLen compilerParts
                    }
            Nothing ->
                CompilerCommand
                    { compilerEnvOverrides = map splitEnvironmentAssignment envAssignments
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

        resolveExecutableCommand envOverrides' cmd = do
            case hasExplicitPath cmd of
                True -> localExecutablePath cmd
                False ->
                    firstResolved $
                        [ findExecutableInSearchPath envOverrides' cmd
                        ]
                            <> [localExecutablePath cmd | not (hasOverriddenSearchPath envOverrides')]

        findExecutableInSearchPath envOverrides' cmd = do
            pathValue <- maybe
                (fromMaybe "" <$> lookupEnv "PATH")
                pure
                (Map.lookup "PATH" envOverrides')
            firstResolved $
                map (localExecutablePath . searchPathCommand cmd) $
                    searchPathEntries pathValue

        searchPathCommand cmd ""  = cmd
        searchPathCommand cmd dir = dir </> cmd

        hasOverriddenSearchPath = Map.member "PATH"

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

        firstResolved [] = pure Nothing
        firstResolved (resolvePath : resolvePaths) = do
            resolved <- resolvePath
            maybe (firstResolved resolvePaths) (pure . Just) resolved

        normalizeLocalExecutablePath cmd
            | hasExplicitPath cmd = cmd
            | otherwise = "./" <> cmd

compilerInvocationArgs :: CompilerCommand -> [String] -> [String]
compilerInvocationArgs compiler extraArgs =
    compilerArguments compiler <> extraArgs

compilerProcessEnv :: [(String, String)] -> IO (Maybe [(String, String)])
compilerProcessEnv [] = pure Nothing
compilerProcessEnv overrides =
    Just . Map.toList . Map.union (Map.fromList overrides) . Map.fromList
        <$> getEnvironment

showCompilerCommandForUser :: CompilerCommand -> [String] -> String
showCompilerCommandForUser compiler extraArgs =
    case compilerEnvOverrides compiler of
        [] ->
            showCommandForUser
                (compilerExecutable compiler)
                (compilerInvocationArgs compiler extraArgs)
        overrides ->
            showCommandForUser
                "env"
                ( map (\(name, value) -> name <> "=" <> value) overrides
                    <> [compilerExecutable compiler]
                    <> compilerInvocationArgs compiler extraArgs
                )

readCompilerProcessWithExitCode :: CompilerCommand -> [String] -> IO (ExitCode, String, String)
readCompilerProcessWithExitCode compiler extraArgs = do
    processEnv <- compilerProcessEnv $ compilerEnvOverrides compiler
    readCreateProcessWithExitCode
        (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
            { env = processEnv
            }
        ""

callCompilerProcess :: CompilerCommand -> [String] -> IO ()
callCompilerProcess compiler extraArgs = do
    processEnv <- compilerProcessEnv $ compilerEnvOverrides compiler
    (_, _, _, processHandle) <- createProcess
        (proc (compilerExecutable compiler) (compilerInvocationArgs compiler extraArgs))
            { env = processEnv
            }
    waitForProcess processHandle >>= \case
        ExitSuccess -> pure ()
        exitCode ->
            ioError . userError $
                showCompilerCommandForUser compiler extraArgs
                    <> " failed with "
                    <> show exitCode

withReadableFile :: FilePath -> FileMode -> IO a -> IO a
withReadableFile path originalMode action
    | intersectFileModes originalMode ownerReadMode /= 0 = action
    | otherwise = do
        setFileMode path readableMode
        action `finally` setFileMode path originalMode
    where
        readableMode = originalMode `unionFileModes` ownerReadMode

validateRunnableLinkedOutput :: FilePath -> Maybe String -> IO Bool
validateRunnableLinkedOutput path maybeProbeMarker =
    catchIOError
        (do
            status <- getSymbolicLinkStatus path
            if isRegularFile status
                then
                    withReadableFile path (fileMode status) $ do
                        bytes <- B.readFile path
                        pure $
                            intersectFileModes (fileMode status) executableFileMode /= 0
                                && looksRunnableLinkedOutput bytes
                                && maybe True (`probeMarkerPresent` bytes) maybeProbeMarker
                else pure False
        )
        (\ioErr -> if isDoesNotExistError ioErr then pure False else ioError ioErr)

shouldValidateRunnableLinkedOutput :: FilePath -> IO Bool
shouldValidateRunnableLinkedOutput path =
    catchIOError
        (do
            status <- getSymbolicLinkStatus path
            pure $ isRegularFile status || isSymbolicLink status
        )
        (\ioErr -> if isDoesNotExistError ioErr then pure True else ioError ioErr)

probeMarkerPresent :: String -> B.ByteString -> Bool
probeMarkerPresent probeMarker =
    B.isInfixOf $ B.pack $ map (fromIntegral . fromEnum) probeMarker

looksRunnableLinkedOutput :: B.ByteString -> Bool
looksRunnableLinkedOutput bytes
    | B.length bytes < 4 = False
    | B.take 4 bytes /= elfMagic = False
    | B.length bytes < 20 = False
    | otherwise =
        elfClass == 2
            && elfMachine == 62
            && elfType == 2
    where
        elfClass = B.index bytes 4
        elfData = B.index bytes 5
        elfType = decodeElfHalfWord elfData (B.index bytes 16) (B.index bytes 17) :: Int
        elfMachine = decodeElfHalfWord elfData (B.index bytes 18) (B.index bytes 19) :: Int

decodeElfHalfWord :: Num a => Word8 -> Word8 -> Word8 -> a
decodeElfHalfWord elfData byte18 byte19
    | elfData == 2 = fromIntegral byte18 * 256 + fromIntegral byte19
    | otherwise = fromIntegral byte18 + fromIntegral byte19 * 256

elfMagic :: B.ByteString
elfMagic = B.pack [0x7f, 0x45, 0x4c, 0x46]

markerSectionAsm :: String -> String -> String
markerSectionAsm label marker = unlines
    [ ".section .rodata"
    , label <> ":"
    , "    .asciz \"" <> escapeAsmString marker <> "\""
    , ".text"
    ]

escapeAsmString :: String -> String
escapeAsmString = concatMap $ \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    c -> [c]

asmCompiler :: IO CompilerCommand
asmCompiler = do
    htccAssembler <- nonEmptyEnv <$> lookupEnv "HTCC_ASSEMBLER"
    compilerSpec <- resolveCompilerCommand $ fromMaybe "gcc" htccAssembler
    ensureX86_64ElfCompiler compilerSpec
    pure compilerSpec

data CompilerProbeFailure
    = CompilerAssemblyProbeFailure
    | CompilerLinkProbeFailure

ensureX86_64ElfCompiler :: CompilerCommand -> IO ()
ensureX86_64ElfCompiler compilerSpec = do
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

        probeCompilerTargets compilerSpec' =
            mapMaybe id <$> mapM (probeCompilerTarget compilerSpec')
                [ "-dumpmachine"
                , "-print-target-triple"
                ]

        probeCompilerTarget compilerSpec' probeArg = do
            catchIOError
                (do
                    (exitCode, stdout', _) <- readCompilerProcessWithExitCode compilerSpec' [probeArg]
                    pure $ case exitCode of
                        ExitSuccess   -> nonEmptyTrimmed stdout'
                        ExitFailure _ -> Nothing
                )
                (const $ pure Nothing)

        probeCompilerInvocation compilerSpec' =
            withProbeFile "htcc-probe-.s" $ \asmPath asmHandle -> do
                withProbeFile "htcc-probe-.o" $ \objPath objHandle -> do
                    let probeMarker = makeProbeMarker asmPath objPath
                    hPutStr asmHandle $ x86_64ElfProbeAsm probeMarker
                    hClose asmHandle
                    setFileMode objPath temporaryWritableMode
                    hClose objHandle
                    let assembleArgs = asmAssembleArgs objPath asmPath
                    probeProcessResult <- probeCommandExitCode compilerSpec' assembleArgs
                    case probeProcessResult of
                        Just ExitSuccess -> do
                            probeTarget <- detectProbeObjectTarget objPath
                            case probeTarget of
                                Just target
                                    | isX86_64ElfTarget target -> do
                                        linkSucceeded <- probeCompilerLink compilerSpec' objPath probeMarker
                                        pure $
                                            if linkSucceeded
                                                then Right target
                                                else Left CompilerLinkProbeFailure
                                    | otherwise -> pure $ Right target
                                Nothing ->
                                    pure $ Left CompilerAssemblyProbeFailure
                        _ ->
                            pure $ Left CompilerAssemblyProbeFailure

        probeCompilerLink compilerSpec' objPath probeMarker =
            withProbeFile "htcc-probe-.out" $ \outputPath outputHandle -> do
                creationMode <- creationMaskedOutputMode
                setFileMode outputPath $
                    stagedOutputMode PreserveReplacementOutputModeKeepingExecutableBits creationMode
                hClose outputHandle
                let linkArgs = asmLinkArgs outputPath objPath
                probeProcessResult <- probeCommandExitCode compilerSpec' linkArgs
                case probeProcessResult of
                    Just ExitSuccess -> validateRunnableLinkedOutput outputPath (Just probeMarker)
                    _                -> pure False

        probeCommandExitCode compilerSpec' args =
            catchIOError
                (do
                    (exitCode, _, _) <- readCompilerProcessWithExitCode compilerSpec' args
                    pure $ Just exitCode
                )
                (const $ pure Nothing)

        withProbeFile prefix action = do
            tmpDir <- getTemporaryDirectory
            (path, handle) <- openTempFile tmpDir prefix
            finally
                (action path handle)
                ( ignoreIOException (hClose handle)
                    *> ignoreIOException (removeFile path)
                )

        makeProbeMarker asmPath objPath =
            "htcc-probe-marker:" <> takeFileName asmPath <> ":" <> takeFileName objPath

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

asmLinkArgs :: FilePath -> FilePath -> [String]
asmLinkArgs outputPath objPath =
    [ "-no-pie"
    , "-o"
    , outputPath
    , objPath
    ]

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
            pure $ maybe False id $ (==) <$> lhsIdentity <*> rhsIdentity

runAsmOutputAliasesInput :: Opts -> IO Bool
runAsmOutputAliasesInput opts = do
    anyM (sameFileAs $ asmOutputPath opts) $ optInput opts

plainOutputAliasesInput :: Opts -> IO Bool
plainOutputAliasesInput opts = maybe
    (pure False)
    (\path -> anyM (sameFileAs path) $ optInput opts)
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

validateOpts :: Opts -> IO ()
validateOpts opts
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

type ParsedInput = (ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
type ParsedInputWithWarnings = (Warnings, ParsedInput)

emitWarnings :: Foldable f => f (M.ParseErrorBundle T.Text Void) -> IO ()
emitWarnings =
    mapM_ (hPutStr stderr . M.errorBundlePretty)

implicitFunctionWarningName :: M.ParseErrorBundle T.Text Void -> Maybe T.Text
implicitFunctionWarningName M.ParseErrorBundle { M.bundleErrors = M.FancyError _ fancyErrors :| [] } = do
    msg <- case Set.toList fancyErrors of
        [M.ErrorFail errMsg] -> Just $ T.pack errMsg
        _                    -> Nothing
    T.stripPrefix (T.pack "warning: the function '") msg
        >>= T.stripSuffix (T.pack "' is not declared.")
implicitFunctionWarningName _ = Nothing

implicitFunctionResolvedAfterMerge :: ParsedInput -> T.Text -> Bool
implicitFunctionResolvedAfterMerge (_, _, _, funcs) name =
    maybe
        False
        (\func -> not (CT.isSCStatic $ PF.fntype func) && not (PF.fnImplicit func))
        $ Map.lookup name funcs

shouldEmitMergedWarning :: ParsedInput -> M.ParseErrorBundle T.Text Void -> Bool
shouldEmitMergedWarning parsedInput warning =
    maybe True
        (not . implicitFunctionResolvedAfterMerge parsedInput)
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
shiftLiteralLabels offset (asts, gvars, lits, funcs) =
    ( map (shiftLiteralLabelsInATree offset) asts
    , Map.map (shiftLiteralLabelsInGVar offset) gvars
    , map (\lit -> lit { ln = ln lit + offset }) lits
    , funcs
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
internalSymbolRenames inputIndex (_, gvars, _, funcs) =
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
renameInternalSymbols inputIndex (asts, gvars, lits, funcs) =
    ( map (renameInternalSymbolsInATree renames) asts
    , Map.mapKeys (renameObjectSymbol renames) $ Map.map (renameInternalSymbolsInGVar renames) gvars
    , lits
    , Map.mapKeys (renameFunctionSymbol renames) funcs
    )
    where
        renames = internalSymbolRenames inputIndex (asts, gvars, lits, funcs)

shiftLiteralLabelsInInputs :: [ParsedInput] -> [ParsedInput]
shiftLiteralLabelsInInputs parsedInputs = snd $ mapAccumL step (0, 0) parsedInputs
    where
        shouldNamespaceInternalSymbols = length parsedInputs > 1
        step (inputIndex, offset) parsed@(_, _, lits, _) =
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

mergeExternalGlobalTypes
    :: CT.StorageClass Integer
    -> CT.StorageClass Integer
    -> Maybe (CT.StorageClass Integer)
mergeExternalGlobalTypes (CT.SCAuto lhs) (CT.SCAuto rhs) =
    CT.SCAuto <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCStatic lhs) (CT.SCStatic rhs) =
    CT.SCStatic <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCRegister lhs) (CT.SCRegister rhs) =
    CT.SCRegister <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeExternalGlobalTypes (CT.SCUndef lhs) (CT.SCUndef rhs) =
    CT.SCUndef <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeExternalGlobalTypes _ _ = Nothing

mergeExternalGlobals :: T.Text -> GVar Integer -> GVar Integer -> Either String (GVar Integer)
mergeExternalGlobals name lhs rhs = case mergeExternalGlobalTypes (gvtype lhs) (gvtype rhs) of
    Nothing ->
        Left $ conflictingExternalDeclarationError name lhs rhs
    Just mergedType
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
        }

definedFunctions :: ParsedInput -> Set.Set T.Text
definedFunctions (asts, _, _, _) =
    Set.fromList
        [ name
        | ATNode (ATDefFunc name _) _ _ _ <- asts
        ]

implicitFunctionCalls :: ParsedInput -> Set.Set T.Text
implicitFunctionCalls (asts, _, _, funcs) =
    foldMap implicitFunctionCallsInATree asts
        `Set.difference` Set.fromList (Map.keys funcs)

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
mergeOutputInputs parsedInputs =
    foldM mergeInput ([], Map.empty, [], Map.empty, Map.empty, Map.empty) (zip [0 :: Int ..] parsedInputs) >>= finalize
    where
        finalize (asts, gvars, lits, funcs, _, _) = do
            (preparedAsts, preparedGVars) <- prepareAsmInput (fmap fst funcs) asts gvars
            pure (preparedAsts, preparedGVars, lits, fmap fst funcs)

        mergeInput (astsAcc, gvarsAcc, litsAcc, funcsAcc, symbolsAcc, staticSymbolsAcc) (inputIndex, (asts, gvars, lits, funcs)) = do
            let parsedInput = (asts, gvars, lits, funcs)
                actualDefinitions = definedFunctions parsedInput
            symbolsAcc' <- foldM (registerImplicitFunction inputIndex staticSymbolsAcc) symbolsAcc $ Set.toList $ implicitFunctionCalls parsedInput
            (symbolsAcc'', staticSymbolsAcc', funcsAcc') <- foldM (registerFunction inputIndex actualDefinitions) (symbolsAcc', staticSymbolsAcc, funcsAcc) $ Map.toList funcs
            (symbolsAcc''', staticSymbolsAcc'', gvarsAcc') <- foldM (registerGlobal inputIndex) (symbolsAcc'', staticSymbolsAcc', gvarsAcc) $ Map.toList gvars
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
                                *> pure symbols
                        _ ->
                            pure symbols

        registerFunction origin actualDefinitions (symbols, staticSymbols, funcsAcc) (name, func)
            | CT.isSCStatic (PF.fntype func) = do
                rejectExternalSymbolConflict origin semanticName newSymbol symbols
                staticSymbols' <- registerStaticSymbol origin semanticName newSymbol staticSymbols
                pure (symbols, staticSymbols', insertFunction name (func, hasBody) funcsAcc)
            | otherwise = do
                rejectStaticSymbolConflict origin semanticName newSymbol staticSymbols
                case Map.lookup semanticName symbols of
                    Nothing ->
                        pure
                            ( insertSymbol origin semanticName newSymbol symbols
                            , staticSymbols
                            , insertFunction name (func, hasBody) funcsAcc
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
                                        , insertFunction name (mergedFunc, mergedHasBody) funcsAcc
                                        )
                                Right _ ->
                                    Left "internal compiler error: unexpected same-input symbol merge result"
                        | otherwise -> case existingSymbol of
                            ExternalImplicitFunction ->
                                mergeExternalFunctions name (implicitExternalFunction, False) (func, hasBody) >>= \_ ->
                                    pure
                                        ( insertSymbol origin semanticName newSymbol symbols
                                        , staticSymbols
                                        , insertFunction name (func, hasBody) funcsAcc
                                        )
                            ExternalFunction existing existingHasBody -> do
                                merged <- mergeExternalFunctions name (existing, existingHasBody) (func, hasBody)
                                pure
                                    ( insertSymbol origin semanticName (uncurry ExternalFunction merged) symbols
                                    , staticSymbols
                                    , insertFunction name merged funcsAcc
                                    )
                            ExternalGlobal _ ->
                                Left $ duplicateExternalSymbolError name
            where
                hasBody = Set.member name actualDefinitions
                semanticName = emittedSymbolName origin (CT.isSCStatic $ PF.fntype func) name
                newSymbol = ExternalFunction func hasBody

        registerGlobal origin (symbols, staticSymbols, gvarsAcc) (name, gvar)
            | CT.isSCStatic (gvtype gvar) = do
                rejectExternalSymbolConflict origin semanticName newSymbol symbols
                staticSymbols' <- registerStaticSymbol origin semanticName newSymbol staticSymbols
                pure (symbols, staticSymbols', Map.insert name gvar gvarsAcc)
            | otherwise = do
                rejectStaticSymbolConflict origin semanticName newSymbol staticSymbols
                case Map.lookup semanticName symbols of
                    Nothing ->
                        pure
                            ( insertSymbol origin semanticName newSymbol symbols
                            , staticSymbols
                            , Map.insert name gvar gvarsAcc
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
                                        , Map.insert name mergedGVar gvarsAcc
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
                                    , Map.insert name merged gvarsAcc
                                    )
            where
                semanticName = emittedSymbolName origin (CT.isSCStatic $ gvtype gvar) name
                newSymbol = ExternalGlobal gvar

        insertFunction name func = Map.insertWith preserveMergedFunctionType name func
        insertSymbol origin name symbol = Map.insert name (origin, symbol)
        insertStaticSymbol origin name symbol = Map.insert (origin, name) symbol

        emittedSymbolName origin isInternal name
            | isInternal =
                denamespaceInternalSymbol (fromIntegral origin) name
            | otherwise =
                name

        rejectExternalSymbolConflict origin name newSymbol symbols = case Map.lookup name symbols of
            Just (existingOrigin, existingSymbol)
                | existingOrigin == origin ->
                    mergeSameOriginExternalSymbol name existingSymbol newSymbol *> pure ()
            _ ->
                pure ()

        rejectStaticSymbolConflict origin name newSymbol staticSymbols = case Map.lookup (origin, name) staticSymbols of
            Just existingSymbol ->
                mergeSameOriginExternalSymbol name existingSymbol newSymbol *> pure ()
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

runAsm :: Maybe Handle -> Opts -> SI.Asm SI.AsmCodeCtx Integer a -> IO a
runAsm outputHandle opts asm
    | optIsRunAsm opts = do
        withReplacementOutputPath PreserveReplacementOutputModeKeepingExecutableBits (asmOutputPath opts) $ \tmpOutputPath -> do
            compilerSpec <- asmCompiler
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
                            setFileMode objPath temporaryWritableMode
                            hClose objHandle
                            let assembleArgs = asmAssembleArgs objPath asmPath
                                linkArgs = asmLinkArgs tmpOutputPath objPath
                            result <- SI.runAsmWithHandle tmpHandle asm
                            hClose tmpHandle
                            when (optIsVerbose opts) $
                                hPutStr stderr $ showCompilerCommandForUser compilerSpec assembleArgs <> "\n"
                            callCompilerProcess compilerSpec assembleArgs
                            when (optIsVerbose opts) $
                                hPutStr stderr $ showCompilerCommandForUser compilerSpec linkArgs <> "\n"
                            callCompilerProcess compilerSpec linkArgs
                            shouldValidateOutput <- shouldValidateRunnableLinkedOutput tmpOutputPath
                            when shouldValidateOutput $ do
                                linkedOutputOk <- validateRunnableLinkedOutput tmpOutputPath Nothing
                                when (not linkedOutputOk) $
                                    ioError . userError $
                                        "HTCC_ASSEMBLER produced a non-runnable final output for -r: "
                                            <> asmOutputPath opts
                            pure result
                        )
                        cleanupObj
                )
                ( ignoreIOException (hClose tmpHandle)
                    *> ignoreIOException (removeFile asmPath)
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
        parserRunner =
            if allowSameInputExternalCollisions
                then PT.runParserAllowSameInputExternalCollisions
                else runParser
        parseInputRawEither fname txt =
            case parserRunner parser fname txt
                :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer) of
                Left x  -> Left x
                Right (warns, asts, gvars, lits, funcs) ->
                    Right (warns, (asts, gvars, lits, funcs))
        parseInputRaw fname txt =
            either
                (\x -> hPutStr stderr (M.errorBundlePretty x) *> exitFailure)
                pure
                (parseInputRawEither fname txt)
        readParsedInputRaw fname =
            readInput fname >>= uncurry parseInputRaw
        readParsedInput fname = do
            (warns, parsedInput) <- readParsedInputRaw fname
            emitWarnings warns
            mergeParsedInputs [parsedInput]
        readMergedInputRaw warnings parsedInputs [] =
            pure (warnings, reverse parsedInputs)
        readMergedInputRaw warnings parsedInputs (fname:fnames) =
            catchIOError
                ( do
                    (inputName, txt) <- readInput fname
                    case parseInputRawEither inputName txt of
                        Left parseErr -> do
                            emitWarnings warnings
                            hPutStr stderr (M.errorBundlePretty parseErr)
                            exitFailure
                        Right (warns, parsedInput) ->
                            readMergedInputRaw
                                (warnings <> toList warns)
                                (parsedInput : parsedInputs)
                                fnames
                )
                (\ioErr -> emitWarnings warnings *> ioError ioErr)
        mergeParsedInputsEither parsedInputs =
            mergeOutputInputs $ shiftLiteralLabelsInInputs parsedInputs
        mergeParsedInputs parsedInputs =
            either
                (\msg -> hPutStr stderr (msg <> "\n") *> exitFailure)
                pure
                (mergeParsedInputsEither parsedInputs)
        readMergedInput = do
            (warnings, parsedInputs) <- readMergedInputRaw [] [] (optInput opts)
            case mergeParsedInputsEither parsedInputs of
                Left msg -> do
                    emitWarnings warnings
                    hPutStr stderr (msg <> "\n")
                    exitFailure
                Right parsedInput -> do
                    emitWarnings
                        [ warning
                        | warning <- warnings
                        , shouldEmitMergedWarning parsedInput warning
                        ]
                    pure parsedInput
        runParsed outputHandle (asts, gvars, lits, _) =
            runAsm outputHandle opts $ casmNormalized' asts gvars lits
        readInput fname = do
            txt <- withFile fname ReadMode $ \h -> do
                txt' <- T.hGetContents h
                _ <- evaluate $ T.foldl' (\n _ -> succ n) (0 :: Int) txt'
                pure txt'
            pure (fname, txt)
    if optIsRunAsm opts
        then forM_ (optInput opts) $ \fname ->
            readParsedInput fname >>= runParsed Nothing
        else maybe
            (case optInput opts of
                [_] ->
                    forM_ (optInput opts) $ \fname ->
                        readParsedInput fname >>= runParsed Nothing
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
