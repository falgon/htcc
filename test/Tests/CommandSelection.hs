module Tests.CommandSelection (
    Command (..),
    autoHtccBinOverride,
    autoHtccBinOverrideFor,
    autoHtccBinOverrideForHost,
    autoHtccBinOverrideWith,
    autoHtccCommand,
    autoHtccCommandFor,
    autoHtccCommandForHost,
    assemblerCommandAvailableInDirectoryWith,
    assemblerCommandAvailableWithDirectories,
    assemblerCommandAvailableWith,
    assemblerCommandAvailableWithTempDirectory,
    collectCommandExitCodes,
    compilerCommandAvailable,
    compilerCommandAvailableWithDirectories,
    compilerCommandAvailableWith,
    commandsToRun,
    defaultCommand,
    defaultCommandFor,
    defaultCommandWithProbes,
    shouldValidateRunnableLinkedOutput,
    validateRunnableLinkedOutput,
    looksRunnableLinkedOutput,
    needsHtccCommandOverride,
    needsSubProcCompilerOverride,
    probeAvailableDirectories,
    resolveCommand,
    resolveCommandWith,
    resolveCommandWithDefault
) where

import           Control.Exception     (AsyncException, IOException,
                                        SomeException, displayException,
                                        finally, fromException, throwIO, try)
import           Data.Bits             (Bits (shiftL, (.&.), (.|.)))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Either           (fromRight)
import           Data.Functor          (($>))
import           Data.List             (foldl', isInfixOf, isPrefixOf)
import           Data.Maybe            (fromMaybe, isJust)
import qualified Data.Text             as T
import           Data.Word             (Word64, Word8)
import           System.Directory      (getCurrentDirectory,
                                        getTemporaryDirectory, removeFile)
import           System.Exit           (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath       (takeFileName)
import           System.Info           (arch, os)
import           System.IO             (Handle, hClose, hPutStr, hPutStrLn,
                                        openTempFile, stderr)
import           System.IO.Error       (catchIOError, isDoesNotExistError)
import           System.Posix.Files    (fileMode, getSymbolicLinkStatus,
                                        groupExecuteMode, intersectFileModes,
                                        isRegularFile, otherExecuteMode,
                                        ownerExecuteMode, ownerReadMode,
                                        setFileMode, unionFileModes)
import           System.Posix.Types    (FileMode)
import           Tests.Utils           (CompilerCommand, absoluteHtccCommand,
                                        absoluteHtccCommandWith,
                                        assemblerCompilerCommand,
                                        htccCommandFor, htccCommandForHost,
                                        probeCompilerShellCommandAvailableInDirectory,
                                        readCompilerProcessWithExitCodeIn)

data Command = WithSubProc | WithDocker | WithSelf | WithComponents
    deriving (Eq, Show)

commandsToRun :: Maybe Command -> Command -> [Command]
commandsToRun Nothing WithSubProc = [WithComponents, WithSubProc]
commandsToRun _ command           = [command]

needsSubProcCompilerOverride :: Maybe Command -> Command -> Bool
needsSubProcCompilerOverride maybeCommand command =
    WithSubProc `elem` commandsToRun maybeCommand command

needsHtccCommandOverride :: Maybe Command -> Command -> Bool
needsHtccCommandOverride maybeCommand command =
    any (`elem` commandsToRun maybeCommand command) [WithSubProc, WithSelf]

defaultCommandFor :: String -> String -> Bool -> Bool -> Command
defaultCommandFor hostOs hostArch compilerAvailable assemblerAvailable
    | supportsSubProcDefault hostOs hostArch
        && compilerAvailable
        && assemblerAvailable =
        WithSubProc
    | otherwise =
        WithComponents

defaultCommandWithProbes :: String -> String -> IO Bool -> IO Bool -> IO Command
defaultCommandWithProbes hostOs hostArch compilerProbe assemblerProbe
    | supportsSubProcDefault hostOs hostArch = do
        compilerAvailable <- compilerProbe
        defaultCommandFor hostOs hostArch compilerAvailable
            <$> if compilerAvailable then assemblerProbe else pure False
    | otherwise =
        pure $ defaultCommandFor hostOs hostArch False False

supportsSubProcDefault :: String -> String -> Bool
supportsSubProcDefault hostOs hostArch =
    hostOs == "linux" && hostArch == "x86_64"

resolveCommandWithDefault :: Command -> Bool -> Maybe Command -> Command
resolveCommandWithDefault autoCommand clean maybeCommand = case maybeCommand of
    Just command ->
        command
    Nothing
        | clean ->
            WithDocker
        | otherwise ->
            autoCommand

defaultCommand :: IO Command
defaultCommand =
    defaultCommandWithProbes os arch compilerCommandAvailable assemblerCommandAvailable

collectCommandExitCodes :: [IO ()] -> IO [ExitCode]
collectCommandExitCodes =
    mapM $ \action -> do
        result <- try action :: IO (Either SomeException ())
        case result of
            Right () ->
                pure ExitSuccess
            Left err ->
                if isJust (fromException err :: Maybe AsyncException)
                    then throwIO err
                    else
                        hPutStrLn stderr (displayException err)
                            $> fromMaybe (ExitFailure 1) (fromException err)

resolveCommandWith :: IO Command -> Bool -> Maybe Command -> IO Command
resolveCommandWith autoCommand clean maybeCommand = case maybeCommand of
    Just command ->
        pure command
    Nothing
        | clean ->
            pure WithDocker
        | otherwise ->
            autoCommand

resolveCommand :: Bool -> Maybe Command -> IO Command
resolveCommand =
    resolveCommandWith defaultCommand

autoHtccCommandFor :: Maybe String -> Maybe FilePath -> T.Text
autoHtccCommandFor =
    htccCommandFor

autoHtccCommandForHost :: String -> Maybe String -> Maybe FilePath -> T.Text
autoHtccCommandForHost =
    htccCommandForHost

autoHtccCommand :: IO T.Text
autoHtccCommand = absoluteHtccCommand

autoHtccBinOverrideFor :: Maybe String -> Maybe FilePath -> Maybe T.Text
autoHtccBinOverrideFor =
    autoHtccBinOverrideForHost os

autoHtccBinOverrideForHost :: String -> Maybe String -> Maybe FilePath -> Maybe T.Text
autoHtccBinOverrideForHost hostOs maybeCompilerCommand maybeRepoBuiltCompilerPath =
    Just $ htccCommandForHost hostOs maybeCompilerCommand maybeRepoBuiltCompilerPath

autoHtccBinOverrideWith :: Maybe String -> Maybe FilePath -> Maybe FilePath -> IO (Maybe T.Text)
autoHtccBinOverrideWith maybeCompilerCommand maybeRepoRoot maybeRepoBuiltCompilerPath =
    Just <$> absoluteHtccCommandWith
        maybeCompilerCommand
        maybeRepoRoot
        maybeRepoBuiltCompilerPath

autoHtccBinOverride :: IO (Maybe T.Text)
autoHtccBinOverride =
    Just <$> absoluteHtccCommand

compilerCommandAvailable :: IO Bool
compilerCommandAvailable =
    compilerCommandAvailableWith autoHtccCommand

compilerCommandAvailableWithDirectories :: [IO FilePath] -> IO T.Text -> IO Bool
compilerCommandAvailableWithDirectories directoryProviders getCompilerCommand =
    probeAvailableDirectories
        directoryProviders
        (`compilerCommandAvailableInDirectoryWith` getCompilerCommand)
    where
        compilerCommandAvailableInDirectoryWith workingDir getCompilerCommand' = do
            result <- try $ do
                compilerCommand <- getCompilerCommand'
                probeCompilerShellCommandAvailableInDirectory workingDir compilerCommand
            pure $ fromRight False (result :: Either IOException Bool)

compilerCommandAvailableWith :: IO T.Text -> IO Bool
compilerCommandAvailableWith =
    compilerCommandAvailableWithDirectories
        [ getTemporaryDirectory
        , getCurrentDirectory
        ]

assemblerCommandAvailable :: IO Bool
assemblerCommandAvailable = do
    result <- try (assemblerCommandAvailableWith $ const assemblerCompilerCommand) :: IO (Either IOException Bool)
    pure $ fromRight False result

assemblerCommandAvailableWithDirectories :: [IO FilePath] -> ([String] -> IO CompilerCommand) -> IO Bool
assemblerCommandAvailableWithDirectories directoryProviders buildAssemblerCommand =
    probeAvailableDirectories
        directoryProviders
        (`assemblerCommandAvailableInDirectoryWith` buildAssemblerCommand)

assemblerCommandAvailableWith :: ([String] -> IO CompilerCommand) -> IO Bool
assemblerCommandAvailableWith =
    assemblerCommandAvailableWithDirectories
        [ getTemporaryDirectory
        , getCurrentDirectory
        ]

assemblerCommandAvailableWithTempDirectory :: IO FilePath -> ([String] -> IO CompilerCommand) -> IO Bool
assemblerCommandAvailableWithTempDirectory getTempDirectory buildAssemblerCommand = do
    tempDir <- getTempDirectory
    assemblerCommandAvailableInDirectoryWith tempDir buildAssemblerCommand

probeAvailableDirectories :: [IO FilePath] -> (FilePath -> IO Bool) -> IO Bool
probeAvailableDirectories directoryProviders probe =
    go [] False True directoryProviders
    where
        go _ sawDirectory allAvailable [] =
            pure $ sawDirectory && allAvailable
        go seenDirectories sawDirectory allAvailable (nextDirectory : remainingDirectoryProviders) = do
            nextDirectoryResult <- try nextDirectory :: IO (Either IOException FilePath)
            case nextDirectoryResult of
                Left _ ->
                    go seenDirectories sawDirectory False remainingDirectoryProviders
                Right directory ->
                    if directory `elem` seenDirectories
                        then go seenDirectories sawDirectory allAvailable remainingDirectoryProviders
                        else do
                            probeResult <- try (probe directory) :: IO (Either IOException Bool)
                            case probeResult of
                                Left _ ->
                                    go (directory : seenDirectories) True False remainingDirectoryProviders
                                Right available ->
                                    go
                                        (directory : seenDirectories)
                                        True
                                        (allAvailable && available)
                                        remainingDirectoryProviders

assemblerCommandAvailableInDirectoryWith :: FilePath -> ([String] -> IO CompilerCommand) -> IO Bool
assemblerCommandAvailableInDirectoryWith workingDir =
    probeAssemblerCommandWith (pure workingDir)

probeAssemblerCommandWith :: IO FilePath -> ([String] -> IO CompilerCommand) -> IO Bool
probeAssemblerCommandWith getWorkingDirectory buildAssemblerCommand = do
    workingDir <- getWorkingDirectory
    withProbeFile (pure workingDir) "htcc-test-probe-.s" $ \asmPath asmHandle -> do
        withProbeFile (pure workingDir) "htcc-test-probe-.o" $ \objPath objHandle -> do
            withProbeFile (pure workingDir) "htcc-test-probe-.out" $ \outputPath outputHandle -> do
                let probeMarker = makeProbeMarker asmPath objPath
                hPutStr asmHandle $ assemblerProbeAsm probeMarker
                hClose asmHandle
                hClose objHandle
                hClose outputHandle
                ignoreIOException $ removeFile outputPath
                assembleResult <- readAssemblerExitCodeWith workingDir buildAssemblerCommand
                    [ "-x"
                    , "assembler"
                    , "-c"
                    , "-o"
                    , objPath
                    , asmPath
                    ]
                case assembleResult of
                    Just ExitSuccess -> do
                        probeTarget <- detectProbeObjectTarget objPath
                        case probeTarget of
                            Just target
                                | isX86_64ElfTarget target -> do
                                    linkResult <- readAssemblerExitCodeWith workingDir buildAssemblerCommand
                                        [ "-no-pie"
                                        , "-o"
                                        , outputPath
                                        , objPath
                                        ]
                                    case linkResult of
                                        Just ExitSuccess ->
                                            validateRunnableLinkedOutput outputPath (Just probeMarker)
                                        _                -> pure False
                                | otherwise ->
                                    pure False
                            Nothing ->
                                pure False
                    _ ->
                        pure False

readAssemblerExitCodeWith :: FilePath -> ([String] -> IO CompilerCommand) -> [String] -> IO (Maybe ExitCode)
readAssemblerExitCodeWith workingDir buildAssemblerCommand args = do
    compiler <- buildAssemblerCommand args
    catchIOError
        (do
            (exitCode, _, _) <- readCompilerProcessWithExitCodeIn (Just workingDir) compiler args
            pure $ Just exitCode
        )
        (const $ pure Nothing)

detectProbeObjectTarget :: FilePath -> IO (Maybe String)
detectProbeObjectTarget path =
    catchIOError
        (do
            status <- getSymbolicLinkStatus path
            if isRegularFile status
                then describeProbeObject <$> B.readFile path
                else pure Nothing
        )
        (const $ pure Nothing)

describeProbeObject :: B.ByteString -> Maybe String
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
        (const $ pure False)

shouldValidateRunnableLinkedOutput :: FilePath -> IO Bool
shouldValidateRunnableLinkedOutput path =
    catchIOError
        (isRegularFile <$> getSymbolicLinkStatus path)
        (\ioErr -> if isDoesNotExistError ioErr then pure True else ioError ioErr)

withReadableFile :: FilePath -> FileMode -> IO a -> IO a
withReadableFile path originalMode action
    | intersectFileModes originalMode ownerReadMode /= 0 = action
    | otherwise = do
        setFileMode path readableMode
        action `finally` setFileMode path originalMode
    where
        readableMode = originalMode `unionFileModes` ownerReadMode

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
                            || ( entryTag /= elfDynamicTagNeeded
                                    && go (entryOffset + fromIntegral elfDynamicEntrySize)
                               )
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
    | not (rangeWithinFile elfProgramHeaderOffset elfProgramHeaderTableSize fileSize) = Nothing
    | otherwise =
        Just LinkedOutputElf
            { linkedOutputElfBytes = bytes
            , linkedOutputElfDataEncoding = elfData
            , linkedOutputElfOsAbi = elfOsAbi
            , linkedOutputElfType = elfType
            , linkedOutputElfFileSize = fileSize
            , linkedOutputElfEntryPoint = elfEntryPoint
            , linkedOutputElfProgramHeaderOffset = elfProgramHeaderOffset
            , linkedOutputElfProgramHeaderEntrySize = elfProgramHeaderEntrySize
            , linkedOutputElfProgramHeaderCount = elfProgramHeaderCount
            }
    where
        fileSize = fromIntegral $ B.length bytes
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
        fileSize = linkedOutputElfProgramHeaderFileSize elf headerOffset
        memorySize = linkedOutputElfProgramHeaderMemorySize elf headerOffset
     in if fileSize == 0
            || fileSize > memorySize
            || not (rangeWithinFile fileOffset fileSize (linkedOutputElfFileSize elf))
            then Nothing
            else Just (fileOffset, fileSize)

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
rangeWithinFile start size fileSize =
    start <= fileSize && size <= fileSize - start

rangeContainsPoint :: Word64 -> Word64 -> Word64 -> Bool
rangeContainsPoint start size point =
    size > 0 && point >= start && point - start < size

executableFileMode :: FileMode
executableFileMode = foldr1 unionFileModes
    [ ownerExecuteMode
    , groupExecuteMode
    , otherExecuteMode
    ]

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

isX86_64ElfTarget :: String -> Bool
isX86_64ElfTarget target =
    let normalizedTarget = map toLower target
     in isX86_64Target normalizedTarget && not (isKnownNonElfTarget normalizedTarget)
    where
        isX86_64Target normalizedTarget =
            "x86_64" `isPrefixOf` normalizedTarget || "amd64" `isPrefixOf` normalizedTarget

        isKnownNonElfTarget normalizedTarget =
            any (`isInfixOf` normalizedTarget)
                [ "apple"
                , "cygwin"
                , "darwin"
                , "mingw"
                , "msvc"
                , "windows"
                ]

withProbeFile :: IO FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
withProbeFile getWorkingDirectory prefix action = do
    workingDir <- getWorkingDirectory
    (path, handle) <- openTempFile workingDir prefix
    finally
        (action path handle)
        ( ignoreIOException (hClose handle)
            *> ignoreIOException (removeFile path)
        )

makeProbeMarker :: FilePath -> FilePath -> String
makeProbeMarker asmPath objPath =
    "htcc-probe-marker:" <> takeFileName asmPath <> ":" <> takeFileName objPath

assemblerProbeAsm :: String -> String
assemblerProbeAsm probeMarker = unlines
    [ ".intel_syntax noprefix"
    , ".section .rodata"
    , "htcc_test_probe_marker:"
    , "    .ascii " <> show probeMarker
    , "    .byte 0"
    , ".text"
    , ".globl main"
    , "main:"
    , "    lea rdx, [rip + htcc_test_probe_marker]"
    , "    xor eax, eax"
    , "    ret"
    ]

ignoreIOException :: IO () -> IO ()
ignoreIOException action =
    catchIOError action $ const (pure ())
