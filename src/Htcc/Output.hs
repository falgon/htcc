module Htcc.Output (
    ReplacementOutputMode (..),
    creationMaskedOutputMode,
    replaceExistingOutputFromPathWith,
    resolveReplacementOutputPath,
    stagedOutputMode,
    temporaryWritableMode,
    withReplacementOutputPathAndResolvedPath,
    withReplacementOutputPath,
) where

import           Control.Exception  (SomeException, catch, displayException,
                                     finally, throwIO)
import           Control.Monad      (when)
import           Data.Bits          (complement)
import qualified Data.ByteString    as B
import           System.Directory   (getTemporaryDirectory, makeAbsolute,
                                     removeDirectory, removeFile, renameFile)
import           System.FilePath    (isRelative, normalise, takeDirectory,
                                     takeFileName, (</>))
import           System.IO          (IOMode (ReadMode, WriteMode), hClose,
                                     openTempFile, withBinaryFile)
import           System.IO.Error    (catchIOError, isDoesNotExistError,
                                     isPermissionError)
import           System.Posix.Files (fileMode, getFileStatus,
                                     getSymbolicLinkStatus, groupExecuteMode,
                                     groupReadMode, groupWriteMode,
                                     intersectFileModes, isRegularFile,
                                     isSymbolicLink, linkCount,
                                     otherExecuteMode, otherReadMode,
                                     otherWriteMode, ownerExecuteMode,
                                     ownerReadMode, ownerWriteMode,
                                     readSymbolicLink, setFileMode,
                                     setGroupIDMode, setUserIDMode,
                                     unionFileModes)
import           System.Posix.IO    (closeFd, createFile)
import           System.Posix.Temp  (mkdtemp)
import           System.Posix.Types (FileMode)

data ReplacementOutputMode
    = PreserveReplacementOutputMode
    | PreserveReplacementOutputModeKeepingExecutableBits

ignoreIOException :: IO () -> IO ()
ignoreIOException = flip catchIOError $ const $ pure ()

defaultOutputFileMode :: FileMode
defaultOutputFileMode = foldr1 unionFileModes
    [ ownerReadMode
    , ownerWriteMode
    , groupReadMode
    , groupWriteMode
    , otherReadMode
    , otherWriteMode
    ]

temporaryWritableMode :: FileMode
temporaryWritableMode = ownerReadMode `unionFileModes` ownerWriteMode

creationMaskedOutputMode :: IO FileMode
creationMaskedOutputMode = do
    tmpDir <- getTemporaryDirectory
    probeDir <- mkdtemp (tmpDir </> "htcc-output-modeXXXXXX")
    let probePath = probeDir </> "mask-probe"
        cleanup =
            ignoreIOException (removeFile probePath)
                *> ignoreIOException (removeDirectory probeDir)
    finally
        ( do
            probeFd <- createFile probePath defaultOutputFileMode
            finally
                ( do
                    probeMode <- fileMode <$> getFileStatus probePath
                    pure $ intersectFileModes probeMode defaultOutputFileMode
                )
                (closeFd probeFd)
        )
        cleanup

resolveReplacementOutputPath :: FilePath -> IO FilePath
resolveReplacementOutputPath = go []
    where
        go seen path = do
            pathKey <- normalise <$> makeAbsolute path
            when (pathKey `elem` seen) $
                ioError . userError $ "cyclic symbolic output path: " <> path
            maybeStatus <- catchIOError
                (Just <$> getSymbolicLinkStatus path)
                (\ioErr -> if isDoesNotExistError ioErr then pure Nothing else ioError ioErr)
            case maybeStatus of
                Just status | isSymbolicLink status -> do
                    target <- readSymbolicLink path
                    let nextPath = normalise $
                            if isRelative target
                                then takeDirectory path </> target
                                else target
                    go (pathKey : seen) nextPath
                _ -> pure path

existingOutputMode :: FilePath -> IO (Maybe FileMode)
existingOutputMode path = catchIOError
    (Just . fileMode <$> getFileStatus path)
    (\ioErr -> if isDoesNotExistError ioErr then pure Nothing else ioError ioErr)

shouldReplaceOutputPath :: FilePath -> IO Bool
shouldReplaceOutputPath path = catchIOError
    (isRegularFile <$> getFileStatus path)
    (\ioErr -> if isDoesNotExistError ioErr then pure True else ioError ioErr)

executableFileMode :: FileMode
executableFileMode = foldr1 unionFileModes
    [ ownerExecuteMode
    , groupExecuteMode
    , otherExecuteMode
    ]

specialFileMode :: FileMode
specialFileMode = foldr1 unionFileModes
    [ setUserIDMode
    , setGroupIDMode
    , 0o1000
    ]

clearSpecialFileMode :: FileMode -> FileMode
clearSpecialFileMode mode =
    intersectFileModes mode $ complement specialFileMode

preservedExecuteMode :: FileMode -> FileMode
preservedExecuteMode mode =
    intersectFileModes mode executableFileMode

minimalRunnableExecuteMode :: FileMode -> FileMode
minimalRunnableExecuteMode mode
    | preservedExecuteMode mode /= 0 = ownerExecuteMode
    | otherwise = 0

replacementExecutableMode :: FileMode -> FileMode -> FileMode
replacementExecutableMode existingMode currentMode =
    preservedExecuteMode existingMode
        `unionFileModes` minimalRunnableExecuteMode replacementMode
    where
        replacementMode = existingMode `unionFileModes` currentMode

stagedOutputMode :: ReplacementOutputMode -> FileMode -> FileMode
stagedOutputMode PreserveReplacementOutputMode _ =
    temporaryWritableMode
stagedOutputMode PreserveReplacementOutputModeKeepingExecutableBits baseMode =
    temporaryWritableMode `unionFileModes` stagedExecuteMode
    where
        stagedExecuteMode
            | existingExecuteMode /= 0 = existingExecuteMode
            | otherwise = ownerExecuteMode
        existingExecuteMode = preservedExecuteMode baseMode

updatedOutputMode :: ReplacementOutputMode -> FileMode -> FileMode -> FileMode
updatedOutputMode PreserveReplacementOutputMode existingMode _ =
    clearSpecialFileMode existingMode
updatedOutputMode PreserveReplacementOutputModeKeepingExecutableBits existingMode currentMode =
    clearSpecialFileMode existingMode
        `unionFileModes` replacementExecutableMode existingMode currentMode

freshOutputMode :: ReplacementOutputMode -> FileMode -> FileMode -> FileMode
freshOutputMode PreserveReplacementOutputMode baseMode _ =
    clearSpecialFileMode baseMode
freshOutputMode PreserveReplacementOutputModeKeepingExecutableBits baseMode currentMode =
    clearSpecialFileMode baseMode
        `unionFileModes` preservedExecuteMode currentMode
        `unionFileModes` minimalRunnableExecuteMode currentMode

copyFileContents :: FilePath -> FilePath -> IO ()
copyFileContents src dst =
    withBinaryFile src ReadMode $ \srcHandle ->
        withBinaryFile dst WriteMode $ \dstHandle ->
            let go = do
                    chunk <- B.hGetSome srcHandle 32768
                    if B.null chunk
                        then pure ()
                        else B.hPut dstHandle chunk *> go
            in go

withReadableSource :: FilePath -> FileMode -> IO a -> IO a
withReadableSource path originalMode action
    | intersectFileModes originalMode ownerReadMode /= 0 = action
    | otherwise = do
        setFileMode path readableMode
        action `finally` setFileMode path originalMode
    where
        readableMode = originalMode `unionFileModes` ownerReadMode

withWritableDestination :: FilePath -> FileMode -> IO a -> IO a
withWritableDestination path originalMode action
    | intersectFileModes originalMode ownerWriteMode /= 0 = action
    | otherwise = do
        setFileMode path writableMode
        action `finally` setFileMode path originalMode
    where
        writableMode = originalMode `unionFileModes` ownerWriteMode

copyExistingOutputToBackup :: FilePath -> FileMode -> FilePath -> IO ()
copyExistingOutputToBackup resolvedOutputPath baseMode backupPath =
    copyFileContents resolvedOutputPath backupPath `catchIOError` \ioErr ->
        if isPermissionError ioErr
            then withReadableSource resolvedOutputPath baseMode $
                copyFileContents resolvedOutputPath backupPath
            else ioError ioErr

ensureInPlaceReplacementSafe :: FilePath -> IO ()
ensureInPlaceReplacementSafe resolvedOutputPath = do
    existingLinkCount <- linkCount <$> getFileStatus resolvedOutputPath
    when (existingLinkCount > 1) $
        ioError . userError $
            "refusing to replace hard-linked output in place: " <> resolvedOutputPath

replaceExistingOutputFromPathWith
    :: (FilePath -> FilePath -> IO ())
    -> ReplacementOutputMode
    -> FilePath
    -> FileMode
    -> FileMode
    -> FilePath
    -> IO ()
replaceExistingOutputFromPathWith copyReplacementOutput modeStrategy resolvedOutputPath baseMode currentMode stagedOutputPath = do
    ensureInPlaceReplacementSafe resolvedOutputPath
    tmpDir <- getTemporaryDirectory
    let backupTemplate = takeFileName resolvedOutputPath <> ".htcc-backup-"
    (backupPath, backupHandle) <- openTempFile tmpDir backupTemplate
    hClose backupHandle
    let restoreOutput = do
            withWritableDestination resolvedOutputPath baseMode $
                copyFileContents backupPath resolvedOutputPath
            setFileMode resolvedOutputPath baseMode
        cleanupBackup =
            ignoreIOException (hClose backupHandle)
                *> ignoreIOException (removeFile backupPath)
        replaceOutput = do
            withWritableDestination resolvedOutputPath baseMode $
                withReadableSource stagedOutputPath currentMode $
                    copyReplacementOutput stagedOutputPath resolvedOutputPath
            setFileMode resolvedOutputPath $
                updatedOutputMode modeStrategy baseMode currentMode
        handleFailure :: SomeException -> IO ()
        handleFailure exc = do
            restoreOutput `catch` rethrowWithRestoreFailure exc
            throwIO exc
        rethrowWithRestoreFailure :: SomeException -> SomeException -> IO ()
        rethrowWithRestoreFailure replacementExc restoreExc =
            ioError . userError $
                "failed to restore original output after replacement failure ("
                    <> displayException replacementExc
                    <> "): "
                    <> displayException restoreExc
    finally
        ( do
            copyExistingOutputToBackup resolvedOutputPath baseMode backupPath
            replaceOutput `catch` handleFailure
        )
        cleanupBackup

replaceExistingOutputFromPath :: ReplacementOutputMode -> FilePath -> FileMode -> FileMode -> FilePath -> IO ()
replaceExistingOutputFromPath =
    replaceExistingOutputFromPathWith copyFileContents

withDirectReplacementOutputPath :: ReplacementOutputMode -> FilePath -> FileMode -> (FilePath -> IO a) -> IO a
withDirectReplacementOutputPath modeStrategy resolvedOutputPath baseMode action = do
    tmpDir <- getTemporaryDirectory
    let outputTemplate = takeFileName resolvedOutputPath <> ".htcc-"
    (tmpOutputPath, tmpOutputHandle) <- openTempFile tmpDir outputTemplate
    finally
        ( do
            setFileMode tmpOutputPath $ stagedOutputMode modeStrategy baseMode
            hClose tmpOutputHandle
            result <- action tmpOutputPath
            currentMode <- fileMode <$> getFileStatus tmpOutputPath
            replaceExistingOutputFromPath modeStrategy resolvedOutputPath baseMode currentMode tmpOutputPath
            pure result
        )
        ( ignoreIOException (hClose tmpOutputHandle)
            *> ignoreIOException (removeFile tmpOutputPath)
        )

withFreshOutputPath :: ReplacementOutputMode -> FilePath -> (FilePath -> IO a) -> IO a
withFreshOutputPath modeStrategy resolvedOutputPath action = do
    let outputDir = takeDirectory resolvedOutputPath
        outputBaseName = takeFileName resolvedOutputPath
        outputDirTemplate = outputBaseName <> ".htcc-XXXXXX"
    tmpOutputDir <- mkdtemp (outputDir </> outputDirTemplate)
    let tmpOutputPath = tmpOutputDir </> outputBaseName
        cleanup =
            ignoreIOException (removeFile tmpOutputPath)
                *> ignoreIOException (removeDirectory tmpOutputDir)
    finally
        ( do
            tmpOutputFd <- createFile tmpOutputPath defaultOutputFileMode
            closeFd tmpOutputFd
            baseMode <- intersectFileModes defaultOutputFileMode . fileMode <$> getFileStatus tmpOutputPath
            setFileMode tmpOutputPath $ stagedOutputMode modeStrategy baseMode
            result <- action tmpOutputPath
            currentMode <- fileMode <$> getFileStatus tmpOutputPath
            setFileMode tmpOutputPath $ freshOutputMode modeStrategy baseMode currentMode
            renameFile tmpOutputPath resolvedOutputPath
            pure result
        )
        cleanup

withReplacementOutputPathAndResolvedPath
    :: ReplacementOutputMode
    -> FilePath
    -> (FilePath -> IO a)
    -> IO (FilePath, a)
withReplacementOutputPathAndResolvedPath modeStrategy outputPath action = do
    resolvedOutputPath <- resolveReplacementOutputPath outputPath
    shouldReplace <- shouldReplaceOutputPath resolvedOutputPath
    if shouldReplace
        then do
            existingMode <- existingOutputMode resolvedOutputPath
            case existingMode of
                Nothing ->
                    do
                        result <- withFreshOutputPath modeStrategy resolvedOutputPath action
                        pure (resolvedOutputPath, result)
                Just baseMode -> do
                    let outputDir = takeDirectory resolvedOutputPath
                        outputTemplate = takeFileName resolvedOutputPath <> ".htcc-"
                        fallbackToDirect ioErr
                            | isPermissionError ioErr =
                                do
                                    result <- withDirectReplacementOutputPath modeStrategy resolvedOutputPath baseMode action
                                    pure (resolvedOutputPath, result)
                            | otherwise =
                                ioError ioErr
                    catchIOError
                        ( do
                            (tmpOutputPath, tmpOutputHandle) <- openTempFile outputDir outputTemplate
                            finally
                                ( do
                                    setFileMode tmpOutputPath $ stagedOutputMode modeStrategy baseMode
                                    hClose tmpOutputHandle
                                    result <- action tmpOutputPath
                                    currentMode <- fileMode <$> getFileStatus tmpOutputPath
                                    setFileMode tmpOutputPath $
                                        updatedOutputMode modeStrategy baseMode currentMode
                                    renameFile tmpOutputPath resolvedOutputPath
                                    pure (resolvedOutputPath, result)
                                )
                                ( ignoreIOException (hClose tmpOutputHandle)
                                    *> ignoreIOException (removeFile tmpOutputPath)
                                )
                        )
                        fallbackToDirect
        else do
            result <- action resolvedOutputPath
            pure (resolvedOutputPath, result)

withReplacementOutputPath :: ReplacementOutputMode -> FilePath -> (FilePath -> IO a) -> IO a
withReplacementOutputPath modeStrategy outputPath action =
    snd <$> withReplacementOutputPathAndResolvedPath modeStrategy outputPath action
