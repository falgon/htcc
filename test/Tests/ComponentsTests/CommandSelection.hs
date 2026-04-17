module Tests.ComponentsTests.CommandSelection (
    test
) where

import           Control.Exception      (AsyncException (ThreadKilled), bracket,
                                         throwIO, try)
import qualified Data.ByteString        as B
import           Data.IORef             (IORef, modifyIORef', newIORef,
                                         readIORef, writeIORef)
import           Data.List              (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Text              as T
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Data.Word              (Word8)
import           System.Directory       (createDirectory,
                                         createDirectoryIfMissing,
                                         findExecutable, getTemporaryDirectory,
                                         removeDirectoryRecursive, removeFile,
                                         setModificationTime,
                                         withCurrentDirectory)
import           System.Environment     (lookupEnv, setEnv, unsetEnv)
import           System.Exit            (ExitCode (..))
import           System.FilePath        (takeDirectory, takeFileName, (</>))
import           System.Info            (arch, os)
import           System.IO              (hClose, hPutStr, openTempFile)
import           System.IO.Error        (catchIOError)
import           System.Posix.Files     (createSymbolicLink, fileMode,
                                         getFileStatus, groupExecuteMode,
                                         intersectFileModes, isRegularFile,
                                         otherExecuteMode, ownerExecuteMode,
                                         ownerModes, setFileMode)
import           Test.HUnit             (Test (..), assertBool, assertEqual,
                                         assertFailure)
import           Tests.CommandSelection (Command (..),
                                         assemblerCommandAvailableInDirectoryWith,
                                         assemblerCommandAvailableWith,
                                         assemblerCommandAvailableWithDirectories,
                                         assemblerCommandAvailableWithTempDirectory,
                                         autoHtccBinOverrideFor,
                                         autoHtccBinOverrideForHost,
                                         autoHtccBinOverrideWith,
                                         autoHtccCommandFor,
                                         autoHtccCommandForHost,
                                         collectCommandExitCodes, commandsToRun,
                                         compilerCommandAvailableWith,
                                         compilerCommandAvailableWithDirectories,
                                         defaultCommandFor,
                                         defaultCommandWithProbes,
                                         looksRunnableLinkedOutput,
                                         needsHtccCommandOverride,
                                         needsSubProcCompilerOverride,
                                         probeAvailableDirectories,
                                         resolveCommandWith,
                                         resolveCommandWithDefault,
                                         shouldValidateRunnableLinkedOutput,
                                         validateRunnableLinkedOutput)
import           Tests.Utils            (CompilerCommand (..),
                                         absoluteHtccCommandWith,
                                         assemblerCompilerCommand,
                                         currentCheckoutRootFromDirectories,
                                         exec, exitCode,
                                         expandWindowsEnvironmentVariablesForTest,
                                         findRepoBuiltExecutableNear,
                                         pinStackLauncherToRepoRoot,
                                         readCompilerProcessWithExitCodeIn,
                                         renderCompilerCommandForHost,
                                         resolveCheckoutRootAndCompilerFromDirectories,
                                         resolveCompilerCommand,
                                         resolveCompilerCommandIn,
                                         resolveCompilerCommandInForHost)

defaultCommandUsesSubProcOnLinuxX86_64Test :: Test
defaultCommandUsesSubProcOnLinuxX86_64Test =
    TestLabel "TestRunner.default-command-uses-subproc-on-linux-x86_64" $ TestCase $
        assertEqual
            "linux/x86_64 should keep subprocess coverage when both external tool probes succeed"
            WithSubProc
            (defaultCommandFor "linux" "x86_64" True True)

defaultCommandFallsBackToComponentsWhenCompilerProbeFailsTest :: Test
defaultCommandFallsBackToComponentsWhenCompilerProbeFailsTest =
    TestLabel "TestRunner.default-command-falls-back-to-components-when-compiler-probe-fails" $ TestCase $
        assertEqual
            "linux/x86_64 should fall back to component coverage when the htcc probe fails"
            WithComponents
            (defaultCommandFor "linux" "x86_64" False True)

defaultCommandFallsBackToComponentsWhenAssemblerProbeFailsTest :: Test
defaultCommandFallsBackToComponentsWhenAssemblerProbeFailsTest =
    TestLabel "TestRunner.default-command-falls-back-to-components-when-assembler-probe-fails" $ TestCase $
        assertEqual
            "linux/x86_64 should fall back to component coverage when the assembler probe fails"
            WithComponents
            (defaultCommandFor "linux" "x86_64" True False)

defaultCommandStaysOnComponentsOffLinuxX86_64Test :: Test
defaultCommandStaysOnComponentsOffLinuxX86_64Test =
    TestLabel "TestRunner.default-command-stays-on-components-off-linux-x86_64" $ TestCase $
        assertEqual
            "non-linux/x86_64 hosts should keep the component test runner"
            WithComponents
            (defaultCommandFor "darwin" "arm64" True True)

autoHtccCommandPrefersEnvOverrideTest :: Test
autoHtccCommandPrefersEnvOverrideTest =
    TestLabel "TestRunner.auto-htcc-command-prefers-env-override" $ TestCase $
        assertEqual
            "HTCC_BIN should still take precedence over the repo-built tool"
            (T.pack "custom htcc command")
            (autoHtccCommandFor (Just "custom htcc command") (Just "/tmp/repo/htcc"))

autoHtccCommandUsesRepoBuiltBinaryWhenUnsetTest :: Test
autoHtccCommandUsesRepoBuiltBinaryWhenUnsetTest =
    TestLabel "TestRunner.auto-htcc-command-uses-repo-built-binary-when-unset" $ TestCase $
        assertEqual
            "the auto-selected runner should POSIX-quote repo-built htcc paths on Unix shells when HTCC_BIN is unset"
            (T.pack "'/tmp/My Projects/repo/htcc'")
            (autoHtccCommandForHost "linux" Nothing (Just "/tmp/My Projects/repo/htcc"))

autoHtccCommandUsesWindowsShellQuotingForRepoBuiltBinaryWhenUnsetTest :: Test
autoHtccCommandUsesWindowsShellQuotingForRepoBuiltBinaryWhenUnsetTest =
    TestLabel "TestRunner.auto-htcc-command-uses-windows-shell-quoting-for-repo-built-binary-when-unset" $ TestCase $
        assertEqual
            "the auto-selected runner should use cmd.exe-compatible quoting for repo-built htcc.exe paths on Windows"
            (T.pack "\"C:\\Users\\Alice Smith\\repo\\htcc.exe\"")
            (autoHtccCommandForHost "mingw32" Nothing (Just "C:\\Users\\Alice Smith\\repo\\htcc.exe"))

autoHtccCommandEscapesWindowsPercentSignsForRepoBuiltBinaryWhenUnsetTest :: Test
autoHtccCommandEscapesWindowsPercentSignsForRepoBuiltBinaryWhenUnsetTest =
    TestLabel "TestRunner.auto-htcc-command-escapes-windows-percent-signs-for-repo-built-binary-when-unset" $ TestCase $
        assertEqual
            "the auto-selected runner should neutralize cmd.exe percent expansion for repo-built htcc.exe paths on Windows"
            (T.pack "\"C:\\work\\100%% ready\\repo\\htcc.exe\"")
            (autoHtccCommandForHost "mingw32" Nothing (Just "C:\\work\\100% ready\\repo\\htcc.exe"))

renderCompilerCommandUsesWindowsShellQuotingTest :: Test
renderCompilerCommandUsesWindowsShellQuotingTest =
    TestLabel "TestRunner.render-compiler-command-uses-windows-shell-quoting" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides =
                        [ ("PATH", "C:\\Program Files\\Toolchain;C:\\bin")
                        , ("SDKROOT", "C:\\Program Files\\Windows Kits")
                        ]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\Program Files\\LLVM\\bin\\clang.exe"
                    , compilerArguments = ["-I", "C:\\Program Files\\SDK\\include"]
                    }
         in assertEqual
                "compiler command helpers should render env overrides and argv with cmd.exe-compatible quoting on Windows"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && set \"PATH=C:\\Program Files\\Toolchain;C:\\bin\""
                        <> " && set \"SDKROOT=C:\\Program Files\\Windows Kits\""
                        <> " && \"C:\\Program Files\\LLVM\\bin\\clang.exe\""
                        <> " \"-I\""
                        <> " \"C:\\Program Files\\SDK\\include\""
                        <> " \"src file.c\""
                )
                (renderCompilerCommandForHost "mingw32" compiler ["src file.c"])

renderCompilerCommandEscapesWindowsPercentSignsAndTrailingBackslashesTest :: Test
renderCompilerCommandEscapesWindowsPercentSignsAndTrailingBackslashesTest =
    TestLabel "TestRunner.render-compiler-command-escapes-windows-percent-signs-and-trailing-backslashes" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides =
                        [("SDKROOT", "C:\\work\\100% ready\\SDK")]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\work\\100% ready\\repo\\htcc.exe"
                    , compilerArguments = ["-I", "C:\\SDK\\include\\"]
                    }
         in assertEqual
                "compiler command helpers should escape cmd.exe percent expansion and preserve trailing backslashes in argv on Windows"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && set \"SDKROOT=C:\\work\\100%% ready\\SDK\""
                        <> " && \"C:\\work\\100%% ready\\repo\\htcc.exe\""
                        <> " \"-I\""
                        <> " \"C:\\SDK\\include\\\\\""
                )
                (renderCompilerCommandForHost "mingw32" compiler [])

renderCompilerCommandPreservesWindowsEnvExpansionInOverridesTest :: Test
renderCompilerCommandPreservesWindowsEnvExpansionInOverridesTest =
    TestLabel "TestRunner.render-compiler-command-preserves-windows-env-expansion-in-overrides" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides =
                        [("PATH", "%PATH%;C:\\work\\100% ready\\toolchain")]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\toolchain\\htcc.exe"
                    , compilerArguments = []
                    }
         in assertEqual
                "compiler command helpers should preserve cmd.exe env expansion in overrides while still escaping literal percent signs"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && set \"PATH=!PATH!;C:\\work\\100%% ready\\toolchain\""
                        <> " && \"C:\\toolchain\\htcc.exe\""
                )
                (renderCompilerCommandForHost "mingw32" compiler [])

renderCompilerCommandPreservesDependentWindowsOverridesTest :: Test
renderCompilerCommandPreservesDependentWindowsOverridesTest =
    TestLabel "TestRunner.render-compiler-command-preserves-dependent-windows-overrides" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides =
                        [ ("SDKROOT", "C:\\Program Files\\Windows Kits")
                        , ("PATH", "%SDKROOT%\\bin;%PATH%")
                        ]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\toolchain\\clang.exe"
                    , compilerArguments = []
                    }
         in assertEqual
                "compiler command helpers should preserve override-to-override dependencies on Windows by using delayed expansion"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && set \"SDKROOT=C:\\Program Files\\Windows Kits\""
                        <> " && set \"PATH=!SDKROOT!\\bin;!PATH!\""
                        <> " && \"C:\\toolchain\\clang.exe\""
                )
                (renderCompilerCommandForHost "mingw32" compiler [])

renderCompilerCommandEscapesQuotedWindowsEnvOverrideValuesTest :: Test
renderCompilerCommandEscapesQuotedWindowsEnvOverrideValuesTest =
    TestLabel "TestRunner.render-compiler-command-escapes-quoted-windows-env-override-values" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides =
                        [("PATH", "\"C:\\Program Files\\SDK\";%PATH%")]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\toolchain\\clang.exe"
                    , compilerArguments = []
                    }
         in assertEqual
                "compiler command helpers should escape embedded quotes inside Windows env override values before rendering set assignments"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && set \"PATH=\"\"C:\\Program Files\\SDK\"\";!PATH!\""
                        <> " && \"C:\\toolchain\\clang.exe\""
                )
                (renderCompilerCommandForHost "mingw32" compiler [])

renderCompilerCommandEnablesDelayedExpansionForWindowsBangPathsTest :: Test
renderCompilerCommandEnablesDelayedExpansionForWindowsBangPathsTest =
    TestLabel "TestRunner.render-compiler-command-enables-delayed-expansion-for-windows-bang-paths" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides = []
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "C:\\work\\bang!dir\\htcc.exe"
                    , compilerArguments = ["-I", "C:\\SDK\\bang!kit\\include"]
                    }
         in assertEqual
                "compiler command helpers should enable delayed expansion whenever a Windows command word contains ! so cmd.exe consumes the escaped bang correctly"
                (T.pack $
                    "setlocal EnableDelayedExpansion"
                        <> " && \"C:\\work\\bang^!dir\\htcc.exe\""
                        <> " \"-I\""
                        <> " \"C:\\SDK\\bang^!kit\\include\""
                        <> " \"C:\\src\\bang^!dir\\main.c\""
                )
                (renderCompilerCommandForHost "mingw32" compiler ["C:\\src\\bang!dir\\main.c"])

renderCompilerCommandPreservesPosixEnvExpansionInOverridesTest :: Test
renderCompilerCommandPreservesPosixEnvExpansionInOverridesTest =
    TestLabel "TestRunner.render-compiler-command-preserves-posix-env-expansion-in-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-posix-env-overrides-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                        pathLogPath = rootDir </> "fake-cc.path"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' \"$PATH\" > " <> shellQuote pathLogPath
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "PATH=${PATH:+$PATH:}$PWD/toolchain fake-cc"
                    let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                    result <- exec $
                        T.pack ("cd " <> shellQuote rootDir <> " && ") <> renderedCommand
                    expandedPath <- readFile pathLogPath
                    assertBool
                        "the rendered helper command should keep braced POSIX PATH expansion syntax instead of single-quoting away $PWD/$PATH expansion"
                        (T.pack "${PATH:+$PATH:}$PWD/toolchain" `T.isInfixOf` renderedCommand
                            && not (T.pack "PATH='${PATH:+$PATH:}$PWD/toolchain'" `T.isInfixOf` renderedCommand)
                        )
                    assertBool
                        "executing the rendered helper command should still expand the PATH override and reach the toolchain-resolved compiler"
                        (exitCode (const False) True result && toolchainDir `isInfixOf` expandedPath)

renderCompilerCommandPreservesTildeExpansionInPosixEnvOverridesTest :: Test
renderCompilerCommandPreservesTildeExpansionInPosixEnvOverridesTest =
    TestLabel "TestRunner.render-compiler-command-preserves-tilde-expansion-in-posix-env-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-tilde-posix-env-overrides-." $ \rootDir -> do
                    let fakeHomeDir = rootDir </> "home"
                        compilerDir = fakeHomeDir
                        compilerPath = compilerDir </> "fake-cc"
                        pathLogPath = rootDir </> "fake-cc.path"
                    createDirectoryIfMissing True compilerDir
                    writeExecutableScript compilerPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' \"$PATH\" > " <> shellQuote pathLogPath
                        ]
                    withEnvVar "HOME" (Just fakeHomeDir) $ do
                        compiler <- resolveCompilerCommandIn (Just rootDir)
                            "PATH=~:$PATH fake-cc"
                        let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                            rerunCommand =
                                "cd " <> shellQuote rootDir <> " && " <> T.unpack renderedCommand
                        result <- exec $
                            T.pack ("bash -lc " <> shellQuote rerunCommand)
                        expandedPath <- readFile pathLogPath
                        assertBool
                            "the rendered helper command should keep leading ~ bare so copied POSIX env overrides still perform tilde expansion"
                            (T.pack "PATH=~:$PATH" `T.isInfixOf` renderedCommand
                                && not (T.pack "PATH=\"~:$PATH\"" `T.isInfixOf` renderedCommand)
                            )
                        assertBool
                            "executing the rendered helper command should still expand ~ in the PATH override and reach the home-relative compiler"
                            (exitCode (const False) True result && compilerDir `isInfixOf` expandedPath)

renderCompilerCommandPreservesLiteralPosixEnvOverridesTest :: Test
renderCompilerCommandPreservesLiteralPosixEnvOverridesTest =
    TestLabel "TestRunner.render-compiler-command-preserves-literal-posix-env-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-literal-posix-env-overrides-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "env-wrapper.sh"
                        logPath = rootDir </> "env-wrapper.log"
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' MSG_QUOTED \"$MSG_QUOTED\" > " <> shellQuote logPath
                        , "printf '%s=%s\\n' MSG_EXPAND \"$MSG_EXPAND\" >> " <> shellQuote logPath
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "MSG_QUOTED='$PWD' MSG_EXPAND=$PWD ./env-wrapper.sh"
                    let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                    result <- exec $
                        T.pack ("cd " <> shellQuote rootDir <> " && ") <> renderedCommand
                    loggedEnv <- lines <$> readFile logPath
                    assertBool
                        "rendered helper commands should keep quoted literal overrides quoted instead of expanding them away"
                        (T.pack "MSG_QUOTED='$PWD'" `T.isInfixOf` renderedCommand
                            && not (T.pack "MSG_QUOTED=$PWD" `T.isInfixOf` renderedCommand)
                        )
                    assertBool
                        "rendered helper commands should keep expandable overrides copy-pastable without single-quoting away $PWD"
                        (T.pack "$PWD" `T.isInfixOf` renderedCommand
                            && not (T.pack "MSG_EXPAND='$PWD'" `T.isInfixOf` renderedCommand)
                        )
                    assertBool
                        "executing the rendered helper command should preserve literal and expandable env override semantics"
                        (exitCode (const False) True result)
                    assertEqual
                        "rendered helper commands should reproduce the same literal versus expanded env values seen by the child process"
                        [ "MSG_QUOTED=$PWD"
                        , "MSG_EXPAND=" <> rootDir
                        ]
                        loggedEnv

renderCompilerCommandPreservesQuotedParameterExpansionWordsTest :: Test
renderCompilerCommandPreservesQuotedParameterExpansionWordsTest =
    TestLabel "TestRunner.render-compiler-command-preserves-quoted-parameter-expansion-words" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-quoted-parameter-word-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "env-wrapper.sh"
                        logPath = rootDir </> "env-wrapper.log"
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' SDKROOT \"$SDKROOT\" > " <> shellQuote logPath
                        ]
                    withEnvVar "SDKROOT" Nothing $ do
                        compiler <- resolveCompilerCommandIn (Just rootDir)
                            "SDKROOT=${SDKROOT:-'$PATH'} ./env-wrapper.sh"
                        let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                        result <- exec $
                            T.pack ("cd " <> shellQuote rootDir <> " && ") <> renderedCommand
                        loggedEnv <- lines <$> readFile logPath
                        assertBool
                            "rendered helper commands should preserve quoted fallback words inside braced parameter expansion so rerunning them stays copy-pasteable"
                            (T.pack "${SDKROOT:-'$PATH'}" `T.isInfixOf` renderedCommand
                                && not (T.pack "${SDKROOT:-$PATH}" `T.isInfixOf` renderedCommand)
                            )
                        assertBool
                            "executing the rendered helper command should keep quoted fallback words literal instead of expanding embedded shell text"
                            (exitCode (const False) True result)
                        assertEqual
                            "rerunning the rendered helper command should reproduce the literal fallback value seen by the child process"
                            ["SDKROOT=$PATH"]
                            loggedEnv

renderCompilerCommandEscapesLiteralPosixMetacharactersInExpandableOverridesTest :: Test
renderCompilerCommandEscapesLiteralPosixMetacharactersInExpandableOverridesTest =
    TestLabel "TestRunner.render-compiler-command-escapes-literal-posix-metacharacters-in-expandable-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-metachar-posix-env-overrides-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "env-wrapper.sh"
                        logPath = rootDir </> "env-wrapper.log"
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' MSG_CMD \"$MSG_CMD\" > " <> shellQuote logPath
                        , "printf '%s=%s\\n' MSG_ARG \"$MSG_ARG\" >> " <> shellQuote logPath
                        , "printf '%s=%s\\n' MSG_BQ \"$MSG_BQ\" >> " <> shellQuote logPath
                        , "printf '%s=%s\\n' MSG_POS \"$MSG_POS\" >> " <> shellQuote logPath
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "MSG_CMD=$(printf_hi) MSG_ARG=$1 MSG_BQ=`printf_hi` MSG_POS=$PWD ./env-wrapper.sh"
                    let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                    result <- exec $
                        T.pack ("cd " <> shellQuote rootDir <> " && ") <> renderedCommand
                    loggedEnv <- lines <$> readFile logPath
                    assertBool
                        "rendered helper commands should escape literal command substitution and positional-parameter syntax instead of letting the shell evaluate it"
                        (all
                            (`T.isInfixOf` renderedCommand)
                            [ T.pack "\\$\\(printf_hi\\)"
                            , T.pack "\\$1"
                            , T.pack "\\`printf_hi\\`"
                            ]
                        )
                    assertBool
                        "rendered helper commands should keep real environment-variable expansion copy-pastable"
                        (T.pack "$PWD" `T.isInfixOf` renderedCommand)
                    assertBool
                        "executing the rendered helper command should preserve literal metacharacters instead of evaluating them"
                        (exitCode (const False) True result)
                    assertEqual
                        "rendered helper commands should reproduce the same literal values proc passes to the child for unsupported shell expansions"
                        [ "MSG_CMD=$(printf_hi)"
                        , "MSG_ARG=$1"
                        , "MSG_BQ=`printf_hi`"
                        , "MSG_POS=" <> rootDir
                        ]
                        loggedEnv

renderCompilerCommandEscapesLeadingHashInExpandableOverridesTest :: Test
renderCompilerCommandEscapesLeadingHashInExpandableOverridesTest =
    TestLabel "TestRunner.render-compiler-command-escapes-leading-hash-in-expandable-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-rendered-leading-hash-posix-env-overrides-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "env-wrapper.sh"
                        logPath = rootDir </> "env-wrapper.log"
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' MSG \"$MSG\" > " <> shellQuote logPath
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "MSG=#leading-comment ./env-wrapper.sh"
                    let renderedCommand = renderCompilerCommandForHost "linux" compiler []
                    result <- exec $
                        T.pack ("cd " <> shellQuote rootDir <> " && ") <> renderedCommand
                    loggedEnv <- lines <$> readFile logPath
                    assertBool
                        "rendered helper commands should escape a leading # in unquoted override words so copy-pasted reruns do not start a shell comment"
                        (T.pack "MSG=\\#leading-comment" `T.isInfixOf` renderedCommand)
                    assertBool
                        "executing the rendered helper command should preserve literal leading # characters in override values"
                        (exitCode (const False) True result)
                    assertEqual
                        "rerunning the rendered helper command should reproduce the literal override value"
                        ["MSG=#leading-comment"]
                        loggedEnv

expandWindowsEnvironmentVariablesMatchesCaseInsensitiveNamesTest :: Test
expandWindowsEnvironmentVariablesMatchesCaseInsensitiveNamesTest =
    TestLabel "TestRunner.expand-windows-environment-variables-matches-case-insensitive-names" $ TestCase $
        assertEqual
            "Windows env expansion should treat variable names case-insensitively so %Path% and %programfiles% resolve from mixed-case environments"
            "C:\\toolchain;C:\\Program Files\\SDK"
            (expandWindowsEnvironmentVariablesForTest
                [ ("Path", "C:\\toolchain")
                , ("ProgramFiles", "C:\\Program Files")
                ]
                '%'
                "%PATH%;%programfiles%\\SDK"
            )

autoHtccCommandKeepsStackExecFallbackWhenRepoBuiltIsMissingTest :: Test
autoHtccCommandKeepsStackExecFallbackWhenRepoBuiltIsMissingTest =
    TestLabel "TestRunner.auto-htcc-command-keeps-stack-exec-fallback-when-repo-built-is-missing" $ TestCase $
        assertEqual
            "the auto-selected runner should stay pinned to stack exec when the repo-built htcc is unavailable"
            (T.pack "stack exec htcc --")
            (autoHtccCommandFor Nothing Nothing)

autoHtccCommandFallsBackToStackExecTest :: Test
autoHtccCommandFallsBackToStackExecTest =
    TestLabel "TestRunner.auto-htcc-command-falls-back-to-stack-exec" $ TestCase $
        assertEqual
            "the auto-selected runner should keep the historical stack exec fallback when no htcc tool is on PATH"
            (T.pack "stack exec htcc --")
            (autoHtccCommandFor Nothing Nothing)

autoHtccBinOverridePrefersEnvOverrideTest :: Test
autoHtccBinOverridePrefersEnvOverrideTest =
    TestLabel "TestRunner.auto-htcc-bin-override-prefers-env-override" $ TestCase $
        assertEqual
            "explicit HTCC_BIN overrides should still be exported to subprocesses"
            (Just $ T.pack "custom htcc command")
            (autoHtccBinOverrideFor (Just "custom htcc command") Nothing)

autoHtccBinOverrideUsesRepoBuiltBinaryWhenUnsetTest :: Test
autoHtccBinOverrideUsesRepoBuiltBinaryWhenUnsetTest =
    TestLabel "TestRunner.auto-htcc-bin-override-uses-repo-built-binary-when-unset" $ TestCase $
        assertEqual
            "repo-built htcc binaries should still be exported to subprocesses when HTCC_BIN is unset"
            (Just $ T.pack "'/tmp/My Projects/repo/htcc'")
            (autoHtccBinOverrideForHost "linux" Nothing (Just "/tmp/My Projects/repo/htcc"))

autoHtccBinOverrideFallsBackToStackExecWhenRepoBuiltIsMissingTest :: Test
autoHtccBinOverrideFallsBackToStackExecWhenRepoBuiltIsMissingTest =
    TestLabel "TestRunner.auto-htcc-bin-override-falls-back-to-stack-exec-when-repo-built-is-missing" $ TestCase $
        assertEqual
            "stack exec fallback commands should still be exported through HTCC_BIN so subprocess tests reuse the probed compiler"
            (Just $ T.pack "stack exec htcc --")
            (autoHtccBinOverrideFor Nothing Nothing)

autoHtccBinOverridePinsStackExecFallbackToCheckoutRootTest :: Test
autoHtccBinOverridePinsStackExecFallbackToCheckoutRootTest =
    TestLabel "TestRunner.auto-htcc-bin-override-pins-stack-exec-fallback-to-checkout-root" $ TestCase $ do
        override <- autoHtccBinOverrideWith Nothing (Just "/tmp/repo") Nothing
        assertBool
            "the exported HTCC_BIN fallback should keep stack pinned to the checkout root when subprocess tests change directories"
            (case override of
                Just command ->
                    all (`T.isInfixOf` command)
                        [ T.pack "--stack-yaml"
                        , T.pack "/tmp/repo/stack.yaml"
                        , T.pack "exec"
                        , T.pack "htcc"
                        ]
                Nothing ->
                    False
            )

pinStackLauncherToRepoRootAddsStackYamlTest :: Test
pinStackLauncherToRepoRootAddsStackYamlTest =
    TestLabel "TestRunner.pin-stack-launcher-to-repo-root-adds-stack-yaml" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides = []
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "/tmp/bin/stack"
                    , compilerArguments = ["exec", "htcc", "--"]
                    }
            pinned = pinStackLauncherToRepoRoot "/tmp/repo" compiler
         in do
            assertEqual
                "stack launchers used from temp directories should stay pinned to the repo's stack.yaml"
                "/tmp/bin/stack"
                (compilerExecutable pinned)
            assertEqual
                "the helper should add --stack-yaml ahead of the original stack arguments"
                ["--stack-yaml", "/tmp/repo/stack.yaml", "exec", "htcc", "--"]
                (compilerArguments pinned)
            assertEqual
                "the helper should not synthesize extra environment overrides"
                []
                (compilerEnvOverrides pinned)

pinStackLauncherToRepoRootRespectsExistingStackYamlPinTest :: Test
pinStackLauncherToRepoRootRespectsExistingStackYamlPinTest =
    TestLabel "TestRunner.pin-stack-launcher-to-repo-root-respects-existing-stack-yaml-pin" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides = [("STACK_YAML", "/tmp/custom/stack.yaml")]
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "/tmp/bin/stack"
                    , compilerArguments = ["exec", "htcc", "--"]
                    }
            pinned = pinStackLauncherToRepoRoot "/tmp/repo" compiler
         in do
            assertEqual
                "an explicit STACK_YAML override should win over the repo-root fallback pin"
                [("STACK_YAML", "/tmp/custom/stack.yaml")]
                (compilerEnvOverrides pinned)
            assertEqual
                "the helper should leave the original stack arguments untouched when already pinned"
                ["exec", "htcc", "--"]
                (compilerArguments pinned)

pinnedStackLauncherRendersPortableHtccBinCommandTest :: Test
pinnedStackLauncherRendersPortableHtccBinCommandTest =
    TestLabel "TestRunner.pinned-stack-launcher-renders-portable-htcc-bin-command" $ TestCase $
        let compiler =
                CompilerCommand
                    { compilerEnvOverrides = []
                    , compilerEnvOverrideSpecs = Nothing
                    , compilerExecutable = "/tmp/bin/stack"
                    , compilerArguments = ["exec", "htcc", "--"]
                    }
            pinned = pinStackLauncherToRepoRoot "/tmp/repo" compiler
         in assertEqual
                "the HTCC_BIN override should keep stack pinned to the checkout root when tests change directories"
                (T.pack "'/tmp/bin/stack' '--stack-yaml' '/tmp/repo/stack.yaml' 'exec' 'htcc' '--'")
                (renderCompilerCommandForHost "linux" pinned [])

absoluteHtccCommandPreservesExplicitRelativeEnvOverrideTest :: Test
absoluteHtccCommandPreservesExplicitRelativeEnvOverrideTest =
    TestLabel "TestRunner.absolute-htcc-command-preserves-explicit-relative-env-override" $ TestCase $
        do
            command <- absoluteHtccCommandWith (Just "bin/htcc-relative") Nothing Nothing
            assertEqual
                "explicit HTCC_BIN overrides should be forwarded verbatim instead of being reparsed and rewritten"
                (T.pack "bin/htcc-relative")
                command

absoluteHtccCommandPreservesExplicitShellEnvOverrideTest :: Test
absoluteHtccCommandPreservesExplicitShellEnvOverrideTest =
    TestLabel "TestRunner.absolute-htcc-command-preserves-explicit-shell-env-override" $ TestCase $
        do
            command <- absoluteHtccCommandWith
                (Just "PATH=$PWD/toolchain:$PATH stack exec htcc --")
                Nothing
                Nothing
            assertEqual
                "explicit HTCC_BIN overrides that depend on shell expansion should stay verbatim"
                (T.pack "PATH=$PWD/toolchain:$PATH stack exec htcc --")
                command

absoluteHtccCommandFallsBackToStackExecWhenRepoBuiltBinaryIsStaleTest :: Test
absoluteHtccCommandFallsBackToStackExecWhenRepoBuiltBinaryIsStaleTest =
    TestLabel "TestRunner.absolute-htcc-command-falls-back-to-stack-exec-when-repo-built-binary-is-stale" $ TestCase $
        withTempDirectory "htcc-test-stale-repo-built-." $ \rootDir -> do
            let compilerPath = rootDir <> "/build/htcc"
                sourcePath = rootDir <> "/src/Htcc/Freshness.hs"
            ensureRepoRootMarker rootDir
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Freshness where\n"
            ensureExecutablePlaceholder compilerPath
            now <- getCurrentTime
            setModificationTime compilerPath $ addUTCTime (-120) now
            setModificationTime sourcePath $ addUTCTime 120 now
            command <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
            assertBool
                "stale repo-built htcc binaries should not shadow the stack exec fallback when compiler sources are newer"
                (not (T.pack compilerPath `T.isInfixOf` command)
                    && all
                        (`T.isInfixOf` command)
                        [ T.pack "--stack-yaml"
                        , T.pack (rootDir <> "/stack.yaml")
                        , T.pack "exec"
                        , T.pack "htcc"
                        ]
                )

absoluteHtccCommandKeepsFreshRepoBuiltBinaryTest :: Test
absoluteHtccCommandKeepsFreshRepoBuiltBinaryTest =
    TestLabel "TestRunner.absolute-htcc-command-keeps-fresh-repo-built-binary" $ TestCase $
        withTempDirectory "htcc-test-fresh-repo-built-." $ \rootDir -> do
            let compilerPath = rootDir <> "/build/htcc"
                sourcePath = rootDir <> "/src/Htcc/Freshness.hs"
            ensureRepoRootMarker rootDir
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Freshness where\n"
            ensureExecutablePlaceholder compilerPath
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-120) now
            setModificationTime compilerPath $ addUTCTime 120 now
            command <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
            assertEqual
                "fresh repo-built htcc binaries should still be preferred over stack exec"
                (T.pack $ shellQuote compilerPath)
                command

absoluteHtccCommandRechecksRepoBuiltBinaryFreshnessAcrossCallsTest :: Test
absoluteHtccCommandRechecksRepoBuiltBinaryFreshnessAcrossCallsTest =
    TestLabel "TestRunner.absolute-htcc-command-rechecks-repo-built-binary-freshness-across-calls" $ TestCase $
        withTempDirectory "htcc-test-rechecked-repo-built-freshness-." $ \rootDir -> do
            let compilerPath = rootDir <> "/build/htcc"
                sourcePath = rootDir <> "/src/Htcc/Freshness.hs"
            ensureRepoRootMarker rootDir
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Freshness where\n"
            ensureExecutablePlaceholder compilerPath
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime compilerPath $ addUTCTime 60 now
            freshCommand <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
            setModificationTime sourcePath $ addUTCTime 120 now
            staleCommand <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
            assertEqual
                "the first resolution should prefer a fresh repo-built htcc"
                (T.pack $ shellQuote compilerPath)
                freshCommand
            assertBool
                "later source updates should invalidate an earlier freshness result instead of reusing a stale repo-built htcc selection"
                (not (T.pack compilerPath `T.isInfixOf` staleCommand)
                    && all
                        (`T.isInfixOf` staleCommand)
                        [ T.pack "--stack-yaml"
                        , T.pack (rootDir <> "/stack.yaml")
                        , T.pack "exec"
                        , T.pack "htcc"
                        ]
                )

absoluteHtccCommandIgnoresGeneratedArtifactsInFreshnessCheckTest :: Test
absoluteHtccCommandIgnoresGeneratedArtifactsInFreshnessCheckTest =
    TestLabel "TestRunner.absolute-htcc-command-ignores-generated-artifacts-in-freshness-check" $ TestCase $
        withTempDirectory "htcc-test-generated-artifact-freshness-." $ \rootDir -> do
            let compilerPath = rootDir <> "/build/htcc"
                sourcePath = rootDir <> "/src/Htcc/Freshness.hs"
                generatedArtifactPath = rootDir <> "/src/Htcc/Freshness.o"
            ensureRepoRootMarker rootDir
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Freshness where\n"
            writeFile generatedArtifactPath ""
            ensureExecutablePlaceholder compilerPath
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime compilerPath $ addUTCTime 60 now
            setModificationTime generatedArtifactPath $ addUTCTime 120 now
            command <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
            assertEqual
                "newer .o/.hi-style artifacts under src/ should not make a fresh repo-built htcc fall back to stack exec"
                (T.pack $ shellQuote compilerPath)
                command

absoluteHtccCommandFallsBackToStackExecWhenCabalProjectIsNewerTest :: Test
absoluteHtccCommandFallsBackToStackExecWhenCabalProjectIsNewerTest =
    TestLabel "TestRunner.absolute-htcc-command-falls-back-to-stack-exec-when-cabal-project-is-newer" $ TestCase $
        assertNewerProjectConfigInvalidatesRepoBuiltBinary
            "htcc-test-cabal-project-freshness-."
            "cabal.project"
            "packages: .\n"
            "a newer cabal.project should invalidate a repo-built htcc so test runners follow the current checkout configuration"

absoluteHtccCommandFallsBackToStackExecWhenCabalProjectLocalIsNewerTest :: Test
absoluteHtccCommandFallsBackToStackExecWhenCabalProjectLocalIsNewerTest =
    TestLabel "TestRunner.absolute-htcc-command-falls-back-to-stack-exec-when-cabal-project-local-is-newer" $ TestCase $
        assertNewerProjectConfigInvalidatesRepoBuiltBinary
            "htcc-test-cabal-project-local-freshness-."
            "cabal.project.local"
            "package *\nflags: +dev\n"
            "a newer cabal.project.local should invalidate a repo-built htcc so test runners do not reuse binaries built with stale local config"

absoluteHtccCommandFallsBackToStackExecWhenCabalProjectFreezeIsNewerTest :: Test
absoluteHtccCommandFallsBackToStackExecWhenCabalProjectFreezeIsNewerTest =
    TestLabel "TestRunner.absolute-htcc-command-falls-back-to-stack-exec-when-cabal-project-freeze-is-newer" $ TestCase $
        assertNewerProjectConfigInvalidatesRepoBuiltBinary
            "htcc-test-cabal-project-freeze-freshness-."
            "cabal.project.freeze"
            "constraints: any.base ==4.18.2.1\n"
            "a newer cabal.project.freeze should invalidate a repo-built htcc so test runners follow the current dependency lock"

assertNewerProjectConfigInvalidatesRepoBuiltBinary :: String -> FilePath -> String -> String -> IO ()
assertNewerProjectConfigInvalidatesRepoBuiltBinary tempPrefix configFile contents failureMessage =
    withTempDirectory tempPrefix $ \rootDir -> do
        let compilerPath = rootDir <> "/build/htcc"
            sourcePath = rootDir <> "/src/Htcc/Freshness.hs"
            projectConfigPath = rootDir </> configFile
        ensureRepoRootMarker rootDir
        createDirectoryIfMissing True $ takeDirectory sourcePath
        writeFile sourcePath "module Htcc.Freshness where\n"
        writeFile projectConfigPath contents
        ensureExecutablePlaceholder compilerPath
        now <- getCurrentTime
        setModificationTime sourcePath $ addUTCTime (-240) now
        setModificationTime compilerPath $ addUTCTime 60 now
        setModificationTime projectConfigPath $ addUTCTime 120 now
        command <- absoluteHtccCommandWith Nothing (Just rootDir) (Just compilerPath)
        assertBool
            failureMessage
            (not (T.pack compilerPath `T.isInfixOf` command)
                && all
                    (`T.isInfixOf` command)
                    [ T.pack "--stack-yaml"
                    , T.pack (rootDir <> "/stack.yaml")
                    , T.pack "exec"
                    , T.pack "htcc"
                    ]
            )

absoluteHtccCommandFallsBackToStackExecWhenRepoRootIsUnknownTest :: Test
absoluteHtccCommandFallsBackToStackExecWhenRepoRootIsUnknownTest =
    TestLabel "TestRunner.absolute-htcc-command-falls-back-to-stack-exec-when-repo-root-is-unknown" $ TestCase $
        withTempDirectory "htcc-test-unknown-root-." $ \sandboxDir -> do
            let compilerPath = sandboxDir <> "/build/htcc"
            ensureExecutablePlaceholder compilerPath
            command <- absoluteHtccCommandWith Nothing Nothing (Just compilerPath)
            assertBool
                "without a checkout root, auto-selection should not trust sibling repo-built htcc artifacts"
                (not (T.pack compilerPath `T.isInfixOf` command)
                    && all
                        (`T.isInfixOf` command)
                        [ T.pack "stack"
                        , T.pack "exec"
                        , T.pack "htcc"
                        ]
                )

resolveCommandPrefersExplicitCommandTest :: Test
resolveCommandPrefersExplicitCommandTest =
    TestLabel "TestRunner.resolve-command-prefers-explicit-command" $ TestCase $
        assertEqual
            "an explicit subcommand should win over clean/default selection"
            WithSelf
            (resolveCommandWithDefault WithComponents True (Just WithSelf))

resolveCommandUsesDockerForCleanWithoutExplicitCommandTest :: Test
resolveCommandUsesDockerForCleanWithoutExplicitCommandTest =
    TestLabel "TestRunner.resolve-command-uses-docker-for-clean-without-explicit-command" $ TestCase $
        assertEqual
            "clean without an explicit subcommand should still select docker"
            WithDocker
            (resolveCommandWithDefault WithSubProc True Nothing)

commandsToRunIncludesComponentsForAutoSelectedSubProcTest :: Test
commandsToRunIncludesComponentsForAutoSelectedSubProcTest =
    TestLabel "TestRunner.commands-to-run-includes-components-for-auto-selected-subproc" $ TestCase $
        assertEqual
            "the default no-arg runner should keep component coverage before subprocess tests"
            [WithComponents, WithSubProc]
            (commandsToRun Nothing WithSubProc)

commandsToRunKeepsExplicitSubProcIsolatedTest :: Test
commandsToRunKeepsExplicitSubProcIsolatedTest =
    TestLabel "TestRunner.commands-to-run-keeps-explicit-subproc-isolated" $ TestCase $
        assertEqual
            "the explicit subp subcommand should still run only the subprocess suite"
            [WithSubProc]
            (commandsToRun (Just WithSubProc) WithSubProc)

needsSubProcCompilerOverrideForAutoSelectedSubProcTest :: Test
needsSubProcCompilerOverrideForAutoSelectedSubProcTest =
    TestLabel "TestRunner.needs-subproc-compiler-override-for-auto-selected-subproc" $ TestCase $
        assertEqual
            "the default no-arg runner should pass the auto-detected compiler into subprocess tests"
            True
            (needsSubProcCompilerOverride Nothing WithSubProc)

needsSubProcCompilerOverrideForExplicitSubProcTest :: Test
needsSubProcCompilerOverrideForExplicitSubProcTest =
    TestLabel "TestRunner.needs-subproc-compiler-override-for-explicit-subproc" $ TestCase $
        assertEqual
            "the explicit subp subcommand should also pass the auto-detected compiler into subprocess tests"
            True
            (needsSubProcCompilerOverride (Just WithSubProc) WithSubProc)

needsSubProcCompilerOverrideSkipsNonSubProcCommandsTest :: Test
needsSubProcCompilerOverrideSkipsNonSubProcCommandsTest =
    TestLabel "TestRunner.needs-subproc-compiler-override-skips-non-subproc-commands" $ TestCase $
        assertEqual
            "commands that do not execute the subprocess suite should not override HTCC_BIN"
            False
            (needsSubProcCompilerOverride Nothing WithComponents)

needsHtccCommandOverrideForExplicitSelfTest :: Test
needsHtccCommandOverrideForExplicitSelfTest =
    TestLabel "TestRunner.needs-htcc-command-override-for-explicit-self" $ TestCase $
        assertEqual
            "the explicit self subcommand should reuse the auto-detected htcc command"
            True
            (needsHtccCommandOverride (Just WithSelf) WithSelf)

needsHtccCommandOverrideSkipsCommandsWithoutHtccExecutionTest :: Test
needsHtccCommandOverrideSkipsCommandsWithoutHtccExecutionTest =
    TestLabel "TestRunner.needs-htcc-command-override-skips-commands-without-htcc-execution" $ TestCase $
        assertEqual
            "commands that do not invoke htcc should not request an override"
            False
            (needsHtccCommandOverride Nothing WithComponents)

defaultCommandRunsProbesOnLinuxX86_64Test :: Test
defaultCommandRunsProbesOnLinuxX86_64Test =
    TestLabel "TestRunner.default-command-runs-probes-on-linux-x86_64" $ TestCase $ do
        compilerProbeCalls <- newProbeCallCounter
        assemblerProbeCalls <- newProbeCallCounter
        command <- defaultCommandWithProbes
            "linux"
            "x86_64"
            (recordProbeCall compilerProbeCalls True)
            (recordProbeCall assemblerProbeCalls True)
        assertEqual
            "linux/x86_64 should keep subprocess coverage when both probes succeed"
            WithSubProc
            command
        assertEqual
            "the compiler probe should run on linux/x86_64 before selecting the default runner"
            1
            =<< readIORef compilerProbeCalls
        assertEqual
            "the assembler probe should run after the compiler probe succeeds"
            1
            =<< readIORef assemblerProbeCalls

defaultCommandSkipsAssemblerProbeAfterCompilerFailureTest :: Test
defaultCommandSkipsAssemblerProbeAfterCompilerFailureTest =
    TestLabel "TestRunner.default-command-skips-assembler-probe-after-compiler-failure" $ TestCase $ do
        compilerProbeCalls <- newProbeCallCounter
        assemblerProbeCalls <- newProbeCallCounter
        command <- defaultCommandWithProbes
            "linux"
            "x86_64"
            (recordProbeCall compilerProbeCalls False)
            (recordProbeCall assemblerProbeCalls True)
        assertEqual
            "linux/x86_64 should fall back to component coverage when the compiler probe fails"
            WithComponents
            command
        assertEqual
            "the compiler probe should still run before giving up on subprocess coverage"
            1
            =<< readIORef compilerProbeCalls
        assertEqual
            "the assembler probe should be skipped once the compiler probe has already failed"
            0
            =<< readIORef assemblerProbeCalls

defaultCommandFallsBackToComponentsWhenCompilerProbeThrowsTest :: Test
defaultCommandFallsBackToComponentsWhenCompilerProbeThrowsTest =
    TestLabel "TestRunner.default-command-falls-back-to-components-when-compiler-probe-throws" $ TestCase $ do
        assemblerProbeCalls <- newProbeCallCounter
        command <- defaultCommandWithProbes
            "linux"
            "x86_64"
            (compilerCommandAvailableWith $ ioError $ userError "missing stack")
            (recordProbeCall assemblerProbeCalls True)
        assertEqual
            "linux/x86_64 should fall back to component coverage when compiler command resolution throws"
            WithComponents
            command
        assertEqual
            "the assembler probe should still be skipped when compiler command resolution aborts"
            0
            =<< readIORef assemblerProbeCalls

defaultCommandSkipsProbesOffLinuxX86_64Test :: Test
defaultCommandSkipsProbesOffLinuxX86_64Test =
    TestLabel "TestRunner.default-command-skips-probes-off-linux-x86_64" $ TestCase $ do
        compilerProbeCalls <- newProbeCallCounter
        assemblerProbeCalls <- newProbeCallCounter
        command <- defaultCommandWithProbes
            "darwin"
            "arm64"
            (recordProbeCall compilerProbeCalls True)
            (recordProbeCall assemblerProbeCalls True)
        assertEqual
            "non-linux/x86_64 hosts should not spend time probing external commands"
            WithComponents
            command
        assertEqual
            "the compiler probe should not run on non-linux/x86_64 hosts"
            0
            =<< readIORef compilerProbeCalls
        assertEqual
            "the assembler probe should not run on non-linux/x86_64 hosts"
            0
            =<< readIORef assemblerProbeCalls

collectCommandExitCodesContinuesAfterFailureTest :: Test
collectCommandExitCodesContinuesAfterFailureTest =
    TestLabel "TestRunner.collect-command-exit-codes-continues-after-failure" $ TestCase $ do
        steps <- newIORef ([] :: [Int])
        exitCodes <- collectCommandExitCodes
            [ recordStep steps 1 *> throwIO (ExitFailure 1)
            , recordStep steps 2
            , recordStep steps 3 *> throwIO (ExitFailure 2)
            ]
        assertEqual
            "the runner should capture every suite result even after an earlier exitFailure"
            [ExitFailure 1, ExitSuccess, ExitFailure 2]
            exitCodes
        assertEqual
            "the runner should continue into later suites after an earlier exitFailure"
            [1, 2, 3]
            =<< readIORef steps

collectCommandExitCodesConvertsUnexpectedExceptionsTest :: Test
collectCommandExitCodesConvertsUnexpectedExceptionsTest =
    TestLabel "TestRunner.collect-command-exit-codes-converts-unexpected-exceptions" $ TestCase $ do
        steps <- newIORef ([] :: [Int])
        exitCodes <- collectCommandExitCodes
            [ recordStep steps 1
            , recordStep steps 2 *> throwIO (userError "suite exploded")
            , recordStep steps 3
            ]
        assertEqual
            "the runner should treat non-ExitCode failures as a failing suite and keep going"
            [ExitSuccess, ExitFailure 1, ExitSuccess]
            exitCodes
        assertEqual
            "the runner should continue into later suites after a non-ExitCode exception"
            [1, 2, 3]
            =<< readIORef steps

collectCommandExitCodesRethrowsAsyncExceptionsTest :: Test
collectCommandExitCodesRethrowsAsyncExceptionsTest =
    TestLabel "TestRunner.collect-command-exit-codes-rethrows-async-exceptions" $ TestCase $ do
        steps <- newIORef ([] :: [Int])
        result <- try
            (collectCommandExitCodes
                [ recordStep steps 1 *> throwIO ThreadKilled
                , recordStep steps 2
                ]
            ) :: IO (Either AsyncException [ExitCode])
        case result of
            Left ThreadKilled ->
                pure ()
            Left err ->
                assertFailure $ "expected ThreadKilled, got " <> show err
            Right exitCodes ->
                assertFailure $ "expected async exception, got " <> show exitCodes
        assertEqual
            "the runner should abort immediately instead of continuing into later suites after an async exception"
            [1]
            =<< readIORef steps

resolveCommandSkipsAutoDetectionForExplicitCommandTest :: Test
resolveCommandSkipsAutoDetectionForExplicitCommandTest =
    TestLabel "TestRunner.resolve-command-skips-auto-detection-for-explicit-command" $ TestCase $ do
        autoCommandCalls <- newProbeCallCounter
        command <- resolveCommandWith
            (recordProbeCall autoCommandCalls WithComponents)
            True
            (Just WithSelf)
        assertEqual
            "an explicit subcommand should return immediately"
            WithSelf
            command
        assertEqual
            "auto-detection should not run when the user already chose a subcommand"
            0
            =<< readIORef autoCommandCalls

resolveCommandSkipsAutoDetectionForCleanWithoutCommandTest :: Test
resolveCommandSkipsAutoDetectionForCleanWithoutCommandTest =
    TestLabel "TestRunner.resolve-command-skips-auto-detection-for-clean-without-command" $ TestCase $ do
        autoCommandCalls <- newProbeCallCounter
        command <- resolveCommandWith
            (recordProbeCall autoCommandCalls WithComponents)
            True
            Nothing
        assertEqual
            "clean without an explicit subcommand should return docker immediately"
            WithDocker
            command
        assertEqual
            "auto-detection should not run when --clean already determines the command"
            0
            =<< readIORef autoCommandCalls

assemblerCommandAvailableHandlesLeadingEnvAssignmentsTest :: Test
assemblerCommandAvailableHandlesLeadingEnvAssignmentsTest =
    TestLabel "TestRunner.assembler-command-available-handles-leading-env-assignments" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else do
                compiler <- assemblerCompilerCommand
                withTempDirectory "htcc-test-assembler-bin-." $ \binDir -> do
                    let scriptPath = binDir <> "/htcc-test-assembler"
                    writeExecutableScript scriptPath (assemblerWrapperScript compiler)
                    available <- assemblerCommandAvailableWith $ const $
                        resolveCompilerCommand $
                            "PATH=" <> binDir <> " " <> takeFileName scriptPath
                    assertEqual
                        "leading PATH assignments should still be treated as environment overrides during probing"
                        True
                        available

compilerCommandAvailableRunsConfiguredCommandTest :: Test
compilerCommandAvailableRunsConfiguredCommandTest =
    TestLabel "TestRunner.compiler-command-available-runs-configured-command" $ TestCase $
        withTempDirectory "htcc-test-bin-." $ \binDir -> do
            let stackPath = binDir <> "/stack"
                logPath = binDir <> "/stack.log"
            writeExecutableScript stackPath $ unlines
                [ "#!/bin/sh"
                , "printf '%s\\n' \"$@\" > " <> shellQuote logPath
                , "last_arg=''"
                , "for arg in \"$@\"; do"
                , "  last_arg=\"$arg\""
                , "done"
                , "[ -n \"$last_arg\" ]"
                , "[ -f \"$last_arg\" ]"
                , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                ]
            available <- compilerCommandAvailableWith $
                pure . T.pack $ shellQuote stackPath <> " exec htcc --"
            loggedArgs <- lines <$> readFile logPath
            let compilerSourcePath = last loggedArgs
            assertEqual
                "the compiler probe should execute the same command shape used by subprocess tests"
                True
                available
            assertEqual
                "the compiler probe should preserve the configured launcher prefix before appending the probe source path"
                ["exec", "htcc", "--"]
                (take 3 loggedArgs)
            assertBool
                "the compiler probe should compile a temporary C source file instead of treating --help success as sufficient"
                (".c" `isSuffixOf` compilerSourcePath)

compilerCommandAvailableHandlesShellExpandedPathAssignmentsTest :: Test
compilerCommandAvailableHandlesShellExpandedPathAssignmentsTest =
    TestLabel "TestRunner.compiler-command-available-handles-shell-expanded-path-assignments" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-shell-path-bin-." $ \rootDir -> do
                    let binDir = rootDir </> "toolchain"
                        stackPath = binDir </> "stack"
                        logPath = binDir </> "stack.log"
                    createDirectory binDir
                    writeExecutableScript stackPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' \"$@\" > " <> shellQuote logPath
                        , "last_arg=''"
                        , "for arg in \"$@\"; do"
                        , "  last_arg=\"$arg\""
                        , "done"
                        , "[ -n \"$last_arg\" ]"
                        , "[ -f \"$last_arg\" ]"
                        , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                        ]
                    available <- compilerCommandAvailableWithDirectories
                        [ pure rootDir ]
                        (pure $ T.pack "PATH=$PWD/toolchain:$PATH stack exec htcc --")
                    loggedArgs <- lines <$> readFile logPath
                    let compilerSourcePath = last loggedArgs
                    assertEqual
                        "shell-expanded PATH assignments in HTCC_BIN should remain usable during probing"
                        True
                        available
                    assertEqual
                        "the shell probe should still append the compiler source path after the configured launcher"
                        ["exec", "htcc", "--"]
                        (take 3 loggedArgs)
                    assertBool
                        "the shell probe should compile a temporary C source file after expanding $PWD in PATH"
                        (".c" `isSuffixOf` compilerSourcePath)

assemblerCommandAvailableRejectsWorkingDirectoryFallbackWhenPathIsOverriddenTest :: Test
assemblerCommandAvailableRejectsWorkingDirectoryFallbackWhenPathIsOverriddenTest =
    TestLabel "TestRunner.assembler-command-available-rejects-working-directory-fallback-when-path-is-overridden" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else do
                compiler <- assemblerCompilerCommand
                withTempDirectory "htcc-test-assembler-explicit-path-no-fallback-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        wrapperName = "htcc-test-assembler"
                    createDirectory toolchainDir
                    writeExecutableScript (rootDir </> wrapperName) (assemblerWrapperScript compiler)
                    available <- assemblerCommandAvailableWithDirectories
                        [pure rootDir]
                        (const $
                            resolveCompilerCommandIn
                                (Just rootDir)
                                ("PATH=./toolchain " <> wrapperName)
                        )
                    assertEqual
                        "assembler probing should not fall back to ./<name> after an explicit PATH override omits the wrapper"
                        False
                        available

readCompilerProcessWithExitCodeExpandsPosixStyleEnvOverridesTest :: Test
readCompilerProcessWithExitCodeExpandsPosixStyleEnvOverridesTest =
    TestLabel "TestRunner.read-compiler-process-with-exit-code-expands-posix-style-env-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-expanded-env-overrides-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "sdk" </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                        envLogPath = rootDir </> "fake-cc.env"
                    createDirectoryIfMissing True toolchainDir
                    writeExecutableScript compilerPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' SDKROOT \"$SDKROOT\" > " <> shellQuote envLogPath
                        , "printf '%s=%s\\n' PATH \"$PATH\" >> " <> shellQuote envLogPath
                        , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "SDKROOT=${SDKROOT:-$PWD/sdk} PATH=${PATH:+$PATH:}$SDKROOT/toolchain fake-cc"
                    (exitCode', stdoutOut, stderrOut) <-
                        readCompilerProcessWithExitCodeIn (Just rootDir) compiler []
                    loggedEnv <- lines <$> readFile envLogPath
                    assertEqual
                        "compiler subprocesses should expand dependent POSIX-style overrides and common braced parameter forms before spawning"
                        ExitSuccess
                        exitCode'
                    assertBool
                        "default-value parameter expansion should resolve SDKROOT before the child process starts"
                        (("SDKROOT=" <> (rootDir </> "sdk")) `elem` loggedEnv)
                    assertBool
                        "the expanded PATH should include the override-derived toolchain directory instead of the literal placeholders"
                        (any
                            (\loggedValue ->
                                "PATH=" `isPrefixOf` loggedValue
                                    && toolchainDir `isInfixOf` loggedValue
                            )
                            loggedEnv
                        )
                    assertBool
                        "the fake compiler should still emit assembly after env expansion succeeds"
                        (".globl main" `isInfixOf` stdoutOut)
                    assertEqual
                        "successful env expansion should not introduce stderr noise"
                        ""
                        stderrOut

readCompilerProcessWithExitCodePreservesInheritedPwdWithoutWorkingDirectoryTest :: Test
readCompilerProcessWithExitCodePreservesInheritedPwdWithoutWorkingDirectoryTest =
    TestLabel "TestRunner.read-compiler-process-with-exit-code-preserves-inherited-pwd-without-working-directory" $ TestCase $
        if os == "mingw32"
            then pure ()
            else do
                envPath <- maybe (assertFailure "missing env executable for inherited PWD test") pure
                    =<< findExecutable "env"
                withTempDirectory "htcc-test-inherited-pwd-env-." $ \logicalPwd -> do
                    withEnvVar "PWD" (Just logicalPwd) $ do
                        compiler <- resolveCompilerCommandIn Nothing $
                            "MSG=$PWD " <> shellQuote envPath
                        (exitCode', stdoutOut, stderrOut) <-
                            readCompilerProcessWithExitCodeIn Nothing compiler []
                        let loggedEnv = lines stdoutOut
                        assertEqual
                            "compiler subprocess helpers should keep caller-provided PWD when no working-directory override is requested"
                            ExitSuccess
                            exitCode'
                        assertBool
                            "POSIX-style env overrides should expand $PWD against the inherited logical PWD"
                            (("MSG=" <> logicalPwd) `elem` loggedEnv)
                        assertBool
                            "the spawned compiler environment should preserve the inherited logical PWD"
                            (("PWD=" <> logicalPwd) `elem` loggedEnv)
                        assertEqual
                            "preserving inherited PWD should not introduce stderr noise"
                            ""
                            stderrOut

readCompilerProcessWithExitCodeExpandsLeadingTildeInPosixEnvOverridesTest :: Test
readCompilerProcessWithExitCodeExpandsLeadingTildeInPosixEnvOverridesTest =
    TestLabel "TestRunner.read-compiler-process-with-exit-code-expands-leading-tilde-in-posix-env-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-expanded-tilde-env-overrides-." $ \homeDir -> do
                    let toolchainDir = homeDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                        envLogPath = homeDir </> "fake-cc.env"
                    createDirectoryIfMissing True toolchainDir
                    writeExecutableScript compilerPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' PATH \"$PATH\" > " <> shellQuote envLogPath
                        , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                        ]
                    withEnvVar "HOME" (Just homeDir) $ do
                        compiler <- resolveCompilerCommandIn Nothing
                            "PATH=~/toolchain:$PATH fake-cc"
                        (exitCode', stdoutOut, stderrOut) <-
                            readCompilerProcessWithExitCodeIn Nothing compiler []
                        loggedEnv <- lines <$> readFile envLogPath
                        assertEqual
                            "compiler subprocesses should expand leading ~ in POSIX-style env overrides before spawning"
                            ExitSuccess
                            exitCode'
                        assertBool
                            "the expanded PATH should include the HOME-relative toolchain directory instead of a literal ~ segment"
                            (any
                                (\loggedValue ->
                                    "PATH=" `isPrefixOf` loggedValue
                                        && toolchainDir `isInfixOf` loggedValue
                                        && not ("~/toolchain" `isInfixOf` loggedValue)
                                )
                                loggedEnv
                            )
                        assertBool
                            "the fake compiler should still emit assembly after tilde expansion succeeds"
                            (".globl main" `isInfixOf` stdoutOut)
                        assertEqual
                            "successful tilde expansion should not introduce stderr noise"
                            ""
                            stderrOut

resolveCompilerCommandExpandsPosixStyleVariablesInCompilerWordsTest :: Test
resolveCompilerCommandExpandsPosixStyleVariablesInCompilerWordsTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-posix-style-variables-in-compiler-words" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-expanded-compiler-words-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar "TOOLCHAIN" (Just toolchainDir) $ do
                        compiler <- resolveCompilerCommandIn (Just rootDir)
                            "$TOOLCHAIN/fake-cc --sysroot=${TOOLCHAIN:-/missing} -B${TOOLCHAIN:+$TOOLCHAIN/}lib"
                        assertEqual
                            "compiler command resolution should expand shell-style variables inside the executable word before PATH/local lookup"
                            compilerPath
                            (compilerExecutable compiler)
                        assertEqual
                            "compiler argv should expand dependent shell variables and common braced parameter forms before proc receives it"
                            [ "--sysroot=" <> toolchainDir
                            , "-B" <> toolchainDir <> "/lib"
                            ]
                            (compilerArguments compiler)

resolveCompilerCommandExpandsLeadingTildeInCompilerWordsTest :: Test
resolveCompilerCommandExpandsLeadingTildeInCompilerWordsTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-leading-tilde-in-compiler-words" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-expanded-tilde-compiler-words-." $ \homeDir -> do
                    let toolchainDir = homeDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar "HOME" (Just homeDir) $ do
                        compiler <- resolveCompilerCommandIn Nothing
                            "~/toolchain/fake-cc ~/sdk"
                        assertEqual
                            "compiler command resolution should expand leading ~ inside the executable word before PATH/local lookup"
                            compilerPath
                            (compilerExecutable compiler)
                        assertEqual
                            "compiler argv should expand leading ~ when later shell words begin with a HOME-relative path"
                            [homeDir </> "sdk"]
                            (compilerArguments compiler)

resolveCompilerCommandExpandsCompilerWordsAgainstInheritedEnvironmentTest :: Test
resolveCompilerCommandExpandsCompilerWordsAgainstInheritedEnvironmentTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-compiler-words-against-inherited-environment" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-inherited-compiler-env-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                        sdkRoot = rootDir </> "sdk"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar "TOOLCHAIN_DIR" (Just toolchainDir) $
                        withEnvVar "SDKROOT" (Just sdkRoot) $ do
                            compiler <- resolveCompilerCommandIn (Just rootDir)
                                "$TOOLCHAIN_DIR/fake-cc --sysroot=$SDKROOT"
                            assertEqual
                                "compiler word expansion should see inherited environment variables when resolving the executable path"
                                compilerPath
                                (compilerExecutable compiler)
                            assertEqual
                                "compiler argv expansion should see inherited environment variables before proc receives the arguments"
                                ["--sysroot=" <> sdkRoot]
                                (compilerArguments compiler)

resolveCompilerCommandExpandsCompilerWordsBeforeLeadingOverridesTest :: Test
resolveCompilerCommandExpandsCompilerWordsBeforeLeadingOverridesTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-compiler-words-before-leading-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-leading-assignment-compiler-env-." $ \rootDir -> do
                    let inheritedToolchainDir = rootDir </> "inherited-toolchain"
                        overriddenToolchainDir = rootDir </> "overridden-toolchain"
                        inheritedCompilerPath = inheritedToolchainDir </> "fake-cc"
                        overriddenCompilerPath = overriddenToolchainDir </> "fake-cc"
                        inheritedSdkRoot = rootDir </> "inherited-sdk"
                        overriddenSdkRoot = rootDir </> "overridden-sdk"
                    createDirectory inheritedToolchainDir
                    createDirectory overriddenToolchainDir
                    writeExecutableScript inheritedCompilerPath "#!/bin/sh\nexit 0\n"
                    writeExecutableScript overriddenCompilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar "TOOLCHAIN_DIR" (Just inheritedToolchainDir) $
                        withEnvVar "SDKROOT" (Just inheritedSdkRoot) $ do
                            compiler <- resolveCompilerCommandIn (Just rootDir) $
                                "TOOLCHAIN_DIR="
                                    <> overriddenToolchainDir
                                    <> " SDKROOT="
                                    <> overriddenSdkRoot
                                    <> " $TOOLCHAIN_DIR/fake-cc --sysroot=$SDKROOT"
                            assertEqual
                                "leading environment assignments should not change how later compiler words expand against the inherited environment"
                                inheritedCompilerPath
                                (compilerExecutable compiler)
                            assertEqual
                                "compiler argv expansion should still observe the inherited environment even when the command carries temporary overrides"
                                ["--sysroot=" <> inheritedSdkRoot]
                                (compilerArguments compiler)

resolveCompilerCommandWindowsExpandsCompilerWordsAfterLeadingOverridesTest :: Test
resolveCompilerCommandWindowsExpandsCompilerWordsAfterLeadingOverridesTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-expands-compiler-words-after-leading-overrides" $ TestCase $
        withTempDirectory "htcc-test-windows-leading-assignment-compiler-env-." $ \rootDir -> do
            let sdkRoot = rootDir </> "sdkroot"
                compilerDir = sdkRoot </> "bin"
                compilerPath = compilerDir </> "clang.exe"
            createDirectoryIfMissing True compilerDir
            writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
            compiler <- resolveCompilerCommandInForHost "mingw32" (Just rootDir) $
                "SDKROOT="
                    <> sdkRoot
                    <> " %SDKROOT%/bin/clang.exe --sysroot=%SDKROOT%"
            assertEqual
                "Windows compiler words should expand against temporary overrides before resolving the executable path"
                [("SDKROOT", sdkRoot)]
                (compilerEnvOverrides compiler)
            assertEqual
                "Windows compiler executable selection should see temporary overrides before launch resolution"
                compilerPath
                (compilerExecutable compiler)
            assertEqual
                "Windows compiler argv expansion should see temporary overrides before proc receives the arguments"
                ["--sysroot=" <> sdkRoot]
                (compilerArguments compiler)

resolveCompilerCommandRetokenizesUnquotedInheritedCompilerExpansionsTest :: Test
resolveCompilerCommandRetokenizesUnquotedInheritedCompilerExpansionsTest =
    TestLabel "TestRunner.resolve-compiler-command-retokenizes-unquoted-inherited-compiler-expansions" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-multiword-compiler-expansion-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        wrapperPath = toolchainDir </> "launcher"
                    createDirectory toolchainDir
                    writeExecutableScript wrapperPath "#!/bin/sh\nexit 0\n"
                    withEnvVar
                        "WRAPPED_CC"
                        (Just "./toolchain/launcher fake-cc --target=x86_64-elf")
                        $ do
                            compiler <- resolveCompilerCommandIn (Just rootDir)
                                "$WRAPPED_CC -c"
                            assertEqual
                                "unquoted inherited compiler expansions should retokenize so the first expanded word remains executable-resolvable"
                                wrapperPath
                                (compilerExecutable compiler)
                            assertEqual
                                "unquoted inherited compiler expansions should contribute additional argv words instead of remaining a single literal token"
                                ["fake-cc", "--target=x86_64-elf", "-c"]
                                (compilerArguments compiler)

resolveCompilerCommandWindowsRetokenizesExpandedCompilerWordsTest :: Test
resolveCompilerCommandWindowsRetokenizesExpandedCompilerWordsTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-retokenizes-expanded-compiler-words" $ TestCase $
        withTempDirectory "htcc-test-windows-multiword-compiler-expansion-." $ \rootDir -> do
            let toolchainDir = rootDir </> "toolchain"
                wrapperPath = toolchainDir </> "launcher"
            createDirectory toolchainDir
            writeExecutableScript wrapperPath "#!/bin/sh\nexit 0\n"
            withEnvVar
                "WRAPPED_CC"
                (Just $ wrapperPath <> " --target=x86_64-unknown-linux-gnu")
                $ do
                    compiler <- resolveCompilerCommandInForHost "mingw32" (Just rootDir)
                        "%WRAPPED_CC% -c"
                    assertEqual
                        "Windows expanded compiler words should retokenize so the executable remains resolvable"
                        wrapperPath
                        (compilerExecutable compiler)
                    assertEqual
                        "Windows expanded compiler words should contribute additional argv fields instead of remaining a single literal token"
                        ["--target=x86_64-unknown-linux-gnu", "-c"]
                        (compilerArguments compiler)

resolveCompilerCommandWindowsPreservesQuotedCompilerWordsWithSpacesTest :: Test
resolveCompilerCommandWindowsPreservesQuotedCompilerWordsWithSpacesTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-preserves-quoted-compiler-words-with-spaces" $ TestCase $
        withTempDirectory "htcc-test-windows-quoted-compiler-word-." $ \rootDir -> do
            let compilerDir = rootDir </> "Program Files" </> "LLVM" </> "bin"
                compilerPath = compilerDir </> "clang.exe"
                includeDir = rootDir </> "SDK Dir" </> "include"
            createDirectoryIfMissing True compilerDir
            createDirectoryIfMissing True includeDir
            writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
            compiler <- resolveCompilerCommandInForHost "mingw32" (Just rootDir) $
                "\"" <> compilerPath <> "\" -I \"" <> includeDir <> "\""
            assertEqual
                "Windows command parsing should keep a quoted executable path with spaces as a single compiler word"
                compilerPath
                (compilerExecutable compiler)
            assertEqual
                "Windows command parsing should keep quoted argv paths with spaces as single arguments"
                ["-I", includeDir]
                (compilerArguments compiler)

resolveCompilerCommandKeepsExpandedAssignmentsAsCompilerWordsTest :: Test
resolveCompilerCommandKeepsExpandedAssignmentsAsCompilerWordsTest =
    TestLabel "TestRunner.resolve-compiler-command-keeps-expanded-assignments-as-compiler-words" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-expanded-leading-assignment-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar
                        "WRAPPED_CC"
                        (Just "PATH=./toolchain fake-cc --target=x86_64-elf")
                        $ do
                            compiler <- resolveCompilerCommandIn (Just rootDir)
                                "$WRAPPED_CC -c"
                            assertEqual
                                "unquoted inherited compiler expansions should not reclassify assignment-like fields as leading environment overrides"
                                []
                                (compilerEnvOverrides compiler)
                            assertEqual
                                "assignment-like fields produced by compiler expansion should stay in argv[0] so shell-visible misconfigurations remain visible"
                                "PATH=./toolchain"
                                (compilerExecutable compiler)
                            assertEqual
                                "assignment-like fields produced by compiler expansion should remain in argv while preserving the remaining retokenized arguments"
                                ["fake-cc", "--target=x86_64-elf", "-c"]
                                (compilerArguments compiler)

resolveCompilerCommandKeepsQuotedExpandedAssignmentsAsCompilerWordsTest :: Test
resolveCompilerCommandKeepsQuotedExpandedAssignmentsAsCompilerWordsTest =
    TestLabel "TestRunner.resolve-compiler-command-keeps-quoted-expanded-assignments-as-compiler-words" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-quoted-expanded-leading-assignment-." $ \rootDir ->
                    withEnvVar
                        "WRAPPED_CC"
                        (Just "PATH=./toolchain fake-cc --target=x86_64-elf")
                        $ do
                            compiler <- resolveCompilerCommandIn (Just rootDir)
                                "\"$WRAPPED_CC\" -c"
                            assertEqual
                                "quoted inherited compiler expansions should not be reclassified as leading environment overrides"
                                []
                                (compilerEnvOverrides compiler)
                            assertEqual
                                "quoted inherited compiler expansions should remain a single compiler word instead of shifting -c into argv[0]"
                                "PATH=./toolchain fake-cc --target=x86_64-elf"
                                (compilerExecutable compiler)
                            assertEqual
                                "quoted inherited compiler expansions should preserve following argv entries"
                                ["-c"]
                                (compilerArguments compiler)

resolveCompilerCommandExpandsQuotedBracedDefaultWordAcrossShellSpansTest :: Test
resolveCompilerCommandExpandsQuotedBracedDefaultWordAcrossShellSpansTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-quoted-braced-default-word-across-shell-spans" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-quoted-braced-default-word-." $ \rootDir -> do
                    withEnvVar "WRAPPED_CC" Nothing $ do
                        compiler <- resolveCompilerCommandIn (Just rootDir)
                            "${WRAPPED_CC:-\"./toolchain/launcher --target=x86_64-elf\"} -c"
                        assertEqual
                            "quoted default words inside braced parameter expansion should expand instead of remaining literal even when they preserve embedded spaces"
                            "./toolchain/launcher --target=x86_64-elf"
                            (compilerExecutable compiler)
                        assertEqual
                            "quoted default words that preserve embedded spaces should still leave following argv entries intact"
                            ["-c"]
                            (compilerArguments compiler)

resolveCompilerCommandExpandsEscapedSpaceBracedAlternativeWordAcrossShellSpansTest :: Test
resolveCompilerCommandExpandsEscapedSpaceBracedAlternativeWordAcrossShellSpansTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-escaped-space-braced-alternative-word-across-shell-spans" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-escaped-braced-alternative-word-." $ \rootDir -> do
                    withEnvVar "WRAPPED_CC" (Just "1") $ do
                        compiler <- resolveCompilerCommandIn (Just rootDir)
                            "${WRAPPED_CC:+./toolchain/launcher\\ --target=x86_64-elf} -c"
                        assertEqual
                            "alternative words inside braced parameter expansion should expand instead of remaining literal when escaped spaces are present"
                            "./toolchain/launcher --target=x86_64-elf"
                            (compilerExecutable compiler)
                        assertEqual
                            "escaped spaces inside braced alternative words should still leave following argv entries intact"
                            ["-c"]
                            (compilerArguments compiler)

resolveCompilerCommandExpandsBracedDefaultWordWithAssignmentValueTildesTest :: Test
resolveCompilerCommandExpandsBracedDefaultWordWithAssignmentValueTildesTest =
    TestLabel "TestRunner.resolve-compiler-command-expands-braced-default-word-with-assignment-value-tildes" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-braced-default-assignment-tilde-." $ \homeDir -> do
                    let sbinDir = homeDir </> "sbin"
                        compilerPath = sbinDir </> "fake-cc"
                    createDirectory sbinDir
                    writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
                    withEnvVar "ALT_PATH" Nothing $
                        withEnvVar "HOME" (Just homeDir) $ do
                            compiler <- resolveCompilerCommandIn Nothing
                                "PATH=${ALT_PATH:-~/bin:~/sbin} fake-cc"
                            assertEqual
                                "default words inside braced parameter expansion should keep assignment-mode tilde expansion semantics for later PATH entries"
                                compilerPath
                                (compilerExecutable compiler)
                            assertEqual
                                "resolving through a later HOME-relative PATH entry should leave argv untouched"
                                []
                                (compilerArguments compiler)

resolveCompilerCommandPreservesUnquotedWindowsBackslashesOnWindowsTest :: Test
resolveCompilerCommandPreservesUnquotedWindowsBackslashesOnWindowsTest =
    TestLabel "TestRunner.resolve-compiler-command-preserves-unquoted-windows-backslashes-on-windows" $ TestCase $
        if os /= "mingw32"
            then pure ()
            else do
                compiler <- resolveCompilerCommand
                    "PATH=C:\\toolchain;%PATH% C:\\LLVM\\bin\\clang.exe -I C:\\SDK\\include"
                assertEqual
                    "unquoted Windows-style env overrides should preserve backslashes instead of treating them as POSIX escapes on Windows hosts"
                    [("PATH", "C:\\toolchain;%PATH%")]
                    (compilerEnvOverrides compiler)
                assertEqual
                    "unquoted Windows-style compiler paths should preserve backslashes in the selected executable on Windows hosts"
                    "C:\\LLVM\\bin\\clang.exe"
                    (compilerExecutable compiler)
                assertEqual
                    "unquoted Windows-style argv entries should preserve backslashes on Windows hosts"
                    ["-I", "C:\\SDK\\include"]
                    (compilerArguments compiler)

resolveCompilerCommandWindowsPreservesTrailingBackslashArgumentTest :: Test
resolveCompilerCommandWindowsPreservesTrailingBackslashArgumentTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-preserves-trailing-backslash-argument" $ TestCase $ do
        compiler <- resolveCompilerCommandInForHost "mingw32" Nothing
            "clang -I C:\\SDK\\include\\"
        assertEqual
            "Windows command parsing should preserve ordinary trailing backslashes instead of treating them as dangling escapes"
            "clang"
            (takeFileName $ compilerExecutable compiler)
        assertEqual
            "Windows argv parsing should keep trailing backslashes in unquoted path arguments"
            ["-I", "C:\\SDK\\include\\"]
            (compilerArguments compiler)

resolveCompilerCommandWindowsPreservesUnquotedUncExecutablePathTest :: Test
resolveCompilerCommandWindowsPreservesUnquotedUncExecutablePathTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-preserves-unquoted-unc-executable-path" $ TestCase $ do
        compiler <- resolveCompilerCommandInForHost "mingw32" Nothing
            "\\\\server\\toolchain\\clang.exe -c src\\main.c"
        assertEqual
            "Windows command parsing should preserve leading UNC backslashes when resolving the executable"
            "\\\\server\\toolchain\\clang.exe"
            (compilerExecutable compiler)
        assertEqual
            "Windows argv parsing should preserve backslashes in later arguments after a UNC executable path"
            ["-c", "src\\main.c"]
            (compilerArguments compiler)

resolveCompilerCommandWindowsTreatsApostrophesAsLiteralCharactersTest :: Test
resolveCompilerCommandWindowsTreatsApostrophesAsLiteralCharactersTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-treats-apostrophes-as-literal-characters" $ TestCase $ do
        compiler <- resolveCompilerCommandInForHost "mingw32" Nothing
            "C:\\Users\\O'Neil\\tool\\clang.exe -I C:\\SDK\\O'Neil\\include"
        assertEqual
            "Windows command parsing should keep apostrophes in compiler paths because cmd.exe does not use single quotes for quoting"
            "C:\\Users\\O'Neil\\tool\\clang.exe"
            (compilerExecutable compiler)
        assertEqual
            "Windows argv parsing should keep apostrophes in later arguments instead of stripping them as POSIX quotes"
            ["-I", "C:\\SDK\\O'Neil\\include"]
            (compilerArguments compiler)

resolveCompilerCommandWindowsUnescapesCaretEscapedSpacesTest :: Test
resolveCompilerCommandWindowsUnescapesCaretEscapedSpacesTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-unescapes-caret-escaped-spaces" $ TestCase $ do
        compiler <- resolveCompilerCommandInForHost "mingw32" Nothing
            "C:\\Program^ Files\\LLVM\\bin\\clang.exe -I C:\\Program^ Files\\SDK\\include"
        assertEqual
            "Windows command parsing should drop cmd.exe caret escapes before spaces in the executable path"
            "C:\\Program Files\\LLVM\\bin\\clang.exe"
            (compilerExecutable compiler)
        assertEqual
            "Windows argv parsing should drop cmd.exe caret escapes before spaces in later arguments"
            ["-I", "C:\\Program Files\\SDK\\include"]
            (compilerArguments compiler)

resolveCompilerCommandWindowsDecodesQuotedCmdExeEscapesTest :: Test
resolveCompilerCommandWindowsDecodesQuotedCmdExeEscapesTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-decodes-quoted-cmd-exe-escapes" $ TestCase $ do
        compiler <- resolveCompilerCommandInForHost "mingw32" Nothing
            "\"C:\\work\\100%% ready\\clang.exe\" \"-I\" \"C:\\SDK\\bang^!kit\\include\""
        assertEqual
            "Windows command parsing should decode cmd.exe percent escapes inside quoted compiler words before selecting the executable"
            "C:\\work\\100% ready\\clang.exe"
            (compilerExecutable compiler)
        assertEqual
            "Windows argv parsing should decode cmd.exe delayed-expansion escapes inside quoted arguments"
            ["-I", "C:\\SDK\\bang!kit\\include"]
            (compilerArguments compiler)

resolveCompilerCommandWindowsPreservesEscapedLiteralExpansionSyntaxTest :: Test
resolveCompilerCommandWindowsPreservesEscapedLiteralExpansionSyntaxTest =
    TestLabel "TestRunner.resolve-compiler-command-windows-preserves-escaped-literal-expansion-syntax" $ TestCase $
        withTempDirectory "htcc-test-windows-literal-expansion-syntax-." $ \rootDir -> do
            let compilerDir = rootDir </> "toolchain"
                compilerPath = compilerDir </> "clang.exe"
            createDirectoryIfMissing True compilerDir
            writeExecutableScript compilerPath "#!/bin/sh\nexit 0\n"
            withEnvVar "PATH" (Just "C:\\Windows\\System32") $
                withEnvVar "literal" (Just "EXPANDED") $ do
                    compiler <- resolveCompilerCommandInForHost "mingw32" Nothing $
                        "PATH="
                            <> compilerDir
                            <> ";^%PATH^% "
                            <> compilerPath
                            <> " ^!literal^!"
                    assertEqual
                        "Windows leading env overrides should preserve caret-escaped %...% literals instead of expanding them against the inherited environment"
                        [("PATH", compilerDir <> ";%PATH%")]
                        (compilerEnvOverrides compiler)
                    assertEqual
                        "Windows compiler resolution should still keep the explicitly addressed executable"
                        compilerPath
                        (compilerExecutable compiler)
                    assertEqual
                        "Windows argv parsing should preserve caret-escaped !...! literals instead of expanding them during command resolution"
                        ["!literal!"]
                        (compilerArguments compiler)

resolveCompilerCommandAppliesBarePosixBackslashEscapesTest :: Test
resolveCompilerCommandAppliesBarePosixBackslashEscapesTest =
    TestLabel "TestRunner.resolve-compiler-command-applies-bare-posix-backslash-escapes" $ TestCase $
        if os == "mingw32"
            then pure ()
            else do
                compiler <- resolveCompilerCommand
                    "SDKROOT=dir\\=name fake-cc -DNAME\\=1 wrap\\:arg list\\,value"
                assertEqual
                    "bare POSIX-style env overrides should drop backslashes before ordinary punctuation characters"
                    [("SDKROOT", "dir=name")]
                    (compilerEnvOverrides compiler)
                assertEqual
                    "bare POSIX-style argv parsing should drop backslashes before ordinary punctuation characters"
                    "fake-cc"
                    (compilerExecutable compiler)
                assertEqual
                    "bare POSIX-style argv parsing should keep the escaped characters while removing the escape marker itself"
                    ["-DNAME=1", "wrap:arg", "list,value"]
                    (compilerArguments compiler)

resolveCompilerCommandPreservesQuotedPosixLiteralPathOverrideTest :: Test
resolveCompilerCommandPreservesQuotedPosixLiteralPathOverrideTest =
    TestLabel "TestRunner.resolve-compiler-command-preserves-quoted-posix-literal-path-override" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-literal-path-override-." $ \rootDir -> do
                    let toolchainDir = rootDir </> "toolchain"
                        compilerPath = toolchainDir </> "fake-cc"
                    createDirectory toolchainDir
                    writeExecutableScript compilerPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "PATH='$PWD/toolchain' fake-cc"
                    assertEqual
                        "quoted leading PATH overrides should keep $PWD literal instead of retargeting driver resolution"
                        [("PATH", "$PWD/toolchain")]
                        (compilerEnvOverrides compiler)
                    assertEqual
                        "literal quoted PATH segments should not cause fake-cc to resolve from the quoted toolchain directory"
                        "fake-cc"
                        (compilerExecutable compiler)

readCompilerProcessWithExitCodePreservesQuotedAndEscapedPosixLiteralOverridesTest :: Test
readCompilerProcessWithExitCodePreservesQuotedAndEscapedPosixLiteralOverridesTest =
    TestLabel "TestRunner.read-compiler-process-with-exit-code-preserves-quoted-and-escaped-posix-literal-overrides" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-literal-env-overrides-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "env-wrapper.sh"
                        logPath = rootDir </> "env-wrapper.log"
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "printf '%s=%s\\n' MSG_QUOTED \"$MSG_QUOTED\" > " <> shellQuote logPath
                        , "printf '%s=%s\\n' MSG_ESCAPED \"$MSG_ESCAPED\" >> " <> shellQuote logPath
                        ]
                    compiler <- resolveCompilerCommandIn (Just rootDir)
                        "MSG_QUOTED='$PWD' MSG_ESCAPED=\\$PWD ./env-wrapper.sh"
                    (exitCode', stdoutOut, stderrOut) <-
                        readCompilerProcessWithExitCodeIn (Just rootDir) compiler []
                    loggedEnv <- lines <$> readFile logPath
                    assertEqual
                        "quoted and escaped leading env overrides should remain literal in the spawned compiler environment"
                        ExitSuccess
                        exitCode'
                    assertEqual
                        "quoted and escaped POSIX literals should stay as $PWD when passed to the child process"
                        [ "MSG_QUOTED=$PWD"
                        , "MSG_ESCAPED=$PWD"
                        ]
                        loggedEnv
                    assertEqual
                        "env logging wrappers should not produce stdout noise"
                        ""
                        stdoutOut
                    assertEqual
                        "env logging wrappers should not produce stderr noise"
                        ""
                        stderrOut

resolveCompilerCommandFallsBackToWorkingDirectoryForBareLocalCommandTest :: Test
resolveCompilerCommandFallsBackToWorkingDirectoryForBareLocalCommandTest =
    TestLabel "TestRunner.resolve-compiler-command-falls-back-to-working-directory-for-bare-local-command" $ TestCase $
        withTempDirectory "htcc-test-local-bare-command-." $ \rootDir -> do
            let wrapperName = "htcc-test-local-only-wrapper-not-on-path"
            writeExecutableScript (rootDir </> wrapperName) "#!/bin/sh\nexit 0\n"
            compiler <- resolveCompilerCommandIn (Just rootDir) $ wrapperName <> " --probe"
            assertEqual
                "bare commands should still fall back to an executable in the working directory when PATH lookup fails"
                (rootDir </> wrapperName)
                (compilerExecutable compiler)
            assertEqual
                "working-directory fallback should preserve the remaining argv"
                ["--probe"]
                (compilerArguments compiler)

assemblerCompilerCommandFallsBackToBareLocalHtccAssemblerWrapperTest :: Test
assemblerCompilerCommandFallsBackToBareLocalHtccAssemblerWrapperTest =
    TestLabel "TestRunner.assembler-compiler-command-falls-back-to-bare-local-htcc-assembler-wrapper" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-local-htcc-assembler-wrapper-." $ \rootDir -> do
                    let wrapperName = "tmp-assembler.sh"
                    writeExecutableScript (rootDir </> wrapperName) "#!/bin/sh\nexit 0\n"
                    withCurrentDirectory rootDir $
                        withEnvVar "HTCC_ASSEMBLER" (Just wrapperName) $ do
                            compiler <- assemblerCompilerCommand
                            assertEqual
                                "HTCC_ASSEMBLER should still resolve bare local wrappers from the current working directory when they are not on PATH"
                                ("./" <> wrapperName)
                                (compilerExecutable compiler)
                            assertEqual
                                "bare local HTCC_ASSEMBLER wrappers should not gain unexpected argv"
                                []
                                (compilerArguments compiler)

compilerCommandAvailableRejectsRelativeWrapperMissingFromTemporaryDirectoryTest :: Test
compilerCommandAvailableRejectsRelativeWrapperMissingFromTemporaryDirectoryTest =
    TestLabel "TestRunner.compiler-command-available-rejects-relative-wrapper-missing-from-temporary-directory" $ TestCase $
        if os == "mingw32"
            then pure ()
            else
                withTempDirectory "htcc-test-relative-htcc-wrapper-." $ \rootDir -> do
                    let wrapperPath = rootDir </> "stack-wrapper"
                        logPath = rootDir </> "stack-wrapper.log"
                    probeDir <- getTemporaryDirectory
                    writeExecutableScript wrapperPath $ unlines
                        [ "#!/bin/sh"
                        , "pwd > " <> shellQuote logPath
                        , "last_arg=''"
                        , "for arg in \"$@\"; do"
                        , "  last_arg=\"$arg\""
                        , "done"
                        , "[ -n \"$last_arg\" ]"
                        , "[ -f \"$last_arg\" ]"
                        , "printf '%s\\n' '.globl main' 'main:' '  ret'"
                        ]
                    available <- compilerCommandAvailableWithDirectories
                        [ pure probeDir
                        , pure rootDir
                        ]
                        (pure $ T.pack "./stack-wrapper exec htcc --")
                    loggedDir <- catchIOError (readFile logPath) (const $ pure "")
                    assertEqual
                        "the htcc probe should reject wrappers that only work from the runner cwd because subprocess tests also execute from temp directories"
                        False
                        available
                    assertBool
                        "the probe should still continue on to the runner cwd after the temp-dir check fails"
                        (rootDir `isInfixOf` loggedDir)

compilerCommandAvailableRejectsZeroExitPlaceholderWrapperTest :: Test
compilerCommandAvailableRejectsZeroExitPlaceholderWrapperTest =
    TestLabel "TestRunner.compiler-command-available-rejects-zero-exit-placeholder-wrapper" $ TestCase $
        withTempDirectory "htcc-test-placeholder-bin-." $ \binDir -> do
            let stackPath = binDir <> "/stack"
            writeExecutableScript stackPath $ unlines
                [ "#!/bin/sh"
                , "exit 0"
                ]
            available <- compilerCommandAvailableWith $
                pure . T.pack $ shellQuote stackPath <> " exec htcc --"
            assertEqual
                "zero-exit wrappers that cannot compile should not make the subprocess runner look available"
                False
                available

compilerCommandAvailableHandlesResolutionFailureTest :: Test
compilerCommandAvailableHandlesResolutionFailureTest =
    TestLabel "TestRunner.compiler-command-available-handles-resolution-failure" $ TestCase $ do
        available <- compilerCommandAvailableWith $ ioError $ userError "stack not found"
        assertEqual
            "compiler command probing should report False instead of aborting when command resolution throws"
            False
            available

findRepoBuiltExecutableNearPrefersMatchingOptimizedBuildArtifactTest :: Test
findRepoBuiltExecutableNearPrefersMatchingOptimizedBuildArtifactTest =
    TestLabel "TestRunner.find-repo-built-executable-near-prefers-matching-optimized-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-cabal-layout-." $ \rootDir -> do
            let testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                expectedCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
                staleGenericCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/build/htcc/htcc"
                staleNooptCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/noopt/build/htcc/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            ensureExecutablePlaceholder staleGenericCompilerExecutable
            ensureExecutablePlaceholder staleNooptCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should prefer the exact repo-built htcc from the matching optimized test build tree before stale generic build/htcc fallbacks"
                (Just expectedCompilerExecutable)
                resolved

findRepoBuiltExecutableNearPrefersMatchingNooptBuildArtifactTest :: Test
findRepoBuiltExecutableNearPrefersMatchingNooptBuildArtifactTest =
    TestLabel "TestRunner.find-repo-built-executable-near-prefers-matching-noopt-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-cabal-noopt-layout-." $ \rootDir -> do
            let testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/noopt/build/htcc-test/htcc-test"
                expectedCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/noopt/build/htcc/htcc"
                staleGenericCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/noopt/build/htcc/htcc"
                staleOptimizedCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            ensureExecutablePlaceholder staleGenericCompilerExecutable
            ensureExecutablePlaceholder staleOptimizedCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should prefer the exact repo-built htcc from the matching noopt test build tree before stale generic noopt/build fallbacks"
                (Just expectedCompilerExecutable)
                resolved

findRepoBuiltExecutableNearIgnoresCheckoutPathNooptDirectoriesTest :: Test
findRepoBuiltExecutableNearIgnoresCheckoutPathNooptDirectoriesTest =
    TestLabel "TestRunner.find-repo-built-executable-near-ignores-checkout-path-noopt-directories" $ TestCase $
        withTempDirectory "htcc-test-checkout-noopt-path-." $ \sandboxDir -> do
            let rootDir = sandboxDir <> "/x/htcc/noopt/build/htcc/repo"
                testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                expectedCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
                staleNooptCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/noopt/build/htcc/htcc"
            createDirectoryIfMissing True rootDir
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            ensureExecutablePlaceholder staleNooptCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "a checkout path containing /noopt/ should not make optimized test binaries prefer noopt compiler artifacts"
                (Just expectedCompilerExecutable)
                resolved

findRepoBuiltExecutableNearPrefersBuildArtifactOverRepoRootExecutableTest :: Test
findRepoBuiltExecutableNearPrefersBuildArtifactOverRepoRootExecutableTest =
    TestLabel "TestRunner.find-repo-built-executable-near-prefers-build-artifact-over-repo-root-executable" $ TestCase $
        withTempDirectory "htcc-test-repo-root-wrapper-layout-." $ \rootDir -> do
            let testExecutable = rootDir <> "/out/htcc-test"
                expectedCompilerExecutable = rootDir <> "/build/htcc"
                staleRepoRootCompilerExecutable = rootDir <> "/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            ensureExecutablePlaceholder staleRepoRootCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should try build artifacts before generic repo-root executables"
                (Just expectedCompilerExecutable)
                resolved

findRepoBuiltExecutableNearSupportsStackBuildLayoutTest :: Test
findRepoBuiltExecutableNearSupportsStackBuildLayoutTest =
    TestLabel "TestRunner.find-repo-built-executable-near-supports-stack-build-layout" $ TestCase $
        withTempDirectory "htcc-test-stack-layout-." $ \rootDir -> do
            let testExecutable =
                    rootDir
                        <> "/.stack-work/dist/x86_64-linux/Cabal-3.10.3.0/build/htcc-test/htcc-test"
                expectedCompilerExecutable =
                    rootDir
                        <> "/.stack-work/dist/x86_64-linux/Cabal-3.10.3.0/build/htcc/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should also find the repo-built htcc in stack-style build trees"
                (Just expectedCompilerExecutable)
                resolved

findRepoBuiltExecutableNearSupportsOutOfTreeCabalBuildArtifactTest :: Test
findRepoBuiltExecutableNearSupportsOutOfTreeCabalBuildArtifactTest =
    TestLabel "TestRunner.find-repo-built-executable-near-supports-out-of-tree-cabal-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-out-of-tree-build-layout-." $ \sandboxDir -> do
            let buildDir = sandboxDir <> "/cabal-build"
                testExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                expectedCompilerExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder expectedCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should still reuse the sibling repo-built htcc when Cabal relocates the test executable outside the checkout"
                (Just expectedCompilerExecutable)
                resolved

currentCheckoutRootFallsBackToCurrentDirectoryTest :: Test
currentCheckoutRootFallsBackToCurrentDirectoryTest =
    TestLabel "TestRunner.current-checkout-root-falls-back-to-current-directory" $ TestCase $
        withTempDirectory "htcc-test-current-checkout-root-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/repo"
                outOfTreeExecutableDir =
                    sandboxDir
                        <> "/cabal-build/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test"
                invocationDir = repoRoot <> "/test-suite"
            createDirectoryIfMissing True invocationDir
            createDirectoryIfMissing True outOfTreeExecutableDir
            ensureRepoRootMarker repoRoot
            resolved <- currentCheckoutRootFromDirectories [outOfTreeExecutableDir, invocationDir]
            assertEqual
                "the checkout-root probe should fall back to the current directory when the test executable lives outside the repo"
                (Just repoRoot)
                resolved

resolveCheckoutRootAndCompilerFromDirectoriesKeepsSameCheckoutOutOfTreeBuildArtifactTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesKeepsSameCheckoutOutOfTreeBuildArtifactTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-keeps-same-checkout-out-of-tree-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-same-checkout-out-of-tree-build-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/repo"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                buildDir = sandboxDir <> "/cabal-build"
                testExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                testSetupConfig =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                repoBuiltCompilerExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder repoBuiltCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory testSetupConfig
            writeFile testSetupConfig (repoRoot <> "/.\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime repoBuiltCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, repoRoot]
            assertEqual
                "the checkout should still resolve from the current repo when the test binary comes from this checkout's out-of-tree builddir"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "same-checkout out-of-tree build artifacts should remain eligible for HTCC_BIN auto-selection"
                (Just repoBuiltCompilerExecutable)
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromOutOfTreeBuildMetadataTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromOutOfTreeBuildMetadataTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-recovers-checkout-from-out-of-tree-build-metadata" $ TestCase $
        withTempDirectory "htcc-test-build-metadata-out-of-tree-checkout-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/repo"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                buildDir = sandboxDir <> "/cabal-build"
                invocationDir = sandboxDir <> "/outside/runner"
                testExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                testSetupConfig =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                repoBuiltCompilerExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            createDirectoryIfMissing True invocationDir
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder repoBuiltCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory testSetupConfig
            writeFile testSetupConfig (repoRoot <> "/.\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime repoBuiltCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, invocationDir]
            assertEqual
                "the checkout should still resolve from setup-config metadata when the runner cwd is outside the repo"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "out-of-tree build metadata should keep the sibling build-tool-depends htcc eligible even when fallback directories miss the repo"
                (Just repoBuiltCompilerExecutable)
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromSpacedOutOfTreeBuildMetadataTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromSpacedOutOfTreeBuildMetadataTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-recovers-checkout-from-spaced-out-of-tree-build-metadata" $ TestCase $
        withTempDirectory "htcc-test-build-metadata-spaced-out-of-tree-checkout-." $ \sandboxDir -> do
            let repoRoot = sandboxDir </> "repo with spaces"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                buildDir = sandboxDir <> "/cabal-build"
                invocationDir = sandboxDir <> "/outside/runner"
                testExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                testSetupConfig =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                repoBuiltCompilerExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            createDirectoryIfMissing True invocationDir
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder repoBuiltCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory testSetupConfig
            writeFile testSetupConfig ("\"" <> repoRoot <> "/.\"\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime repoBuiltCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, invocationDir]
            assertEqual
                "the checkout should still resolve from setup-config metadata when the recovered checkout path contains spaces"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "space-containing setup-config checkout paths should keep the sibling build-tool-depends htcc eligible for auto-selection"
                (Just repoBuiltCompilerExecutable)
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesUsesRepoRootFallbackForOutOfTreeBuildTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesUsesRepoRootFallbackForOutOfTreeBuildTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-uses-repo-root-fallback-for-out-of-tree-build" $ TestCase $
        withTempDirectory "htcc-test-out-of-tree-build-repo-root-fallback-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/repo"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                buildDir = sandboxDir <> "/cabal-build"
                testExecutable =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                testSetupConfig =
                    buildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                repoRootFallbackCompilerExecutable =
                    repoRoot <> "/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder repoRootFallbackCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory testSetupConfig
            writeFile testSetupConfig (repoRoot <> "/.\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime repoRootFallbackCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, repoRoot]
            assertEqual
                "the checkout should still resolve from the current repo when the matching compiler fallback lives at the repo root"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "repo-root fallback compiler binaries should be probed from the checkout root even when the test executable lives in an out-of-tree builddir"
                (Just repoRootFallbackCompilerExecutable)
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesIgnoresForeignOutOfTreeBuildArtifactTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesIgnoresForeignOutOfTreeBuildArtifactTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-ignores-foreign-out-of-tree-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-foreign-out-of-tree-build-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/repo"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                foreignBuildDir = sandboxDir <> "/foreign-build"
                foreignRepoRoot = sandboxDir <> "/foreign-repo"
                testExecutable =
                    foreignBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                foreignSetupConfig =
                    foreignBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                foreignCompilerExecutable =
                    foreignBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder foreignCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory foreignSetupConfig
            writeFile foreignSetupConfig (foreignRepoRoot <> "/.\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime foreignCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, repoRoot]
            assertEqual
                "the auto-selected checkout should still come from the current repo when the test binary lives in an unrelated out-of-tree build directory"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "foreign out-of-tree build artifacts should not be auto-selected as HTCC_BIN for self/subp"
                Nothing
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesIgnoresSiblingPrefixOutOfTreeBuildArtifactTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesIgnoresSiblingPrefixOutOfTreeBuildArtifactTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-ignores-sibling-prefix-out-of-tree-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-sibling-prefix-out-of-tree-build-." $ \sandboxDir -> do
            let repoRoot = sandboxDir <> "/htcc"
                sourcePath = repoRoot <> "/src/Htcc/Selection.hs"
                siblingRepoRoot = sandboxDir <> "/htcc-old"
                siblingBuildDir = sandboxDir <> "/sibling-build"
                testExecutable =
                    siblingBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                siblingSetupConfig =
                    siblingBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/setup-config"
                siblingCompilerExecutable =
                    siblingBuildDir
                        <> "/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
            ensureRepoRootMarker repoRoot
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder siblingCompilerExecutable
            createDirectoryIfMissing True $ takeDirectory siblingSetupConfig
            writeFile siblingSetupConfig (siblingRepoRoot <> "/.\n")
            now <- getCurrentTime
            setModificationTime sourcePath $ addUTCTime (-240) now
            setModificationTime siblingCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable, repoRoot]
            assertEqual
                "the checkout should still resolve from the current repo when an out-of-tree builddir belongs to a similarly named sibling checkout"
                (Just repoRoot)
                resolvedRepoRoot
            assertEqual
                "similarly named sibling build artifacts must not be auto-selected as HTCC_BIN for self/subp"
                Nothing
                resolvedCompiler

resolveCheckoutRootAndCompilerFromDirectoriesSkipsStaleEarlierBuildArtifactTest :: Test
resolveCheckoutRootAndCompilerFromDirectoriesSkipsStaleEarlierBuildArtifactTest =
    TestLabel "TestRunner.resolve-checkout-root-and-compiler-from-directories-skips-stale-earlier-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-stale-earlier-build-artifact-." $ \rootDir -> do
            let sourcePath = rootDir <> "/src/Htcc/Selection.hs"
                testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                staleCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/x/htcc/build/htcc/htcc"
                freshFallbackCompilerExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/build/htcc/htcc"
            ensureRepoRootMarker rootDir
            createDirectoryIfMissing True $ takeDirectory sourcePath
            writeFile sourcePath "module Htcc.Selection where\n"
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder staleCompilerExecutable
            ensureExecutablePlaceholder freshFallbackCompilerExecutable
            now <- getCurrentTime
            setModificationTime staleCompilerExecutable $ addUTCTime (-120) now
            setModificationTime sourcePath $ addUTCTime 0 now
            setModificationTime freshFallbackCompilerExecutable $ addUTCTime 120 now
            (resolvedRepoRoot, resolvedCompiler) <-
                resolveCheckoutRootAndCompilerFromDirectories
                    testExecutable
                    [takeDirectory testExecutable]
            assertEqual
                "when the test binary is rooted in this checkout, the checkout should still resolve from the executable path"
                (Just rootDir)
                resolvedRepoRoot
            assertEqual
                "a stale earlier repo-built htcc candidate should not hide a later fresh fallback candidate"
                (Just freshFallbackCompilerExecutable)
                resolvedCompiler

findRepoBuiltExecutableNearStopsAtCheckoutRootTest :: Test
findRepoBuiltExecutableNearStopsAtCheckoutRootTest =
    TestLabel "TestRunner.find-repo-built-executable-near-stops-at-checkout-root" $ TestCase $
        withTempDirectory "htcc-test-root-boundary-." $ \sandboxDir -> do
            let parentDir = sandboxDir <> "/parent"
                rootDir = parentDir <> "/repo"
                testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                parentCompilerExecutable =
                    parentDir <> "/htcc"
            createDirectoryIfMissing True rootDir
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder parentCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "the resolver should not escape the checkout root and reuse an unrelated parent-directory htcc"
                Nothing
                resolved

findRepoBuiltExecutableNearIgnoresAncestorWorkspaceMarkersTest :: Test
findRepoBuiltExecutableNearIgnoresAncestorWorkspaceMarkersTest =
    TestLabel "TestRunner.find-repo-built-executable-near-ignores-ancestor-workspace-markers" $ TestCase $
        withTempDirectory "htcc-test-ancestor-workspace-." $ \sandboxDir -> do
            let parentDir = sandboxDir <> "/workspace"
                rootDir = parentDir <> "/nested/htcc"
                testExecutable =
                    rootDir
                        <> "/dist-newstyle/build/x86_64-linux/ghc-9.6.6/htcc-0.0.0.1/t/htcc-test/build/htcc-test/htcc-test"
                parentCompilerExecutable =
                    parentDir <> "/build/htcc"
            createDirectoryIfMissing True rootDir
            writeFile (parentDir <> "/stack.yaml") "resolver: lts-22.0\n"
            writeFile (parentDir <> "/package.yaml") "name: workspace\nversion: 0.0.0.1\n"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureExecutablePlaceholder parentCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "ancestor workspaces with generic stack/package markers should not shadow the nested htcc checkout"
                Nothing
                resolved

findRepoBuiltExecutableNearSkipsBrokenBuildArtifactTest :: Test
findRepoBuiltExecutableNearSkipsBrokenBuildArtifactTest =
    TestLabel "TestRunner.find-repo-built-executable-near-skips-broken-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-broken-build-artifact-." $ \rootDir -> do
            let testExecutable = rootDir <> "/out/htcc-test"
                brokenCompilerExecutable = rootDir <> "/build/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            createDirectoryIfMissing True $ takeDirectory brokenCompilerExecutable
            writeExecutableScript brokenCompilerExecutable "#!/bin/sh\nexit 1\n"
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "non-runnable repo-built htcc artifacts should be ignored so the caller can fall back to stack exec"
                Nothing
                resolved

findRepoBuiltExecutableNearSkipsZeroExitPlaceholderBuildArtifactTest :: Test
findRepoBuiltExecutableNearSkipsZeroExitPlaceholderBuildArtifactTest =
    TestLabel "TestRunner.find-repo-built-executable-near-skips-zero-exit-placeholder-build-artifact" $ TestCase $
        withTempDirectory "htcc-test-zero-exit-placeholder-build-artifact-." $ \rootDir -> do
            let testExecutable = rootDir <> "/out/htcc-test"
                placeholderCompilerExecutable = rootDir <> "/build/htcc"
                fallbackCompilerExecutable = rootDir <> "/htcc/htcc"
            ensureRepoRootMarker rootDir
            ensureExecutablePlaceholder testExecutable
            ensureZeroExitPlaceholder placeholderCompilerExecutable
            ensureExecutablePlaceholder fallbackCompilerExecutable
            resolved <- findRepoBuiltExecutableNear testExecutable "htcc"
            assertEqual
                "zero-exit placeholder build artifacts should not shadow a later repo-built htcc candidate that can actually compile"
                (Just fallbackCompilerExecutable)
                resolved

looksRunnableLinkedOutputAcceptsPieExecutablesTest :: Test
looksRunnableLinkedOutputAcceptsPieExecutablesTest =
    TestLabel "TestRunner.looks-runnable-linked-output-accepts-pie-executables" $ TestCase $
        assertEqual
            "the ELF validator should accept ET_DYN static-PIE outputs when their loadable segments are runnable"
            True
            (looksRunnableLinkedOutput $ linkedElfHeader 3)

looksRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest :: Test
looksRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-interpreterless-shared-objects" $ TestCase $
        assertEqual
            "the ELF validator should reject ET_DYN shared-library outputs that do not carry a PT_INTERP loader or static-PIE flags"
            False
            (looksRunnableLinkedOutput linkedSharedObjectHeader)

looksRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest :: Test
looksRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-pie-marked-dependent-shared-objects" $ TestCase $
        assertEqual
            "the ELF validator should reject ET_DYN outputs that advertise DF_1_PIE but still carry DT_NEEDED entries without PT_INTERP"
            False
            (looksRunnableLinkedOutput $
                linkedElfHeaderWithInterpreterAndDynamicEntries
                    3
                    []
                    [ (elfDynamicTagFlags1, elfDynamicFlag1Pie)
                    , (elfDynamicTagNeeded, 1)
                    , (elfDynamicTagNull, 0)
                    ]
            )

looksRunnableLinkedOutputRejectsRelocatableObjectsTest :: Test
looksRunnableLinkedOutputRejectsRelocatableObjectsTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-relocatable-objects" $ TestCase $
        assertEqual
            "the assembler probe should still reject relocatable ELF objects"
            False
            (looksRunnableLinkedOutput $ linkedElfHeader 1)

looksRunnableLinkedOutputRejectsMalformedExecutablesTest :: Test
looksRunnableLinkedOutputRejectsMalformedExecutablesTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-malformed-executables" $ TestCase $
        assertEqual
            "the assembler probe should reject ET_EXEC blobs that do not contain a loadable executable segment"
            False
            (looksRunnableLinkedOutput malformedLinkedElfBlob)

looksRunnableLinkedOutputAcceptsLoadableExecutablesTest :: Test
looksRunnableLinkedOutputAcceptsLoadableExecutablesTest =
    TestLabel "TestRunner.looks-runnable-linked-output-accepts-loadable-executables" $ TestCase $
        assertEqual
            "the ELF validator should still accept well-formed loadable x86_64 ET_EXEC outputs"
            True
            (looksRunnableLinkedOutput $ linkedElfHeader 2)

looksRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest :: Test
looksRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest =
    TestLabel "TestRunner.looks-runnable-linked-output-accepts-foreign-osabi-dynamic-executables" $ TestCase $
        assertEqual
            "the ELF validator should accept structurally valid PT_INTERP executables regardless of the advertised ELF OS ABI"
            True
            (looksRunnableLinkedOutput $
                setElfOsAbi foreignElfOsAbiForHost $
                    linkedElfHeaderWithInterpreter 2 "/lib64/ld-linux-x86-64.so.2"
            )

looksRunnableLinkedOutputAcceptsForeignOsAbiTest :: Test
looksRunnableLinkedOutputAcceptsForeignOsAbiTest =
    TestLabel "TestRunner.looks-runnable-linked-output-accepts-foreign-osabi" $ TestCase $
        assertEqual
            "the ELF validator should accept well-formed x86_64 ELF executables even when they advertise a non-Linux OS ABI"
            True
            (looksRunnableLinkedOutput $ setElfOsAbi 9 $ linkedElfHeader 2)

looksRunnableLinkedOutputRejectsMalformedInterpreterTest :: Test
looksRunnableLinkedOutputRejectsMalformedInterpreterTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-malformed-interpreter" $ TestCase $
        assertEqual
            "the ELF validator should reject PT_INTERP segments that are not null-terminated absolute paths"
            False
            (looksRunnableLinkedOutput $
                linkedElfHeaderWithInterpreterBytes 2 $
                    map (fromIntegral . fromEnum) "/no/such/ld-linux-x86-64.so.2"
            )

looksRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest :: Test
looksRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest =
    TestLabel "TestRunner.looks-runnable-linked-output-rejects-dynamic-executables-without-interpreter" $ TestCase $
        assertEqual
            "the ELF validator should reject ET_EXEC outputs that still carry PT_DYNAMIC but lost PT_INTERP"
            False
            (looksRunnableLinkedOutput $
                linkedElfHeaderWithInterpreterAndDynamicEntries 2 [] [(elfDynamicTagNull, 0)]
            )

validateRunnableLinkedOutputAcceptsLoadableExecutablesWithMarkerTest :: Test
validateRunnableLinkedOutputAcceptsLoadableExecutablesWithMarkerTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-loadable-executables-with-marker" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:test"
            executableBytes = linkedElfHeader 2 <> asciiBytes probeMarker
        withTempExecutableBytes "htcc-test-runnable-probe-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
            assertEqual
                "probe validation should accept structurally valid loadable ELF executables when the marker is present"
                True
                validated

shouldValidateRunnableLinkedOutputRejectsSpecialOutputsTest :: Test
shouldValidateRunnableLinkedOutputRejectsSpecialOutputsTest =
    TestLabel "TestRunner.should-validate-runnable-linked-output-rejects-special-outputs" $ TestCase $
        if os == "mingw32"
            then pure ()
            else do
                shouldValidate <- shouldValidateRunnableLinkedOutput "/dev/null"
                assertEqual
                    "post-link ELF validation should be skipped for special output sinks such as /dev/null"
                    False
                    shouldValidate

shouldValidateRunnableLinkedOutputAcceptsFreshRegularTargetsTest :: Test
shouldValidateRunnableLinkedOutputAcceptsFreshRegularTargetsTest =
    TestLabel "TestRunner.should-validate-runnable-linked-output-accepts-fresh-regular-targets" $ TestCase $
        withTempDirectory "htcc-test-runnable-fresh-target-." $ \tmpDir -> do
            let missingOutputPath = tmpDir </> "fresh-output"
            shouldValidate <- shouldValidateRunnableLinkedOutput missingOutputPath
            assertEqual
                "post-link ELF validation should still run for fresh regular output paths that do not exist yet"
                True
                shouldValidate

validateRunnableLinkedOutputRejectsSymlinkedLoadableExecutablesWithMarkerTest :: Test
validateRunnableLinkedOutputRejectsSymlinkedLoadableExecutablesWithMarkerTest =
    TestLabel "TestRunner.validate-runnable-linked-output-rejects-symlinked-loadable-executables-with-marker" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:symlink"
            executableBytes = linkedElfHeader 2 <> asciiBytes probeMarker
        withTempDirectory "htcc-test-runnable-symlink-probe-." $ \tmpDir -> do
            let targetPath = tmpDir </> "linked-target"
                symlinkPath = tmpDir </> "linked-output"
            B.writeFile targetPath executableBytes
            setFileMode targetPath ownerModes
            createSymbolicLink targetPath symlinkPath
            validated <- validateRunnableLinkedOutput symlinkPath (Just probeMarker)
            assertEqual
                "probe validation should reject symlinked outputs so stale cached executables cannot pass -r validation"
                False
                validated

validateRunnableLinkedOutputRejectsNonElfOutputsEvenWithMarkerTest :: Test
validateRunnableLinkedOutputRejectsNonElfOutputsEvenWithMarkerTest =
    TestLabel "TestRunner.validate-runnable-linked-output-rejects-non-elf-outputs-even-with-marker" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:script"
            scriptContents = unlines
                [ "#!/bin/sh"
                , "exit 0"
                , "# " <> probeMarker
                ]
        withTempExecutableScript "htcc-test-runnable-missing-interpreter-." scriptContents $ \scriptPath -> do
            validated <- validateRunnableLinkedOutput scriptPath (Just probeMarker)
            assertEqual
                "probe validation should reject executable outputs that are not ELF binaries even when the marker is present"
                False
                validated

validateRunnableLinkedOutputAcceptsLoadableExecutablesWithoutRunningThemTest :: Test
validateRunnableLinkedOutputAcceptsLoadableExecutablesWithoutRunningThemTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-loadable-executables-without-running-them" $ TestCase $ do
        let executableBytes = linkedElfHeader 2
        withTempExecutableBytes "htcc-test-runnable-nonzero-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should accept structurally valid executable ELF outputs without running them"
                True
                validated

validateRunnableLinkedOutputAcceptsPieExecutablesWithoutRunningThemTest :: Test
validateRunnableLinkedOutputAcceptsPieExecutablesWithoutRunningThemTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-pie-executables-without-running-them" $ TestCase $ do
        let executableBytes = linkedElfHeader 3
        withTempExecutableBytes "htcc-test-runnable-static-pie-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should accept structurally valid ET_DYN static-PIE outputs without running them"
                True
                validated

validateRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest :: Test
validateRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-foreign-osabi-dynamic-executables" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:foreign-osabi"
            executableBytes =
                setElfOsAbi foreignElfOsAbiForHost $
                    linkedElfHeaderWithInterpreter 2 "/lib64/ld-linux-x86-64.so.2"
                        <> asciiBytes probeMarker
        withTempExecutableBytes "htcc-test-runnable-foreign-osabi-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
            assertEqual
                "final-output validation should accept structurally valid PT_INTERP executables even when they advertise a foreign ELF OS ABI"
                True
                validated

validateRunnableLinkedOutputRejectsForeignOsAbiStaticExecutablesTest :: Test
validateRunnableLinkedOutputRejectsForeignOsAbiStaticExecutablesTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-foreign-osabi-static-executables" $ TestCase $ do
        let executableBytes =
                setElfOsAbi foreignElfOsAbiForHost $
                    linkedElfHeader 2
        withTempExecutableBytes "htcc-test-runnable-foreign-osabi-static-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should accept structurally valid static ET_EXEC outputs regardless of ELF OS ABI"
                True
                validated

validateRunnableLinkedOutputRejectsSystemVDynamicExecutablesOnNonElfHostsTest :: Test
validateRunnableLinkedOutputRejectsSystemVDynamicExecutablesOnNonElfHostsTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-systemv-dynamic-executables-on-non-elf-hosts" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:systemv-non-elf-host"
            executableBytes =
                linkedElfHeaderWithInterpreter 2 "/lib64/ld-linux-x86-64.so.2"
                    <> asciiBytes probeMarker
        withTempExecutableBytes "htcc-test-runnable-systemv-non-elf-host-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
            assertEqual
                "final-output validation should accept structurally valid System V ELF executables without depending on host execution support"
                True
                validated

validateRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest :: Test
validateRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest =
    TestLabel "TestRunner.validate-runnable-linked-output-rejects-interpreterless-shared-objects" $ TestCase $ do
        withTempExecutableBytes "htcc-test-runnable-shared-object-." linkedSharedObjectHeader $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should reject ET_DYN shared-library outputs that omit PT_INTERP and static-PIE flags"
                False
                validated

validateRunnableLinkedOutputRejectsMissingDynamicLoaderTest :: Test
validateRunnableLinkedOutputRejectsMissingDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-missing-dynamic-loader" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:missing-loader"
            missingInterpreter = "/htcc-review/nonexistent-ld-linux-x86-64.so.2"
            executableBytes =
                linkedElfHeaderWithInterpreter 2 missingInterpreter <> asciiBytes probeMarker
        withTempExecutableBytes "htcc-test-runnable-missing-loader-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
            assertEqual
                "probe validation should accept structurally valid ET_EXEC outputs without checking whether the PT_INTERP loader exists on the current host"
                True
                validated

validateRunnableLinkedOutputAcceptsUnreadableDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsUnreadableDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-unreadable-dynamic-loader" $ TestCase $ do
        maybeUnreadableInterpreter <- findUnreadableExecutableFile
        case maybeUnreadableInterpreter of
            Nothing ->
                pure ()
            Just unreadableInterpreter -> do
                let probeMarker = "htcc-probe-marker:unreadable-loader"
                    executableBytes =
                        linkedElfHeaderWithInterpreter 2 unreadableInterpreter <> asciiBytes probeMarker
                withTempExecutableBytes "htcc-test-runnable-unreadable-loader-bin-." executableBytes $ \executablePath -> do
                    validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                    assertEqual
                        "probe validation should accept ET_EXEC outputs with unreadable PT_INTERP targets because loader readability is not part of structural ELF validation"
                        True
                        validated

validateRunnableLinkedOutputAcceptsRecognizedDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsRecognizedDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-recognized-dynamic-loader" $ TestCase $
        withTempDirectory "htcc-test-runnable-valid-loader-." $ \tmpDir -> do
            let probeMarker = "htcc-probe-marker:valid-loader"
                loaderPath = tmpDir </> "ld-linux-x86-64.so.2"
                executableBytes =
                    linkedElfHeaderWithInterpreter 2 loaderPath <> asciiBytes probeMarker
            B.writeFile loaderPath linkedSharedObjectHeader
            setFileMode loaderPath ownerModes
            withTempExecutableBytes "htcc-test-runnable-valid-loader-bin-." executableBytes $ \executablePath -> do
                validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                assertEqual
                    "probe validation should accept ET_EXEC outputs whose PT_INTERP names a loader-shaped ELF binary"
                    True
                    validated

validateRunnableLinkedOutputAcceptsExecuteOnlyDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsExecuteOnlyDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-execute-only-dynamic-loader" $ TestCase $
        withTempDirectory "htcc-test-runnable-exec-only-loader-." $ \tmpDir -> do
            let probeMarker = "htcc-probe-marker:exec-only-loader"
                loaderPath = tmpDir </> "ld-linux-x86-64.so.2"
                executableBytes =
                    linkedElfHeaderWithInterpreter 2 loaderPath <> asciiBytes probeMarker
            B.writeFile loaderPath linkedSharedObjectHeader
            setFileMode loaderPath ownerExecuteMode
            withTempExecutableBytes "htcc-test-runnable-exec-only-loader-bin-." executableBytes $ \executablePath -> do
                validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                assertEqual
                    "probe validation should accept execute-only PT_INTERP loader paths because loader permissions are not part of structural ELF validation"
                    True
                    validated

validateRunnableLinkedOutputAcceptsBsdDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsBsdDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-bsd-dynamic-loader" $ TestCase $
        withTempDirectory "htcc-test-runnable-bsd-loader-." $ \tmpDir -> do
            let probeMarker = "htcc-probe-marker:bsd-loader"
                loaderPath = tmpDir </> "ld.elf_so"
                executableBytes =
                    linkedElfHeaderWithInterpreter 2 loaderPath <> asciiBytes probeMarker
            B.writeFile loaderPath linkedSharedObjectHeader
            setFileMode loaderPath ownerModes
            withTempExecutableBytes "htcc-test-runnable-bsd-loader-bin-." executableBytes $ \executablePath -> do
                validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                assertEqual
                    "probe validation should accept BSD loader-shaped PT_INTERP outputs"
                    True
                    validated

validateRunnableLinkedOutputAcceptsRenamedDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsRenamedDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-renamed-dynamic-loader" $ TestCase $
        withTempDirectory "htcc-test-runnable-libc-loader-." $ \tmpDir -> do
            let probeMarker = "htcc-probe-marker:renamed-loader"
                loaderPath = tmpDir </> "sandboxed-loader-copy"
                executableBytes =
                    linkedElfHeaderWithInterpreter 2 loaderPath <> asciiBytes probeMarker
            B.writeFile loaderPath linkedSharedObjectHeader
            setFileMode loaderPath ownerModes
            withTempExecutableBytes "htcc-test-runnable-libc-loader-bin-." executableBytes $ \executablePath -> do
                validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                assertEqual
                    "probe validation should accept valid PT_INTERP targets with renamed loader paths"
                    True
                    validated

validateRunnableLinkedOutputAcceptsDependentDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsDependentDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-dependent-dynamic-loader" $ TestCase $
        withTempDirectory "htcc-test-runnable-dependent-loader-." $ \tmpDir -> do
            let probeMarker = "htcc-probe-marker:dependent-loader"
                loaderPath = tmpDir </> "ld-linux-x86-64.so.2"
                dependentLoaderBytes =
                    linkedElfHeaderWithInterpreterAndDynamicEntries
                        3
                        []
                        [ (elfDynamicTagNeeded, 1)
                        , (elfDynamicTagNull, 0)
                        ]
                executableBytes =
                    linkedElfHeaderWithInterpreter 2 loaderPath <> asciiBytes probeMarker
            B.writeFile loaderPath dependentLoaderBytes
            setFileMode loaderPath ownerModes
            withTempExecutableBytes "htcc-test-runnable-dependent-loader-bin-." executableBytes $ \executablePath -> do
                validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
                assertEqual
                    "probe validation should accept ET_EXEC outputs without inspecting standalone PT_INTERP loader semantics"
                    True
                    validated

validateRunnableLinkedOutputAcceptsSpecialFileDynamicLoaderTest :: Test
validateRunnableLinkedOutputAcceptsSpecialFileDynamicLoaderTest =
    TestLabel "TestRunner.validate-runnable-linked-output-accepts-special-file-dynamic-loader" $ TestCase $ do
        let probeMarker = "htcc-probe-marker:special-loader"
            executableBytes =
                linkedElfHeaderWithInterpreter 2 "/dev/null" <> asciiBytes probeMarker
        withTempExecutableBytes "htcc-test-runnable-special-loader-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath (Just probeMarker)
            assertEqual
                "probe validation should accept PT_INTERP paths naming special files"
                True
                validated

validateRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest :: Test
validateRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest =
    TestLabel "TestRunner.validate-runnable-linked-output-rejects-dynamic-executables-without-interpreter" $ TestCase $ do
        let executableBytes =
                linkedElfHeaderWithInterpreterAndDynamicEntries 2 [] [(elfDynamicTagNull, 0)]
        withTempExecutableBytes "htcc-test-runnable-missing-interpreter-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should reject ET_EXEC outputs that carry PT_DYNAMIC but omit PT_INTERP"
                False
                validated

validateRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest :: Test
validateRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest =
    TestLabel "TestRunner.validate-runnable-linked-output-rejects-pie-marked-dependent-shared-objects" $ TestCase $ do
        let executableBytes =
                linkedElfHeaderWithInterpreterAndDynamicEntries
                    3
                    []
                    [ (elfDynamicTagFlags1, elfDynamicFlag1Pie)
                    , (elfDynamicTagNeeded, 1)
                    , (elfDynamicTagNull, 0)
                    ]
        withTempExecutableBytes "htcc-test-runnable-dependent-static-pie-." executableBytes $ \executablePath -> do
            validated <- validateRunnableLinkedOutput executablePath Nothing
            assertEqual
                "final-output validation should reject ET_DYN outputs that still need DT_NEEDED resolution when no PT_INTERP loader is present"
                False
                validated

assemblerCommandAvailableCreatesProbeFilesInWorkingDirectoryTest :: Test
assemblerCommandAvailableCreatesProbeFilesInWorkingDirectoryTest =
    TestLabel "TestRunner.assembler-command-available-creates-probe-files-in-working-directory" $ TestCase $
        withTempDirectory "htcc-test-probe-dir-." $ \probeDir ->
            withTempExecutableScript "htcc-test-probe-fail-.sh" "#!/bin/sh\nexit 1\n" $ \scriptPath -> do
                firstProbePaths <- newIORef Nothing
                available <- assemblerCommandAvailableInDirectoryWith probeDir $ \args -> do
                    case args of
                        [ "-x"
                            , "assembler"
                            , "-c"
                            , "-o"
                            , objPath
                            , asmPath
                            ] -> writeIORef firstProbePaths (Just (objPath, asmPath))
                        _ -> assertFailure $ "unexpected assembler probe arguments: " <> show args
                    resolveCompilerCommand scriptPath
                recordedPaths <- readIORef firstProbePaths
                case recordedPaths of
                    Just (objPath, asmPath) -> do
                        assertEqual
                            "the assembler probe should create its temporary assembly file in the working directory"
                            probeDir
                            (takeDirectory asmPath)
                        assertEqual
                            "the assembler probe should create its temporary object file in the working directory"
                            probeDir
                            (takeDirectory objPath)
                    Nothing ->
                        assertFailure "expected the assembler probe to record its temporary file paths"
                assertEqual
                    "a failing assembler command should still report the probe as unavailable"
                    False
                    available

assemblerCommandAvailableRunsRelativeWrapperFromProbeDirectoryTest :: Test
assemblerCommandAvailableRunsRelativeWrapperFromProbeDirectoryTest =
    TestLabel "TestRunner.assembler-command-available-runs-relative-wrapper-from-probe-directory" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else
                withTempDirectory "htcc-test-relative-assembler-root-." $ \rootDir -> do
                    let probeDir = rootDir </> "probe"
                        wrapperName = "assembler-wrapper.sh"
                        wrapperPath = rootDir </> wrapperName
                        logPath = rootDir </> "assembler-wrapper.log"
                    createDirectory probeDir
                    compiler <- assemblerCompilerCommand
                    let wrapperScriptLines = lines $ assemblerWrapperScript compiler
                    writeExecutableScript wrapperPath . unlines $
                        [head wrapperScriptLines, "pwd > " <> shellQuote logPath]
                            <> tail wrapperScriptLines
                    available <- assemblerCommandAvailableInDirectoryWith probeDir $
                        const (resolveCompilerCommandIn (Just probeDir) $ "../" <> wrapperName)
                    loggedDir <- catchIOError (readFile logPath) (const $ pure "")
                    assertEqual
                        "the assembler probe should execute relative wrappers from the directory being validated"
                        True
                        available
                    assertBool
                        "the probe should run the wrapper from the requested probe directory instead of the runner cwd"
                        (probeDir `isInfixOf` loggedDir)

assemblerCommandAvailablePreservesProbePwdWithoutOverridesTest :: Test
assemblerCommandAvailablePreservesProbePwdWithoutOverridesTest =
    TestLabel "TestRunner.assembler-command-available-preserves-probe-pwd-without-overrides" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else do
                compiler <- assemblerCompilerCommand
                withTempDirectory "htcc-test-probe-pwd-env-." $ \probeDir ->
                    withTempDirectory "htcc-test-parent-pwd-env-." $ \parentPwd -> do
                        let wrapperPath = probeDir </> "htcc-test-assembler-pwd"
                            wrapperScriptLines = lines $ assemblerWrapperScript compiler
                        writeExecutableScript wrapperPath . unlines $
                            [head wrapperScriptLines, "[ \"$PWD\" = " <> shellQuote probeDir <> " ]"]
                                <> tail wrapperScriptLines
                        withEnvVar "PWD" (Just parentPwd) $ do
                            available <- assemblerCommandAvailableInDirectoryWith probeDir $
                                const (resolveCompilerCommand wrapperPath)
                            assertEqual
                                "assembler probing should export the probe directory as PWD even when the configured wrapper has no explicit env overrides"
                                True
                                available

assemblerCommandAvailableWritesIntelSyntaxProbeTest :: Test
assemblerCommandAvailableWritesIntelSyntaxProbeTest =
    TestLabel "TestRunner.assembler-command-available-writes-intel-syntax-probe" $ TestCase $
        withTempDirectory "htcc-test-intel-probe-dir-." $ \probeDir ->
            withTempExecutableScript "htcc-test-probe-fail-.sh" "#!/bin/sh\nexit 1\n" $ \scriptPath -> do
                probeAsmContents <- newIORef Nothing
                available <- assemblerCommandAvailableInDirectoryWith probeDir $ \args -> do
                    case args of
                        [ "-x"
                            , "assembler"
                            , "-c"
                            , "-o"
                            , _
                            , asmPath
                            ] -> readFile asmPath >>= writeIORef probeAsmContents . Just
                        _ -> pure ()
                    resolveCompilerCommand scriptPath
                assertEqual
                    "a failing assembler command should still report the probe as unavailable"
                    False
                    available
                recordedProbeAsm <- readIORef probeAsmContents
                case recordedProbeAsm of
                    Just asmContents -> do
                        assertBool
                            "the assembler probe should emit Intel-syntax assembly so it exercises the same syntax htcc itself generates"
                            (".intel_syntax noprefix" `isInfixOf` asmContents
                                && "lea rdx, [rip + htcc_test_probe_marker]" `isInfixOf` asmContents
                                && "xor eax, eax" `isInfixOf` asmContents
                            )
                        assertBool
                            "the assembler probe should no longer use the old AT&T-syntax operands"
                            (not ("leaq htcc_test_probe_marker(%rip), %rdx" `isInfixOf` asmContents))
                    Nothing ->
                        assertFailure "expected the assembler probe to record its Intel-syntax assembly source"

assemblerCommandAvailableUsesTemporaryDirectoryByDefaultTest :: Test
assemblerCommandAvailableUsesTemporaryDirectoryByDefaultTest =
    TestLabel "TestRunner.assembler-command-available-checks-temporary-and-current-directories-by-default" $ TestCase $
        withTempDirectory "htcc-test-noexec-like-cwd-." $ \workingDir -> do
            withTempExecutableScript "htcc-test-probe-fail-.sh" "#!/bin/sh\nexit 1\n" $ \scriptPath -> do
                assemblyProbePaths <- newIORef ([] :: [(FilePath, FilePath)])
                available <- assemblerCommandAvailableWithDirectories
                    [ getTemporaryDirectory
                    , pure workingDir
                    ]
                    (\args -> do
                        case args of
                            [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                , objPath
                                , asmPath
                                ] -> modifyIORef' assemblyProbePaths (++ [(objPath, asmPath)])
                            _ -> assertFailure $ "unexpected assembler probe arguments: " <> show args
                        resolveCompilerCommand scriptPath
                    )
                assertEqual
                    "a failing assembler command should still report the probe as unavailable"
                    False
                    available
                recordedPaths <- readIORef assemblyProbePaths
                case recordedPaths of
                    (objPath, asmPath):_ -> do
                        assertBool
                            "the default assembler probe should still start from a temporary directory before considering any fallback"
                            (takeDirectory asmPath /= workingDir)
                        assertBool
                            "the default assembler probe should still avoid placing its initial temporary object files under the current working directory"
                            (takeDirectory objPath /= workingDir)
                        assertBool
                            "the default assembler probe should also verify the current working directory because subprocess tests execute from both locations"
                            (any ((== workingDir) . takeDirectory . snd) recordedPaths)
                    [] ->
                        assertFailure "expected the default assembler probe to record its temporary file paths"

probeAvailableDirectoriesRequiresEveryDirectoryTest :: Test
probeAvailableDirectoriesRequiresEveryDirectoryTest =
    TestLabel "TestRunner.probe-available-directories-requires-every-directory" $ TestCase $ do
        probedDirectories <- newIORef ([] :: [FilePath])
        available <- probeAvailableDirectories
            [ pure "/tmp/htcc-probe-noexec"
            , pure "/workspaces/htcc"
            ]
            (\directory -> do
                modifyIORef' probedDirectories (++ [directory])
                pure $ directory == "/workspaces/htcc"
            )
        assertEqual
            "directory probing should fail when any required execution directory is unsuitable"
            False
            available
        assertEqual
            "directory probing should still preserve directory order while checking every required location"
            ["/tmp/htcc-probe-noexec", "/workspaces/htcc"]
            =<< readIORef probedDirectories

probeAvailableDirectoriesSkipsDuplicateDirectoriesTest :: Test
probeAvailableDirectoriesSkipsDuplicateDirectoriesTest =
    TestLabel "TestRunner.probe-available-directories-skips-duplicate-directories" $ TestCase $ do
        probedDirectories <- newIORef ([] :: [FilePath])
        available <- probeAvailableDirectories
            [ pure "/workspaces/htcc"
            , pure "/workspaces/htcc"
            ]
            (\directory -> do
                modifyIORef' probedDirectories (++ [directory])
                pure False
            )
        assertEqual
            "duplicate fallback directories should not trigger redundant probe attempts"
            False
            available
        assertEqual
            "the duplicate directory should only be probed once"
            ["/workspaces/htcc"]
            =<< readIORef probedDirectories

probeAvailableDirectoriesContinuesAfterProbeIOExceptionTest :: Test
probeAvailableDirectoriesContinuesAfterProbeIOExceptionTest =
    TestLabel "TestRunner.probe-available-directories-continues-after-probe-ioexception" $ TestCase $ do
        probedDirectories <- newIORef ([] :: [FilePath])
        available <- probeAvailableDirectories
            [ pure "/tmp/htcc-unusable"
            , pure "/workspaces/htcc"
            ]
            (\directory -> do
                modifyIORef' probedDirectories (++ [directory])
                if directory == "/tmp/htcc-unusable"
                    then throwIO (userError "temporary directory is not usable")
                    else pure True
            )
        assertEqual
            "per-directory IO failures should make directory probing fail because every execution directory must be verified"
            False
            available
        assertEqual
            "directory probing should continue to the next directory after an IOException so later locations are still inspected"
            ["/tmp/htcc-unusable", "/workspaces/htcc"]
            =<< readIORef probedDirectories

assemblerCommandAvailableRequiresEveryExecutionDirectoryTest :: Test
assemblerCommandAvailableRequiresEveryExecutionDirectoryTest =
    TestLabel "TestRunner.assembler-command-available-requires-every-execution-directory" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else do
                compiler <- assemblerCompilerCommand
                tempDir <- getTemporaryDirectory
                withTempDirectory "htcc-test-noexec-like-cwd-required-." $ \workingDir -> do
                    withTempExecutableScript "htcc-test-assembler-fail-.sh" "#!/bin/sh\nexit 1\n" $ \failingScript -> do
                        probeDirs <- newIORef ([] :: [FilePath])
                        available <- assemblerCommandAvailableWithDirectories
                            [ getTemporaryDirectory
                            , pure workingDir
                            ]
                            (\args -> do
                                let probeDir = case args of
                                        [ "-x"
                                            , "assembler"
                                            , "-c"
                                            , "-o"
                                            , objPath
                                            , _
                                            ] -> takeDirectory objPath
                                        [ "-no-pie"
                                            , "-o"
                                            , outputPath
                                            , _
                                            ] -> takeDirectory outputPath
                                        _ -> error $ "unexpected assembler probe arguments: " <> show args
                                modifyIORef' probeDirs (++ [probeDir])
                                if probeDir == workingDir
                                    then resolveCompilerCommand failingScript
                                    else pure compiler
                            )
                        assertEqual
                            "the default assembler probe should stay unavailable when any required execution directory fails"
                            False
                            available
                        recordedProbeDirs <- readIORef probeDirs
                        assertBool
                            "the default assembler probe should verify the temporary directory"
                            (tempDir `elem` recordedProbeDirs)
                        assertBool
                            "the default assembler probe should also verify the current working directory instead of stopping after the first success"
                            (workingDir `elem` recordedProbeDirs)

assemblerCommandAvailableDoesNotFallBackFromTemporaryDirectoryTest :: Test
assemblerCommandAvailableDoesNotFallBackFromTemporaryDirectoryTest =
    TestLabel "TestRunner.assembler-command-available-does-not-fall-back-from-temporary-directory" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else
                withTempDirectory "htcc-test-noexec-like-tmp-." $ \tmpDir -> do
                    probeDirs <- newIORef ([] :: [FilePath])
                    withTempExecutableScript "htcc-test-assembler-fail-.sh" "#!/bin/sh\nexit 1\n" $ \failingScript -> do
                        available <- assemblerCommandAvailableWithTempDirectory (pure tmpDir) $ \args -> do
                            let probeDir = case args of
                                    [ "-x"
                                        , "assembler"
                                        , "-c"
                                        , "-o"
                                        , objPath
                                        , _
                                        ] -> takeDirectory objPath
                                    [ "-no-pie"
                                        , "-o"
                                        , outputPath
                                        , _
                                        ] -> takeDirectory outputPath
                                    _ -> error $ "unexpected assembler probe arguments: " <> show args
                            modifyIORef' probeDirs (++ [probeDir])
                            resolveCompilerCommand failingScript
                        assertEqual
                            "the assembler probe should stay unavailable when the temp-dir probe fails instead of retrying elsewhere"
                            False
                            available
                    recordedProbeDirs <- readIORef probeDirs
                    assertBool
                        "the first assembler probe should still start in the configured temporary directory"
                        (case recordedProbeDirs of
                            firstProbeDir:_ -> firstProbeDir == tmpDir
                            []              -> False
                        )
                    assertBool
                        "the assembler probe should not retry in a second directory when the temporary directory probe fails"
                        (all (== tmpDir) recordedProbeDirs)

assemblerCommandAvailableRejectsLinkedOutputsWithoutProbeMarkerTest :: Test
assemblerCommandAvailableRejectsLinkedOutputsWithoutProbeMarkerTest =
    TestLabel "TestRunner.assembler-command-available-rejects-linked-outputs-without-probe-marker" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else
                withTempDirectory "htcc-test-markerless-probe-." $ \probeDir -> do
                    let relocatableObjectPath = probeDir <> "/markerless-probe.o"
                        cannedExecutablePath = probeDir <> "/markerless-probe.out"
                    B.writeFile relocatableObjectPath $ linkedElfHeader 1
                    B.writeFile cannedExecutablePath $ linkedElfHeader 2
                    setFileMode cannedExecutablePath ownerModes
                    withTempExecutableScript
                        "htcc-test-markerless-probe-.sh"
                        (probeBypassingAssemblerScript relocatableObjectPath cannedExecutablePath)
                        $ \scriptPath -> do
                            available <- assemblerCommandAvailableInDirectoryWith probeDir $
                                const (resolveCompilerCommand scriptPath)
                            assertEqual
                                "the assembler probe should reject linked outputs that do not embed the probe marker"
                                False
                                available

assemblerCommandAvailableAcceptsLinkedOutputsWithoutExecutingThemTest :: Test
assemblerCommandAvailableAcceptsLinkedOutputsWithoutExecutingThemTest =
    TestLabel "TestRunner.assembler-command-available-accepts-linked-outputs-without-executing-them" $ TestCase $
        if os /= "linux" || arch /= "x86_64"
            then pure ()
            else
                withTempDirectory "htcc-test-unrunnable-probe-." $ \probeDir -> do
                    let probeMarker = "htcc-probe-marker:unrunnable"
                        relocatableObjectPath = probeDir <> "/unrunnable-probe.o"
                        cannedExecutablePath = probeDir <> "/unrunnable-probe.out"
                    B.writeFile relocatableObjectPath $ linkedElfHeader 1
                    B.writeFile cannedExecutablePath $ linkedElfHeader 2 <> asciiBytes probeMarker
                    setFileMode cannedExecutablePath ownerModes
                    withTempExecutableScript
                        "htcc-test-unrunnable-probe-.sh"
                        (probeBypassingAssemblerScript relocatableObjectPath cannedExecutablePath)
                        $ \scriptPath -> do
                            available <- assemblerCommandAvailableInDirectoryWith probeDir $
                                const (resolveCompilerCommand scriptPath)
                            assertEqual
                                "the assembler probe should accept valid x86_64-ELF outputs without requiring them to execute on the host"
                                True
                                available

newProbeCallCounter :: IO (IORef Int)
newProbeCallCounter =
    newIORef 0

recordProbeCall :: IORef Int -> a -> IO a
recordProbeCall callCounter result = do
    modifyIORef' callCounter (+1)
    pure result

recordStep :: IORef [Int] -> Int -> IO ()
recordStep steps step =
    modifyIORef' steps (++ [step])

withTempExecutableScript :: String -> String -> (FilePath -> IO a) -> IO a
withTempExecutableScript prefix scriptContents action = do
    tmpDir <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile tmpDir prefix
            hPutStr handle scriptContents
            hClose handle
            setFileMode path ownerModes
            pure path
        )
        (\path -> catchIOError (removeFile path) $ const $ pure ())
        action

withTempExecutableBytes :: String -> B.ByteString -> (FilePath -> IO a) -> IO a
withTempExecutableBytes prefix executableBytes action = do
    tmpDir <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile tmpDir prefix
            B.hPut handle executableBytes
            hClose handle
            setFileMode path ownerModes
            pure path
        )
        (\path -> catchIOError (removeFile path) $ const $ pure ())
        action

assemblerWrapperScript :: CompilerCommand -> String
assemblerWrapperScript compiler = unlines
    [ "#!/bin/sh"
    , unwords $
        map renderEnvAssignment (compilerEnvOverrides compiler)
            <> ["exec", shellQuote (compilerExecutable compiler)]
            <> map shellQuote (compilerArguments compiler)
            <> ["\"$@\""]
    ]
    where
        renderEnvAssignment (name, value) =
            name <> "=" <> shellQuote value

shellQuote :: String -> String
shellQuote word =
    T.unpack $ quoteShellWord word

quoteShellWord :: String -> T.Text
quoteShellWord word =
    T.pack "'" <> T.replace (T.pack "'") (T.pack "'\"'\"'") (T.pack word) <> T.pack "'"

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
    tempDir <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile tempDir prefix
            hClose handle
            removeFile path
            createDirectory path
            pure path
        )
        (\path -> catchIOError (removeDirectoryRecursive path) $ const $ pure ())
        action

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name maybeValue =
    bracket
        (do
            previousValue <- lookupEnv name
            maybe (unsetEnv name) (setEnv name) maybeValue
            pure previousValue
        )
        (maybe (unsetEnv name) (setEnv name))
        . const

writeExecutableScript :: FilePath -> String -> IO ()
writeExecutableScript path scriptContents = do
    writeFile path scriptContents
    setFileMode path ownerModes

ensureExecutablePlaceholder :: FilePath -> IO ()
ensureExecutablePlaceholder path = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path $ unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "if [ \"$#\" -gt 0 ]; then"
        , "  [ -f \"$1\" ]"
        , "  printf '%s\\n' '.globl main' 'main:' '  ret'"
        , "fi"
        ]
    setFileMode path ownerModes

ensureZeroExitPlaceholder :: FilePath -> IO ()
ensureZeroExitPlaceholder path = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path "#!/bin/sh\nexit 0\n"
    setFileMode path ownerModes

ensureRepoRootMarker :: FilePath -> IO ()
ensureRepoRootMarker rootDir = do
    createDirectoryIfMissing True rootDir
    writeFile (rootDir <> "/htcc.cabal") "name: htcc\nversion: 0.0.0.1\n"

probeBypassingAssemblerScript :: FilePath -> FilePath -> String
probeBypassingAssemblerScript relocatableObjectPath cannedExecutablePath = unlines
    [ "#!/bin/sh"
    , "set -eu"
    , "case \"$1\" in"
    , "  -x)"
    , "    cat " <> shellQuote relocatableObjectPath <> " > \"$5\""
    , "    ;;"
    , "  -no-pie)"
    , "    cat " <> shellQuote cannedExecutablePath <> " > \"$3\""
    , "    chmod +x \"$3\""
    , "    ;;"
    , "  *)"
    , "    exit 1"
    , "    ;;"
    , "esac"
    ]

linkedElfHeader :: Int -> B.ByteString
linkedElfHeader elfType =
    linkedElfHeaderWithInterpreterBytes elfType []

linkedElfHeaderWithInterpreter :: Int -> String -> B.ByteString
linkedElfHeaderWithInterpreter elfType interpreterPath =
    linkedElfHeaderWithInterpreterBytes
        elfType
        (map (fromIntegral . fromEnum) interpreterPath <> [0])

linkedElfHeaderWithInterpreterBytes :: Int -> [Word8] -> B.ByteString
linkedElfHeaderWithInterpreterBytes elfType interpreterBytes =
    linkedElfHeaderWithInterpreterAndDynamicEntries
        elfType
        interpreterBytes
        (defaultDynamicEntries elfType interpreterBytes)

linkedSharedObjectHeader :: B.ByteString
linkedSharedObjectHeader =
    linkedElfHeaderWithInterpreterAndDynamicEntries 3 [] [(elfDynamicTagNull, 0)]

linkedElfHeaderWithInterpreterAndDynamicEntries :: Int -> [Word8] -> [(Int, Int)] -> B.ByteString
linkedElfHeaderWithInterpreterAndDynamicEntries elfType interpreterBytes dynamicEntries =
    B.pack $
        [ 0x7f, 0x45, 0x4c, 0x46
        , 0x02, 0x01, 0x01, 0x00
        , 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00
        ]
            <> word16le elfType
            <> word16le 0x3e
            <> word32le 0x1
            <> word64le (0x400000 + fromIntegral codeOffset)
            <> word64le 0x40
            <> word64le 0x0
            <> word32le 0x0
            <> word16le 0x40
            <> word16le 0x38
            <> word16le programHeaderCount
            <> word16le 0x0
            <> word16le 0x0
            <> word16le 0x0
            <> word32le 0x1
            <> word32le 0x5
            <> word64le 0x0
            <> word64le 0x400000
            <> word64le 0x400000
            <> word64le totalFileSize
            <> word64le totalFileSize
            <> word64le 0x1000
            <> interpreterProgramHeader
            <> dynamicProgramHeader
            <> [0x90]
            <> interpreterBytes
            <> dynamicBytes
    where
        hasInterpreter = not (null interpreterBytes)
        dynamicBytes = concatMap (\(tag, value) -> word64le (fromIntegral tag) <> word64le (fromIntegral value)) dynamicEntries
        hasDynamic = not (null dynamicBytes)
        programHeaderCount
            | hasInterpreter && hasDynamic = 3
            | hasInterpreter || hasDynamic = 2
            | otherwise = 1
        codeOffset = 0x40 + programHeaderCount * 0x38
        interpreterOffset = codeOffset + 1
        dynamicOffset = interpreterOffset + length interpreterBytes
        totalFileSize = fromIntegral $ dynamicOffset + length dynamicBytes
        interpreterProgramHeader
            | hasInterpreter =
                word32le 0x3
                    <> word32le 0x0
                    <> word64le (fromIntegral interpreterOffset)
                    <> word64le 0x0
                    <> word64le 0x0
                    <> word64le (fromIntegral $ length interpreterBytes)
                    <> word64le (fromIntegral $ length interpreterBytes)
                    <> word64le 0x1
            | otherwise =
                []
        dynamicProgramHeader
            | hasDynamic =
                word32le 0x2
                    <> word32le 0x0
                    <> word64le (fromIntegral dynamicOffset)
                    <> word64le (0x400000 + fromIntegral dynamicOffset)
                    <> word64le 0x0
                    <> word64le (fromIntegral $ length dynamicBytes)
                    <> word64le (fromIntegral $ length dynamicBytes)
                    <> word64le 0x8
            | otherwise =
                []

defaultDynamicEntries :: Int -> [Word8] -> [(Int, Int)]
defaultDynamicEntries elfType interpreterBytes
    | elfType == 3 && null interpreterBytes =
        [ (elfDynamicTagFlags1, elfDynamicFlag1Pie)
        , (elfDynamicTagNull, 0)
        ]
    | otherwise =
        []

setElfOsAbi :: Word8 -> B.ByteString -> B.ByteString
setElfOsAbi osAbi bytes =
    B.take 7 bytes <> B.singleton osAbi <> B.drop 8 bytes

foreignElfOsAbiForHost :: Word8
foreignElfOsAbiForHost
    | os == "freebsd" = 3
    | otherwise = 9

malformedLinkedElfBlob :: B.ByteString
malformedLinkedElfBlob =
    B.pack
        [ 0x7f, 0x45, 0x4c, 0x46
        , 0x02, 0x01, 0x01, 0x00
        , 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00
        , 0x02, 0x00
        , 0x3e, 0x00
        , 0xde, 0xad, 0xbe, 0xef
        ]

asciiBytes :: String -> B.ByteString
asciiBytes =
    B.pack . map (fromIntegral . fromEnum)

elfDynamicTagNull :: Int
elfDynamicTagNull = 0

elfDynamicTagNeeded :: Int
elfDynamicTagNeeded = 1

elfDynamicTagFlags1 :: Int
elfDynamicTagFlags1 = 0x6ffffffb

elfDynamicFlag1Pie :: Int
elfDynamicFlag1Pie = 0x08000000

word16le :: Int -> [Word8]
word16le value =
    [ fromIntegral value
    , fromIntegral $ value `div` 0x100
    ]

word32le :: Int -> [Word8]
word32le value =
    [ fromIntegral value
    , fromIntegral $ value `div` 0x100
    , fromIntegral $ value `div` 0x10000
    , fromIntegral $ value `div` 0x1000000
    ]

word64le :: Integer -> [Word8]
word64le value =
    map
        (fromIntegral . (`mod` 0x100))
        [ value
        , value `div` 0x100
        , value `div` 0x10000
        , value `div` 0x1000000
        , value `div` 0x100000000
        , value `div` 0x10000000000
        , value `div` 0x1000000000000
        , value `div` 0x100000000000000
        ]

findUnreadableExecutableFile :: IO (Maybe FilePath)
findUnreadableExecutableFile =
    go ["/usr/bin/sudo", "/bin/sudo"]
    where
        go [] =
            pure Nothing
        go (candidate : remainingCandidates) =
            catchIOError
                (do
                    status <- getFileStatus candidate
                    let mode = fileMode status
                        hasExecuteBits =
                            any
                                (/= 0)
                                [ intersectFileModes mode ownerExecuteMode
                                , intersectFileModes mode groupExecuteMode
                                , intersectFileModes mode otherExecuteMode
                                ]
                    if not (isRegularFile status) || not hasExecuteBits
                        then go remainingCandidates
                        else do
                            readable <- catchIOError (True <$ B.readFile candidate) (const $ pure False)
                            if readable
                                then go remainingCandidates
                                else pure (Just candidate)
                )
                (const $ go remainingCandidates)

test :: Test
test = TestList
    [ defaultCommandUsesSubProcOnLinuxX86_64Test
    , defaultCommandFallsBackToComponentsWhenCompilerProbeFailsTest
    , defaultCommandFallsBackToComponentsWhenAssemblerProbeFailsTest
    , defaultCommandStaysOnComponentsOffLinuxX86_64Test
    , autoHtccCommandPrefersEnvOverrideTest
    , autoHtccCommandUsesRepoBuiltBinaryWhenUnsetTest
    , autoHtccCommandUsesWindowsShellQuotingForRepoBuiltBinaryWhenUnsetTest
    , autoHtccCommandEscapesWindowsPercentSignsForRepoBuiltBinaryWhenUnsetTest
    , renderCompilerCommandUsesWindowsShellQuotingTest
    , renderCompilerCommandEscapesWindowsPercentSignsAndTrailingBackslashesTest
    , renderCompilerCommandPreservesWindowsEnvExpansionInOverridesTest
    , renderCompilerCommandPreservesDependentWindowsOverridesTest
    , renderCompilerCommandEscapesQuotedWindowsEnvOverrideValuesTest
    , renderCompilerCommandEnablesDelayedExpansionForWindowsBangPathsTest
    , expandWindowsEnvironmentVariablesMatchesCaseInsensitiveNamesTest
    , autoHtccCommandKeepsStackExecFallbackWhenRepoBuiltIsMissingTest
    , autoHtccCommandFallsBackToStackExecTest
    , autoHtccBinOverridePrefersEnvOverrideTest
    , autoHtccBinOverrideUsesRepoBuiltBinaryWhenUnsetTest
    , autoHtccBinOverrideFallsBackToStackExecWhenRepoBuiltIsMissingTest
    , autoHtccBinOverridePinsStackExecFallbackToCheckoutRootTest
    , pinStackLauncherToRepoRootAddsStackYamlTest
    , pinStackLauncherToRepoRootRespectsExistingStackYamlPinTest
    , pinnedStackLauncherRendersPortableHtccBinCommandTest
    , absoluteHtccCommandPreservesExplicitRelativeEnvOverrideTest
    , absoluteHtccCommandPreservesExplicitShellEnvOverrideTest
    , absoluteHtccCommandFallsBackToStackExecWhenRepoBuiltBinaryIsStaleTest
    , absoluteHtccCommandKeepsFreshRepoBuiltBinaryTest
    , absoluteHtccCommandRechecksRepoBuiltBinaryFreshnessAcrossCallsTest
    , absoluteHtccCommandIgnoresGeneratedArtifactsInFreshnessCheckTest
    , absoluteHtccCommandFallsBackToStackExecWhenCabalProjectIsNewerTest
    , absoluteHtccCommandFallsBackToStackExecWhenCabalProjectLocalIsNewerTest
    , absoluteHtccCommandFallsBackToStackExecWhenCabalProjectFreezeIsNewerTest
    , absoluteHtccCommandFallsBackToStackExecWhenRepoRootIsUnknownTest
    , commandsToRunIncludesComponentsForAutoSelectedSubProcTest
    , commandsToRunKeepsExplicitSubProcIsolatedTest
    , needsSubProcCompilerOverrideForAutoSelectedSubProcTest
    , needsSubProcCompilerOverrideForExplicitSubProcTest
    , needsSubProcCompilerOverrideSkipsNonSubProcCommandsTest
    , needsHtccCommandOverrideForExplicitSelfTest
    , needsHtccCommandOverrideSkipsCommandsWithoutHtccExecutionTest
    , defaultCommandRunsProbesOnLinuxX86_64Test
    , defaultCommandSkipsAssemblerProbeAfterCompilerFailureTest
    , defaultCommandFallsBackToComponentsWhenCompilerProbeThrowsTest
    , defaultCommandSkipsProbesOffLinuxX86_64Test
    , collectCommandExitCodesContinuesAfterFailureTest
    , collectCommandExitCodesConvertsUnexpectedExceptionsTest
    , collectCommandExitCodesRethrowsAsyncExceptionsTest
    , resolveCommandPrefersExplicitCommandTest
    , resolveCommandUsesDockerForCleanWithoutExplicitCommandTest
    , resolveCommandSkipsAutoDetectionForExplicitCommandTest
    , resolveCommandSkipsAutoDetectionForCleanWithoutCommandTest
    , assemblerCommandAvailableHandlesLeadingEnvAssignmentsTest
    , assemblerCommandAvailableRejectsWorkingDirectoryFallbackWhenPathIsOverriddenTest
    , compilerCommandAvailableRunsConfiguredCommandTest
    , compilerCommandAvailableHandlesShellExpandedPathAssignmentsTest
    , readCompilerProcessWithExitCodeExpandsPosixStyleEnvOverridesTest
    , readCompilerProcessWithExitCodePreservesInheritedPwdWithoutWorkingDirectoryTest
    , readCompilerProcessWithExitCodeExpandsLeadingTildeInPosixEnvOverridesTest
    , resolveCompilerCommandExpandsPosixStyleVariablesInCompilerWordsTest
    , resolveCompilerCommandExpandsLeadingTildeInCompilerWordsTest
    , resolveCompilerCommandExpandsCompilerWordsAgainstInheritedEnvironmentTest
    , resolveCompilerCommandExpandsCompilerWordsBeforeLeadingOverridesTest
    , resolveCompilerCommandWindowsExpandsCompilerWordsAfterLeadingOverridesTest
    , resolveCompilerCommandRetokenizesUnquotedInheritedCompilerExpansionsTest
    , resolveCompilerCommandWindowsRetokenizesExpandedCompilerWordsTest
    , resolveCompilerCommandWindowsPreservesQuotedCompilerWordsWithSpacesTest
    , resolveCompilerCommandKeepsExpandedAssignmentsAsCompilerWordsTest
    , resolveCompilerCommandKeepsQuotedExpandedAssignmentsAsCompilerWordsTest
    , resolveCompilerCommandExpandsQuotedBracedDefaultWordAcrossShellSpansTest
    , resolveCompilerCommandExpandsEscapedSpaceBracedAlternativeWordAcrossShellSpansTest
    , resolveCompilerCommandExpandsBracedDefaultWordWithAssignmentValueTildesTest
    , resolveCompilerCommandPreservesUnquotedWindowsBackslashesOnWindowsTest
    , resolveCompilerCommandWindowsPreservesTrailingBackslashArgumentTest
    , resolveCompilerCommandWindowsPreservesUnquotedUncExecutablePathTest
    , resolveCompilerCommandWindowsTreatsApostrophesAsLiteralCharactersTest
    , resolveCompilerCommandWindowsUnescapesCaretEscapedSpacesTest
    , resolveCompilerCommandWindowsDecodesQuotedCmdExeEscapesTest
    , resolveCompilerCommandWindowsPreservesEscapedLiteralExpansionSyntaxTest
    , resolveCompilerCommandAppliesBarePosixBackslashEscapesTest
    , resolveCompilerCommandPreservesQuotedPosixLiteralPathOverrideTest
    , readCompilerProcessWithExitCodePreservesQuotedAndEscapedPosixLiteralOverridesTest
    , resolveCompilerCommandFallsBackToWorkingDirectoryForBareLocalCommandTest
    , assemblerCompilerCommandFallsBackToBareLocalHtccAssemblerWrapperTest
    , renderCompilerCommandPreservesPosixEnvExpansionInOverridesTest
    , renderCompilerCommandPreservesTildeExpansionInPosixEnvOverridesTest
    , renderCompilerCommandPreservesLiteralPosixEnvOverridesTest
    , renderCompilerCommandPreservesQuotedParameterExpansionWordsTest
    , renderCompilerCommandEscapesLiteralPosixMetacharactersInExpandableOverridesTest
    , renderCompilerCommandEscapesLeadingHashInExpandableOverridesTest
    , compilerCommandAvailableRejectsRelativeWrapperMissingFromTemporaryDirectoryTest
    , compilerCommandAvailableRejectsZeroExitPlaceholderWrapperTest
    , compilerCommandAvailableHandlesResolutionFailureTest
    , findRepoBuiltExecutableNearPrefersMatchingOptimizedBuildArtifactTest
    , findRepoBuiltExecutableNearPrefersMatchingNooptBuildArtifactTest
    , findRepoBuiltExecutableNearIgnoresCheckoutPathNooptDirectoriesTest
    , findRepoBuiltExecutableNearPrefersBuildArtifactOverRepoRootExecutableTest
    , findRepoBuiltExecutableNearSupportsStackBuildLayoutTest
    , findRepoBuiltExecutableNearSupportsOutOfTreeCabalBuildArtifactTest
    , currentCheckoutRootFallsBackToCurrentDirectoryTest
    , resolveCheckoutRootAndCompilerFromDirectoriesKeepsSameCheckoutOutOfTreeBuildArtifactTest
    , resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromOutOfTreeBuildMetadataTest
    , resolveCheckoutRootAndCompilerFromDirectoriesRecoversCheckoutFromSpacedOutOfTreeBuildMetadataTest
    , resolveCheckoutRootAndCompilerFromDirectoriesUsesRepoRootFallbackForOutOfTreeBuildTest
    , resolveCheckoutRootAndCompilerFromDirectoriesIgnoresForeignOutOfTreeBuildArtifactTest
    , resolveCheckoutRootAndCompilerFromDirectoriesIgnoresSiblingPrefixOutOfTreeBuildArtifactTest
    , resolveCheckoutRootAndCompilerFromDirectoriesSkipsStaleEarlierBuildArtifactTest
    , findRepoBuiltExecutableNearStopsAtCheckoutRootTest
    , findRepoBuiltExecutableNearIgnoresAncestorWorkspaceMarkersTest
    , findRepoBuiltExecutableNearSkipsBrokenBuildArtifactTest
    , findRepoBuiltExecutableNearSkipsZeroExitPlaceholderBuildArtifactTest
    , looksRunnableLinkedOutputAcceptsPieExecutablesTest
    , looksRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest
    , looksRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest
    , looksRunnableLinkedOutputRejectsRelocatableObjectsTest
    , looksRunnableLinkedOutputRejectsMalformedExecutablesTest
    , looksRunnableLinkedOutputAcceptsLoadableExecutablesTest
    , looksRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest
    , looksRunnableLinkedOutputAcceptsForeignOsAbiTest
    , looksRunnableLinkedOutputRejectsMalformedInterpreterTest
    , looksRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest
    , validateRunnableLinkedOutputAcceptsLoadableExecutablesWithMarkerTest
    , shouldValidateRunnableLinkedOutputRejectsSpecialOutputsTest
    , shouldValidateRunnableLinkedOutputAcceptsFreshRegularTargetsTest
    , validateRunnableLinkedOutputRejectsSymlinkedLoadableExecutablesWithMarkerTest
    , validateRunnableLinkedOutputRejectsNonElfOutputsEvenWithMarkerTest
    , validateRunnableLinkedOutputAcceptsLoadableExecutablesWithoutRunningThemTest
    , validateRunnableLinkedOutputAcceptsPieExecutablesWithoutRunningThemTest
    , validateRunnableLinkedOutputRejectsForeignOsAbiDynamicExecutablesTest
    , validateRunnableLinkedOutputRejectsForeignOsAbiStaticExecutablesTest
    , validateRunnableLinkedOutputRejectsSystemVDynamicExecutablesOnNonElfHostsTest
    , validateRunnableLinkedOutputRejectsInterpreterlessSharedObjectsTest
    , validateRunnableLinkedOutputRejectsMissingDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsUnreadableDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsRecognizedDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsExecuteOnlyDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsBsdDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsRenamedDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsDependentDynamicLoaderTest
    , validateRunnableLinkedOutputAcceptsSpecialFileDynamicLoaderTest
    , validateRunnableLinkedOutputRejectsDynamicExecutablesWithoutInterpreterTest
    , validateRunnableLinkedOutputRejectsPieMarkedDependentSharedObjectsTest
    , assemblerCommandAvailableCreatesProbeFilesInWorkingDirectoryTest
    , assemblerCommandAvailableRunsRelativeWrapperFromProbeDirectoryTest
    , assemblerCommandAvailablePreservesProbePwdWithoutOverridesTest
    , assemblerCommandAvailableWritesIntelSyntaxProbeTest
    , assemblerCommandAvailableUsesTemporaryDirectoryByDefaultTest
    , probeAvailableDirectoriesRequiresEveryDirectoryTest
    , probeAvailableDirectoriesSkipsDuplicateDirectoriesTest
    , probeAvailableDirectoriesContinuesAfterProbeIOExceptionTest
    , assemblerCommandAvailableRequiresEveryExecutionDirectoryTest
    , assemblerCommandAvailableDoesNotFallBackFromTemporaryDirectoryTest
    , assemblerCommandAvailableRejectsLinkedOutputsWithoutProbeMarkerTest
    , assemblerCommandAvailableAcceptsLinkedOutputsWithoutExecutingThemTest
    ]
