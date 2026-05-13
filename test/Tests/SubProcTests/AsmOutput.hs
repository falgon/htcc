{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.SubProcTests.AsmOutput (
    externalBoolLowByteNormalizationTest,
    externalBoolParameterLowByteNormalizationTest,
    externalIntegralReturnNormalizationTest,
    outputFileTest,
    outputFileLegacyLongAliasTest,
    visualizeAstLegacyFlagsTest,
    visualizeAstPreservesParsedForSectionsTest,
    visualizeAstAcceptsAsmNormalizationFailureTest,
    visualizeAstRejectsNonFiniteResolutionTest,
    visualizeAstRejectsNonPositiveResolutionTest,
    visualizeAstRejectsDeclarationOnlyInputTest,
    visualizeAstRejectsNonSvgOutputTest,
    visualizeAstAcceptsSymlinkAliasToSvgTargetTest,
    visualizeAstRejectsSymlinkedNonSvgOutputTest,
    imgResolutionRequiresVisualizeAstTest,
    visualizeAstDefaultOutputSamePathTest,
    visualizeAstDefaultOutputMissingInputTest,
    visualizeAstSingleInputImplicitFunctionDefinitionWarningTest,
    visualizeAstSingleInputPrototypeRetypeTest,
    visualizeAstUsesMergedTentativeArrayTypeTest,
    visualizeAstWriteFailurePreservesExistingOutputTest,
    suppressWarnsCanonicalFlagTest,
    suppressWarnsLegacyFlagTest,
    suppressWarnsRunAsmTest,
    suppressWarnsRunAsmSuppressesStdoutWarningsTest,
    suppressWarnsRunAsmPreservesBinaryOutputTest,
    suppressWarnsRunAsmPreservesUnterminatedStderrTest,
    suppressWarnsRunAsmPreservesStdoutStderrInterleavingTest,
    suppressWarnsRunAsmPreservesSameReadStdoutInterleavingTest,
    suppressWarnsRunAsmPreservesSameReadStderrChunksTest,
    suppressWarnsRunAsmScopesRetainedIndicesByStreamTest,
    suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutTest,
    suppressWarnsRunAsmDropsSplitWarningPrefixTest,
    suppressWarnsRunAsmDropsCrossStreamWarningPreambleTest,
    suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrTest,
    suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutTest,
    suppressWarnsRunAsmDropsWarningPreambleTest,
    suppressWarnsRunAsmDropsAnsiWarningPreambleTest,
    suppressWarnsRunAsmDropsLeadingNoteTest,
    suppressWarnsRunAsmDropsDriverContextTest,
    suppressWarnsRunAsmDropsClangSnippetTest,
    suppressWarnsRunAsmDropsMultiLineClangSnippetTest,
    suppressWarnsRunAsmDropsSnippetOnlyContextTest,
    suppressWarnsRunAsmDropsCrLfWarningBlockTest,
    suppressWarnsRunAsmPreservesStandaloneLeadingNoteTest,
    suppressWarnsRunAsmPreservesIndentedPostWarningStderrTest,
    suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrTest,
    suppressWarnsRunAsmPreservesDirectivePostWarningStderrTest,
    suppressWarnsRunAsmPreservesStandalonePreSummaryNoteTest,
    suppressWarnsRunAsmPreservesStandalonePostWarningNoteTest,
    suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryTest,
    suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenTest,
    suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest,
    suppressWarnsRunAsmPreservesFailingDiagnosticsTest,
    suppressWarnsRunAsmPreservesErrorPreambleAfterWarningTest,
    suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesTest,
    suppressWarnsRunAsmStreamsRetainedStdoutPromptTest,
    suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineTest,
    suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrTest,
    suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleTest,
    suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleTest,
    suppressWarnsRunAsmProbeClosesStdinTest,
    suppressWarnsRunAsmPreservesStdinForRealInvocationsTest,
    outputFileSingleInputStaticTest,
    outputFileSingleInputImplicitFunctionTest,
    outputFileSingleInputImplicitFunctionConflictTest,
    outputFileSingleInputStaticImplicitFunctionConflictTest,
    outputFileSingleInputImplicitFunctionDefinitionWarningTest,
    outputFileSingleInputImplicitFunctionPrototypeWarningTest,
    outputFileSingleInputPrototypeRetypeTest,
    stdoutSingleInputImplicitFunctionConflictTest,
    stdoutMultiInputStaticFunctionTest,
    stdoutMultiInputImplicitFunctionDefinitionWarningTest,
    stdoutMultiInputSameInputImplicitFunctionDefinitionWarningTest,
    stdoutMultiInputImplicitFunctionUnresolvedWarningTest,
    stdoutMultiInputParseFailurePreservesWarningsTest,
    stdoutMultiInputPrototypeOnlyArityRetypeTest,
    stdoutMultiInputParameterIndirectFunctionPointerArityRetypeTest,
    outputFileMultiInputTest,
    outputFileMultiInputFunctionDeclarationConflictTest,
    outputFileMultiInputImplicitFunctionConflictTest,
    outputFileMultiInputConflictPreservesWarningsTest,
    outputFileMultiInputReadFailurePreservesWarningsTest,
    outputFileMultiInputImplicitFunctionTypeConflictTest,
    outputFileMultiInputImplicitFunctionTypeConflictReverseOrderTest,
    outputFileMultiInputFunctionTypeConflictTest,
    outputFileMultiInputFunctionPointerRedeclarationConflictTest,
    outputFileMultiInputAdjustedFunctionParamTypeTest,
    outputFileMultiInputCompatiblePrototypeMergeTest,
    outputFileMultiInputLargeStructReturnMergeTest,
    outputFileMultiInputRepeatedPrototypeTest,
    outputFileMultiInputTaggedPrototypeScopeMergeTest,
    outputFileMultiInputPrototypeOnlyArityRetypeTest,
    outputFileMultiInputSignedIntRedeclarationTest,
    outputFileMultiInputOldStyleDeclarationTest,
    outputFileMultiInputOldStylePromotionConflictTest,
    outputFileMultiInputVoidPrototypeConflictTest,
    outputFileMultiInputImplicitFunctionDefinitionWarningTest,
    outputFileMultiInputImplicitFunctionPrototypeWarningTest,
    outputFileMultiInputBlockScopeExternPrototypeWarningTest,
    outputFileMultiInputImplicitFunctionDefinitionTest,
    outputFileMultiInputBlockScopeExternPrototypeDefinitionTest,
    outputFileMultiInputBlockScopeExternObjectConflictTest,
    outputFileMultiInputBlockScopeExternObjectFunctionConflictTest,
    outputFileMultiInputBlockScopeExternPrototypeVisibilityTest,
    outputFileMultiInputDeferredIncompletePointeeUseTest,
    outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectTest,
    outputFileMultiInputDeferredIncompletePointerIncDecRejectTest,
    outputFileMultiInputImplicitFunctionArityRetypeTest,
    outputFileMultiInputImplicitFunctionObjectPointerRetypeTest,
    outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeTest,
    outputFileMultiInputIndirectFunctionPointerArityRetypeTest,
    outputFileMultiInputParameterSizeofRetypeTest,
    outputFileMultiInputIndirectFunctionPointerVoidRetypeTest,
    outputFileMultiInputImplicitFunctionVoidRetypeTest,
    outputFileMultiInputFunctionDesignatorAssignmentRetypeTest,
    outputFileMultiInputFunctionDesignatorReturnRetypeTest,
    outputFileMultiInputObjectPointerReturnRetypeTest,
    outputFileMultiInputFunctionDesignatorInitializerRetypeTest,
    outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeTest,
    outputFileMultiInputObjectPointerAssignmentRetypeTest,
    outputFileMultiInputObjectPointerInitializerRetypeTest,
    outputFileMultiInputPointerPointeeArrayConflictTest,
    outputFileMultiInputAggregateFunctionDesignatorInitializerTest,
    outputFileMultiInputSameInputImplicitFunctionDefinitionWarningTest,
    outputFileMultiInputSameInputImplicitFunctionConflictTest,
    outputFileMultiInputSameInputFunctionDeclarationConflictTest,
    outputFileMultiInputSameInputStaticImplicitFunctionConflictTest,
    outputFileMultiInputSameInputInternalLinkageConflictTest,
    outputFileMultiInputTentativeGlobalTest,
    outputFileMultiInputExternGlobalDeclarationMergeTest,
    outputFileMultiInputTentativeArrayTest,
    outputFileMultiInputTentativeArrayDecayRetypeTest,
    outputFileMultiInputTentativeIncompleteArrayTest,
    outputFileMultiInputTentativeNestedIncompleteArrayTest,
    outputFileMultiInputTentativeNestedArrayExtentInferenceTest,
    outputFileMultiInputTentativeArrayRankConflictTest,
    outputFileMultiInputTentativeArrayInnerExtentConflictTest,
    outputFileMultiInputTentativeArrayUseSiteTest,
    outputFileMultiInputTentativeArrayAddressUseSiteTest,
    outputFileMultiInputTentativeArrayInitializerRetypeTest,
    outputFileMultiInputStaticTest,
    outputFileMultiInputStaticFunctionTest,
    outputFileMultiInputStaticFunctionPointerTest,
    outputFilePreservesExistingModeTest,
    outputFileClearsSpecialBitsTest,
    outputFileFollowsSymlinkTargetTest,
    outputFileSpecialPathDevNullTest,
    outputFileSamePathTest,
    outputFileMissingInputMatchingOutputPathTest,
    outputFileHardLinkAliasTest,
    outputFileParseFailurePreservesExistingOutputTest,
    outputFileReadFailurePreservesExistingOutputTest,
    outputFileOpenFailurePreservesExistingOutputTest,
    outputFileHardLinkedRenameReplacementPreservesAliasTest,
    outputFileReadOnlyParentWritableTargetTest,
    outputFileReadOnlyParentWriteOnlyTargetTest,
    outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputTest,
    outputFileReadOnlyParentWriteFailurePreservesExistingOutputTest,
    outputFileWriteFailurePreservesExistingOutputTest,
    outputFileFreshOutputRestrictiveUmaskTest,
    runAsmTest,
    runAsmDoesNotInjectValidationMarkerIntoFinalAsmTest,
    runAsmAcceptsUserDefinedValidationMarkerSymbolTest,
    runAsmAcceptsGcSectionsLinkerTest,
    runAsmSingleInputImplicitFunctionConflictTest,
    runAsmSpecialPathDevNullTest,
    runAsmSpecialPathDevNullSkipsPostLinkValidationWaitTest,
    runAsmPreservesExecutableBitsTest,
    runAsmPreservesExistingExecuteMaskTest,
    runAsmRestoresOwnerExecuteBitTest,
    runAsmFreshOutputInPlaceLinkDriverTest,
    runAsmProbePreservesPrecreatedOutputTest,
    runAsmClearsSpecialBitsTest,
    runAsmLinkUsesResolvedDriverTest,
    runAsmBareLocalAssemblerPathTest,
    runAsmQuotedCompilerTest,
    runAsmWrappedAssemblerTest,
    runAsmWrappedAssemblerFirstWordDriverTest,
    runAsmWrappedAssemblerProbeFallbackTest,
    runAsmWrappedAssemblerHostMetadataFallbackTest,
    runAsmWrappedAssemblerDelayedChildHostMetadataTest,
    runAsmWrappedAssemblerDelayedChildFinalOutputTest,
    runAsmDoesNotWaitOnBackgroundProcessGroupHelpersTest,
    runAsmProbeDoesNotHangOnInheritedPipeHandlesTest,
    runAsmProbeDoesNotHangOnEarlyClosedStdoutTest,
    runAsmTildeExpandedAssemblerPathTest,
    runAsmLeadingEnvAssignmentTest,
    runAsmLeadingEnvAssignmentExpandsTildePathTest,
    runAsmExpandedAssignmentWordFailsTest,
    runAsmQuotedAssignmentLikeExecutableTest,
    runAsmEscapedAssignmentLikeExecutableTest,
    runAsmLeadingEnvAssignmentWithoutEnvPathTest,
    runAsmLeadingEnvAssignmentPreservesPathOverrideTest,
    runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangTest,
    runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionTest,
    runAsmLeadingEnvAssignmentPreservesInheritedPwdTest,
    runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralTest,
    runAsmEnvPathOverrideEmptyEntryTest,
    runAsmEnvPathOverrideNoLocalFallbackTest,
    runAsmQuotedBackslashArgTest,
    runAsmIgnoresCcTest,
    runAsmGccPrefersPathTest,
    runAsmFailurePreservesExistingOutputTest,
    runAsmParseFailurePreservesExistingOutputTest,
    runAsmReadFailurePreservesExistingOutputTest,
    runAsmHardLinkedRenameReplacementPreservesAliasTest,
    runAsmReadOnlyParentWritableTargetTest,
    runAsmReadOnlyParentWriteOnlyTargetTest,
    runAsmReadOnlyParentExecutableOnlyTargetTest,
    runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputTest,
    runAsmReadOnlyParentLinkFailurePreservesExistingOutputTest,
    runAsmFailurePreservesInputOutputAliasTest,
    runAsmMissingInputMatchingOutputPathTest,
    runAsmFailurePreservesHardLinkInputOutputAliasTest,
    runAsmMalformedAssemblerPreservesExistingOutputTest,
    runAsmMalformedAssemblerTest,
    runAsmAcceptsFreeBsdElfTargetDriverTest,
    runAsmRejectsMissingAssemblerDriverTest,
    runAsmRejectsNonExecutableAssemblerDriverTest,
    runAsmRejectsAssemblerWithoutLinkDriverTest,
    runAsmRejectsSingleObjectOnlyLinkDriverTest,
    runAsmRejectsScriptLinkProbeDriverTest,
    runAsmRejectsSharedLinkProbeDriverTest,
    runAsmRejectsForeignAbiLinkProbeDriverTest,
    runAsmRejectsBlobLinkProbeDriverTest,
    runAsmRejectsSymlinkLinkProbeDriverTest,
    runAsmRejectsSymlinkSpecialLinkProbeDriverTest,
    runAsmRejectsNonExecutableLinkProbeDriverTest,
    runAsmRejectsMarkerlessFinalOutputTest,
    runAsmRejectsBogusFinalLinkOutputTest,
    runAsmRejectsNamedPipeFinalLinkOutputTest,
    runAsmRejectsExecutableObjectProbeDriverTest,
    runAsmRejectsSymlinkObjectProbeDriverTest,
    runAsmRejectsTouchingLinkDriverTest,
    runAsmRejectsIncompatibleTargetDriverTest,
    runAsmRejectsMetadataSpoofingDriverTest,
    runAsmRejectsWrappedNonElfDriverTest,
    runAsmFreshOutputRestrictiveUmaskTest
) where

import           Control.Exception     (finally)
import           Control.Monad         (when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromMaybe, isNothing)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Word             (Word8)
import           Numeric               (showHex)
import           System.Directory      (createDirectoryIfMissing, doesFileExist,
                                        findExecutable, getCurrentDirectory,
                                        listDirectory, removeFile)
import           System.Exit           (ExitCode (ExitSuccess))
import           System.FilePath       ((</>))
import           System.Info           (os)
import           System.IO             (hClose, openTempFile)
import           System.IO.Error       (catchIOError)
import           System.Posix.Files    (fileMode, getFileStatus,
                                        getSymbolicLinkStatus, groupExecuteMode,
                                        groupReadMode, groupWriteMode,
                                        intersectFileModes, isSymbolicLink,
                                        otherExecuteMode, otherReadMode,
                                        otherWriteMode, ownerExecuteMode,
                                        ownerReadMode, ownerWriteMode,
                                        setFileMode, setGroupIDMode,
                                        setUserIDMode, unionFileModes)
import           System.Posix.Types    (FileMode)
import           System.Process        (CreateProcess (std_err, std_out),
                                        StdStream (CreatePipe), proc,
                                        waitForProcess, withCreateProcess)
import           System.Timeout        (timeout)
import           Tests.Utils

source :: T.Text
source = "int main() { int x; x = 0; while (x < 2) { if (x == 1) goto done; x = x + 1; continue; } done: switch (x) { case 1: return 0; default: return 1; } }"

parseFailureSource :: T.Text
parseFailureSource = "int main( { return 0; }"

writeFailureSource :: T.Text
writeFailureSource = T.unlines $
    [ "int main() {"
    , "  int x;"
    , "  x = 0;"
    ]
        <> replicate 512 "  x = x + 1;"
        <> [ "  return x;"
           , "}"
           ]

containsTextsInOrder :: [T.Text] -> T.Text -> Bool
containsTextsInOrder =
    go
    where
        go [] _ = True
        go (needle:rest) haystack =
            case T.breakOn needle haystack of
                (_, suffix)
                    | T.null suffix -> False
                    | otherwise ->
                        go rest (T.drop (T.length needle) suffix)

outputFileMsg :: T.Text
outputFileMsg = "CLI -o writes complete asm to the requested file"

outputFileLegacyLongAliasMsg :: T.Text
outputFileLegacyLongAliasMsg = "CLI --out remains accepted as a legacy alias for --output"

visualizeAstLegacyFlagsMsg :: T.Text
visualizeAstLegacyFlagsMsg = "CLI --visualize-ast with --img-resolution still renders SVG output"

visualizeAstPreservesParsedForSectionsMsg :: T.Text
visualizeAstPreservesParsedForSectionsMsg = "CLI --visualize-ast renders the parsed AST for omitted for-loop sections"

visualizeAstAcceptsAsmNormalizationFailureMsg :: T.Text
visualizeAstAcceptsAsmNormalizationFailureMsg = "CLI --visualize-ast still renders parseable inputs even when asm normalization would fail"

visualizeAstRejectsNonFiniteResolutionMsg :: T.Text
visualizeAstRejectsNonFiniteResolutionMsg = "CLI --visualize-ast rejects non-finite --img-resolution values and falls back to the default size"

visualizeAstRejectsNonPositiveResolutionMsg :: T.Text
visualizeAstRejectsNonPositiveResolutionMsg = "CLI --visualize-ast rejects non-positive --img-resolution values and falls back to the default size"

visualizeAstRejectsDeclarationOnlyInputMsg :: T.Text
visualizeAstRejectsDeclarationOnlyInputMsg = "CLI --visualize-ast rejects declaration-only inputs that produce no renderable AST"

visualizeAstRejectsNonSvgOutputMsg :: T.Text
visualizeAstRejectsNonSvgOutputMsg = "CLI --visualize-ast rejects non-SVG output paths before rendering"

visualizeAstAcceptsSymlinkAliasToSvgTargetMsg :: T.Text
visualizeAstAcceptsSymlinkAliasToSvgTargetMsg = "CLI --visualize-ast accepts symlink aliases that resolve to SVG targets"

visualizeAstRejectsSymlinkedNonSvgOutputMsg :: T.Text
visualizeAstRejectsSymlinkedNonSvgOutputMsg = "CLI --visualize-ast rejects symlinked outputs whose resolved target is not SVG"

imgResolutionRequiresVisualizeAstMsg :: T.Text
imgResolutionRequiresVisualizeAstMsg = "CLI rejects --img-resolution unless --visualize-ast is also enabled"

visualizeAstDefaultOutputSamePathMsg :: T.Text
visualizeAstDefaultOutputSamePathMsg = "CLI --visualize-ast rejects the default out.svg path when it aliases the input"

visualizeAstDefaultOutputMissingInputMsg :: T.Text
visualizeAstDefaultOutputMissingInputMsg = "CLI --visualize-ast reports missing out.svg inputs instead of misclassifying them as output collisions"

visualizeAstSingleInputImplicitFunctionDefinitionWarningMsg :: T.Text
visualizeAstSingleInputImplicitFunctionDefinitionWarningMsg = "CLI --visualize-ast keeps same-file implicit-function warnings even when a later definition resolves the call"

visualizeAstSingleInputPrototypeRetypeMsg :: T.Text
visualizeAstSingleInputPrototypeRetypeMsg = "CLI --visualize-ast revalidates same-file direct calls after later prototype refinements"

visualizeAstUsesMergedTentativeArrayTypeMsg :: T.Text
visualizeAstUsesMergedTentativeArrayTypeMsg = "CLI --visualize-ast renders merge-refined tentative array types instead of stale parsed nodes"

visualizeAstWriteFailurePreservesExistingOutputMsg :: T.Text
visualizeAstWriteFailurePreservesExistingOutputMsg = "CLI --visualize-ast preserves existing outputs when SVG rendering fails after opening the replacement file"

suppressWarnsCanonicalFlagMsg :: T.Text
suppressWarnsCanonicalFlagMsg = "CLI --suppress-warns suppresses parser warnings with the canonical spelling"

suppressWarnsLegacyFlagMsg :: T.Text
suppressWarnsLegacyFlagMsg = "CLI --supress-warns still suppresses parser warnings"

suppressWarnsRunAsmMsg :: T.Text
suppressWarnsRunAsmMsg = "CLI --suppress-warns suppresses HTCC_ASSEMBLER warnings in -r mode"

suppressWarnsRunAsmSuppressesStdoutWarningsMsg :: T.Text
suppressWarnsRunAsmSuppressesStdoutWarningsMsg = "CLI --suppress-warns suppresses HTCC_ASSEMBLER warnings emitted on stdout in -r mode"

suppressWarnsRunAsmPreservesBinaryOutputMsg :: T.Text
suppressWarnsRunAsmPreservesBinaryOutputMsg = "CLI --suppress-warns preserves non-UTF-8 HTCC_ASSEMBLER output in -r mode"

suppressWarnsRunAsmPreservesUnterminatedStderrMsg :: T.Text
suppressWarnsRunAsmPreservesUnterminatedStderrMsg = "CLI --suppress-warns preserves unterminated HTCC_ASSEMBLER stderr when no warning matches in -r mode"

suppressWarnsRunAsmPreservesStdoutStderrInterleavingMsg :: T.Text
suppressWarnsRunAsmPreservesStdoutStderrInterleavingMsg = "CLI --suppress-warns preserves HTCC_ASSEMBLER stdout/stderr interleaving when no warnings are removed in -r mode"

suppressWarnsRunAsmPreservesSameReadStdoutInterleavingMsg :: T.Text
suppressWarnsRunAsmPreservesSameReadStdoutInterleavingMsg = "CLI --suppress-warns preserves stderr between same-read stdout lines in -r mode"

suppressWarnsRunAsmPreservesSameReadStderrChunksMsg :: T.Text
suppressWarnsRunAsmPreservesSameReadStderrChunksMsg = "CLI --suppress-warns preserves same-read stderr lines before later stdout in -r mode"

suppressWarnsRunAsmScopesRetainedIndicesByStreamMsg :: T.Text
suppressWarnsRunAsmScopesRetainedIndicesByStreamMsg = "CLI --suppress-warns keeps retained chunk indices scoped by stream in -r mode"

suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutMsg :: T.Text
suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutMsg = "CLI --suppress-warns drops HTCC_ASSEMBLER warnings even when stderr warning lines are split around interleaved stdout in -r mode"

suppressWarnsRunAsmDropsSplitWarningPrefixMsg :: T.Text
suppressWarnsRunAsmDropsSplitWarningPrefixMsg = "CLI --suppress-warns drops HTCC_ASSEMBLER warnings even when the diagnostic prefix reaches warning: only after a later pipe read in -r mode"

suppressWarnsRunAsmDropsCrossStreamWarningPreambleMsg :: T.Text
suppressWarnsRunAsmDropsCrossStreamWarningPreambleMsg = "CLI --suppress-warns drops HTCC_ASSEMBLER warning preambles even when the preamble is on stdout and the warning is on stderr in -r mode"

suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrMsg :: T.Text
suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrMsg = "CLI --suppress-warns preserves non-newline stdout bytes before later stderr in -r mode"

suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutMsg :: T.Text
suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutMsg = "CLI --suppress-warns preserves non-newline stderr bytes before later stdout in -r mode"

suppressWarnsRunAsmDropsWarningPreambleMsg :: T.Text
suppressWarnsRunAsmDropsWarningPreambleMsg = "CLI --suppress-warns drops HTCC_ASSEMBLER include-stack warning preambles in -r mode"

suppressWarnsRunAsmDropsAnsiWarningPreambleMsg :: T.Text
suppressWarnsRunAsmDropsAnsiWarningPreambleMsg = "CLI --suppress-warns drops ANSI-colored HTCC_ASSEMBLER warning preambles in -r mode"

suppressWarnsRunAsmDropsLeadingNoteMsg :: T.Text
suppressWarnsRunAsmDropsLeadingNoteMsg = "CLI --suppress-warns drops leading HTCC_ASSEMBLER note lines that precede warnings in -r mode"

suppressWarnsRunAsmDropsDriverContextMsg :: T.Text
suppressWarnsRunAsmDropsDriverContextMsg = "CLI --suppress-warns drops GCC/as warning context lines in -r mode"

suppressWarnsRunAsmDropsClangSnippetMsg :: T.Text
suppressWarnsRunAsmDropsClangSnippetMsg = "CLI --suppress-warns drops clang-style warning snippets in -r mode"

suppressWarnsRunAsmDropsMultiLineClangSnippetMsg :: T.Text
suppressWarnsRunAsmDropsMultiLineClangSnippetMsg = "CLI --suppress-warns drops multi-line clang-style warning snippets in -r mode"

suppressWarnsRunAsmDropsSnippetOnlyContextMsg :: T.Text
suppressWarnsRunAsmDropsSnippetOnlyContextMsg = "CLI --suppress-warns preserves snippet-like HTCC_ASSEMBLER stderr when no annotation follows in -r mode"

suppressWarnsRunAsmDropsCrLfWarningBlockMsg :: T.Text
suppressWarnsRunAsmDropsCrLfWarningBlockMsg = "CLI --suppress-warns drops CRLF-terminated HTCC_ASSEMBLER warning blocks in -r mode"

suppressWarnsRunAsmPreservesStandaloneLeadingNoteMsg :: T.Text
suppressWarnsRunAsmPreservesStandaloneLeadingNoteMsg = "CLI --suppress-warns preserves standalone HTCC_ASSEMBLER notes that precede later warnings in -r mode"

suppressWarnsRunAsmPreservesIndentedPostWarningStderrMsg :: T.Text
suppressWarnsRunAsmPreservesIndentedPostWarningStderrMsg = "CLI --suppress-warns preserves unrelated indented stderr after HTCC_ASSEMBLER warnings in -r mode"

suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrMsg :: T.Text
suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrMsg = "CLI --suppress-warns preserves unrelated stderr containing punctuation or brace terminators after HTCC_ASSEMBLER warnings in -r mode"

suppressWarnsRunAsmPreservesDirectivePostWarningStderrMsg :: T.Text
suppressWarnsRunAsmPreservesDirectivePostWarningStderrMsg = "CLI --suppress-warns preserves directive-like stderr after HTCC_ASSEMBLER warnings in -r mode"

suppressWarnsRunAsmPreservesStandalonePreSummaryNoteMsg :: T.Text
suppressWarnsRunAsmPreservesStandalonePreSummaryNoteMsg = "CLI --suppress-warns preserves standalone HTCC_ASSEMBLER notes that appear before a later warning summary in -r mode"

suppressWarnsRunAsmPreservesStandalonePostWarningNoteMsg :: T.Text
suppressWarnsRunAsmPreservesStandalonePostWarningNoteMsg = "CLI --suppress-warns preserves standalone post-warning notes in -r mode"

suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryMsg :: T.Text
suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryMsg = "CLI --suppress-warns preserves standalone post-warning notes after GCC-style warnings without a trailing summary in -r mode"

suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenMsg :: T.Text
suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenMsg = "CLI --suppress-warns keeps real error snippets even when source lines contain the token warning:"

suppressWarnsRunAsmPreservesWarningLabelErrorSnippetMsg :: T.Text
suppressWarnsRunAsmPreservesWarningLabelErrorSnippetMsg = "CLI --suppress-warns keeps real error snippets even when source lines begin with warning:"

suppressWarnsRunAsmPreservesFailingDiagnosticsMsg :: T.Text
suppressWarnsRunAsmPreservesFailingDiagnosticsMsg = "CLI --suppress-warns preserves HTCC_ASSEMBLER diagnostics when -r fails"

suppressWarnsRunAsmPreservesErrorPreambleAfterWarningMsg :: T.Text
suppressWarnsRunAsmPreservesErrorPreambleAfterWarningMsg = "CLI --suppress-warns preserves error preambles that follow suppressed HTCC_ASSEMBLER warnings in -r mode"

suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesMsg :: T.Text
suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesMsg = "CLI --suppress-warns -r returns even when an HTCC_ASSEMBLER wrapper leaves inherited pipe handles open after exit"

suppressWarnsRunAsmStreamsRetainedStdoutPromptMsg :: T.Text
suppressWarnsRunAsmStreamsRetainedStdoutPromptMsg = "CLI --suppress-warns streams retained HTCC_ASSEMBLER stdout before the wrapped assembler exits in -r mode"

suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineMsg :: T.Text
suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineMsg = "CLI --suppress-warns streams retained HTCC_ASSEMBLER stdout without a trailing newline before the wrapped assembler exits in -r mode"

suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrMsg :: T.Text
suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrMsg = "CLI --suppress-warns streams retained HTCC_ASSEMBLER stdout even while earlier unterminated stderr remains pending in -r mode"

suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleMsg :: T.Text
suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleMsg = "CLI --suppress-warns keeps HTCC_ASSEMBLER target probes working when warning preambles are split across stdout and stderr"

suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleMsg :: T.Text
suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleMsg = "CLI --suppress-warns keeps HTCC_ASSEMBLER target probes working when an earlier partial warning preamble completes on another stream only at EOF"

suppressWarnsRunAsmProbeClosesStdinMsg :: T.Text
suppressWarnsRunAsmProbeClosesStdinMsg = "CLI --suppress-warns closes stdin for HTCC_ASSEMBLER metadata probes in -r mode"

suppressWarnsRunAsmPreservesStdinForRealInvocationsMsg :: T.Text
suppressWarnsRunAsmPreservesStdinForRealInvocationsMsg = "CLI --suppress-warns preserves stdin for real HTCC_ASSEMBLER invocations in -r mode"

outputFileSingleInputStaticMsg :: T.Text
outputFileSingleInputStaticMsg = "CLI -o preserves internal-linkage symbols for single-input outputs"

outputFileSingleInputImplicitFunctionMsg :: T.Text
outputFileSingleInputImplicitFunctionMsg = "CLI -o keeps single-input implicit function calls on the standalone code path"

outputFileSingleInputImplicitFunctionConflictMsg :: T.Text
outputFileSingleInputImplicitFunctionConflictMsg = "CLI -o rejects single-input implicit-function/global collisions on the standalone code path"

outputFileSingleInputStaticImplicitFunctionConflictMsg :: T.Text
outputFileSingleInputStaticImplicitFunctionConflictMsg = "CLI -o rejects single-input implicit-function/static-global collisions on the standalone code path"

outputFileSingleInputImplicitFunctionDefinitionWarningMsg :: T.Text
outputFileSingleInputImplicitFunctionDefinitionWarningMsg = "CLI -o keeps same-file implicit-function warnings even when a later definition resolves the call"

outputFileSingleInputImplicitFunctionPrototypeWarningMsg :: T.Text
outputFileSingleInputImplicitFunctionPrototypeWarningMsg = "CLI -o keeps same-file implicit-function warnings when only a later prototype appears"

outputFileSingleInputPrototypeRetypeMsg :: T.Text
outputFileSingleInputPrototypeRetypeMsg = "CLI -o revalidates same-file direct calls after later prototype refinements"

stdoutSingleInputImplicitFunctionConflictMsg :: T.Text
stdoutSingleInputImplicitFunctionConflictMsg = "CLI stdout rejects single-input implicit-function/global collisions on the standalone code path"

stdoutMultiInputStaticFunctionMsg :: T.Text
stdoutMultiInputStaticFunctionMsg = "CLI stdout namespaces internal-linkage functions across multiple inputs"

stdoutMultiInputImplicitFunctionDefinitionWarningMsg :: T.Text
stdoutMultiInputImplicitFunctionDefinitionWarningMsg = "CLI stdout suppresses cross-input implicit-function warnings once another input defines the function"

stdoutMultiInputSameInputImplicitFunctionDefinitionWarningMsg :: T.Text
stdoutMultiInputSameInputImplicitFunctionDefinitionWarningMsg = "CLI stdout keeps same-input implicit-function warnings even when another input enables multi-input merging"

stdoutMultiInputImplicitFunctionUnresolvedWarningMsg :: T.Text
stdoutMultiInputImplicitFunctionUnresolvedWarningMsg = "CLI stdout keeps implicit-function warnings when other inputs do not provide a real declaration"

stdoutMultiInputParseFailurePreservesWarningsMsg :: T.Text
stdoutMultiInputParseFailurePreservesWarningsMsg = "CLI stdout flushes earlier warnings before aborting on a later multi-input parse failure"

stdoutMultiInputPrototypeOnlyArityRetypeMsg :: T.Text
stdoutMultiInputPrototypeOnlyArityRetypeMsg = "CLI stdout revalidates direct calls after merging later prototype-only declarations"

stdoutMultiInputParameterIndirectFunctionPointerArityRetypeMsg :: T.Text
stdoutMultiInputParameterIndirectFunctionPointerArityRetypeMsg = "CLI stdout revalidates indirect calls inside definitions after merged parameter-type refinements"

outputFileMultiInputMsg :: T.Text
outputFileMultiInputMsg = "CLI -o combines asm from multiple inputs into one file"

outputFileMultiInputFunctionDeclarationConflictMsg :: T.Text
outputFileMultiInputFunctionDeclarationConflictMsg = "CLI -o rejects function declarations that conflict with globals across multiple inputs"

outputFileMultiInputImplicitFunctionConflictMsg :: T.Text
outputFileMultiInputImplicitFunctionConflictMsg = "CLI -o rejects implicit function references that conflict with globals across multiple inputs"

outputFileMultiInputConflictPreservesWarningsMsg :: T.Text
outputFileMultiInputConflictPreservesWarningsMsg = "CLI -o preserves earlier warnings even when multi-input merge later fails"

outputFileMultiInputReadFailurePreservesWarningsMsg :: T.Text
outputFileMultiInputReadFailurePreservesWarningsMsg = "CLI -o flushes earlier warnings before aborting on a later multi-input read failure"

outputFileMultiInputImplicitFunctionTypeConflictMsg :: T.Text
outputFileMultiInputImplicitFunctionTypeConflictMsg = "CLI -o rejects implicit function references that conflict with later function declarations across multiple inputs"

outputFileMultiInputImplicitFunctionTypeConflictReverseOrderMsg :: T.Text
outputFileMultiInputImplicitFunctionTypeConflictReverseOrderMsg = "CLI -o rejects implicit function references that conflict with earlier function declarations across multiple inputs"

outputFileMultiInputFunctionTypeConflictMsg :: T.Text
outputFileMultiInputFunctionTypeConflictMsg = "CLI -o rejects incompatible extern function declarations across multiple inputs"

outputFileMultiInputFunctionPointerRedeclarationConflictMsg :: T.Text
outputFileMultiInputFunctionPointerRedeclarationConflictMsg = "CLI -o rejects extern globals that only looked compatible via function-return equality"

outputFileMultiInputAdjustedFunctionParamTypeMsg :: T.Text
outputFileMultiInputAdjustedFunctionParamTypeMsg = "CLI -o accepts compatible extern declarations after array/function parameter adjustment"

outputFileMultiInputCompatiblePrototypeMergeMsg :: T.Text
outputFileMultiInputCompatiblePrototypeMergeMsg = "CLI -o rejects extern function returns that only differ by pointee array bound inference"

outputFileMultiInputLargeStructReturnMergeMsg :: T.Text
outputFileMultiInputLargeStructReturnMergeMsg = "CLI -o rejects merged function definitions that resolve to unsupported large struct returns"

outputFileMultiInputRepeatedPrototypeMsg :: T.Text
outputFileMultiInputRepeatedPrototypeMsg = "CLI -o accepts repeated prototypes when another input provides the single function definition"

outputFileMultiInputTaggedPrototypeScopeMergeMsg :: T.Text
outputFileMultiInputTaggedPrototypeScopeMergeMsg = "CLI -o ignores per-input tagged-struct scope ids when merging compatible extern prototypes"

outputFileMultiInputPrototypeOnlyArityRetypeMsg :: T.Text
outputFileMultiInputPrototypeOnlyArityRetypeMsg = "CLI -o revalidates direct calls after merging later prototype-only declarations"

outputFileMultiInputSignedIntRedeclarationMsg :: T.Text
outputFileMultiInputSignedIntRedeclarationMsg = "CLI -o accepts compatible int/signed redeclarations across multiple inputs"

outputFileMultiInputOldStyleDeclarationMsg :: T.Text
outputFileMultiInputOldStyleDeclarationMsg = "CLI -o accepts old-style declarations when another input provides the function definition"

outputFileMultiInputOldStylePromotionConflictMsg :: T.Text
outputFileMultiInputOldStylePromotionConflictMsg = "CLI -o rejects old-style declarations that only match after default promotions"

outputFileMultiInputVoidPrototypeConflictMsg :: T.Text
outputFileMultiInputVoidPrototypeConflictMsg = "CLI -o rejects void prototypes that conflict with later parameterized declarations"

outputFileMultiInputImplicitFunctionDefinitionWarningMsg :: T.Text
outputFileMultiInputImplicitFunctionDefinitionWarningMsg = "CLI -o suppresses cross-input implicit-function warnings once another input defines the function"

outputFileMultiInputImplicitFunctionPrototypeWarningMsg :: T.Text
outputFileMultiInputImplicitFunctionPrototypeWarningMsg = "CLI -o suppresses cross-input implicit-function warnings once another input declares the function"

outputFileMultiInputBlockScopeExternPrototypeWarningMsg :: T.Text
outputFileMultiInputBlockScopeExternPrototypeWarningMsg = "CLI -o keeps implicit-function warnings when only a same-input block-scope extern prototype exists"

outputFileMultiInputImplicitFunctionDefinitionMsg :: T.Text
outputFileMultiInputImplicitFunctionDefinitionMsg = "CLI -o accepts implicit function calls when another input provides the function definition"

outputFileMultiInputBlockScopeExternPrototypeDefinitionMsg :: T.Text
outputFileMultiInputBlockScopeExternPrototypeDefinitionMsg = "CLI -o keeps block-scope extern prototypes available for cross-input function merges"

outputFileMultiInputBlockScopeExternObjectConflictMsg :: T.Text
outputFileMultiInputBlockScopeExternObjectConflictMsg = "CLI -o rejects block-scope extern objects that conflict with later object definitions"

outputFileMultiInputBlockScopeExternObjectFunctionConflictMsg :: T.Text
outputFileMultiInputBlockScopeExternObjectFunctionConflictMsg = "CLI -o rejects block-scope extern objects that collide with later function definitions"

outputFileMultiInputBlockScopeExternPrototypeVisibilityMsg :: T.Text
outputFileMultiInputBlockScopeExternPrototypeVisibilityMsg = "CLI -o keeps block-scope extern prototype refinements from escaping into later file-scope call checking"

outputFileMultiInputDeferredIncompletePointeeUseMsg :: T.Text
outputFileMultiInputDeferredIncompletePointeeUseMsg = "CLI -o rejects cross-input function returns that only differ by pointee array bound inference"

outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectMsg :: T.Text
outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectMsg = "CLI -o rejects deferred +=/-= on incomplete pointers before emitting asm"

outputFileMultiInputDeferredIncompletePointerIncDecRejectMsg :: T.Text
outputFileMultiInputDeferredIncompletePointerIncDecRejectMsg = "CLI -o rejects deferred ++/-- on incomplete pointers before emitting asm"

outputFileMultiInputImplicitFunctionArityRetypeMsg :: T.Text
outputFileMultiInputImplicitFunctionArityRetypeMsg = "CLI -o revalidates implicit calls against later merged function arity"

outputFileMultiInputImplicitFunctionObjectPointerRetypeMsg :: T.Text
outputFileMultiInputImplicitFunctionObjectPointerRetypeMsg = "CLI -o rejects implicit calls that later resolve to object-pointer parameters"

outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeMsg :: T.Text
outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeMsg = "CLI -o rejects implicit calls whose merged object-pointer parameters are incompatible"

outputFileMultiInputIndirectFunctionPointerArityRetypeMsg :: T.Text
outputFileMultiInputIndirectFunctionPointerArityRetypeMsg = "CLI -o revalidates indirect calls after merged extern function-pointer declarations"

outputFileMultiInputParameterSizeofRetypeMsg :: T.Text
outputFileMultiInputParameterSizeofRetypeMsg = "CLI -o rejects cross-input parameter declarations that only differ by pointer-to-array bounds"

outputFileMultiInputIndirectFunctionPointerVoidRetypeMsg :: T.Text
outputFileMultiInputIndirectFunctionPointerVoidRetypeMsg = "CLI -o preserves void prototypes when merged extern function-pointer declarations refine empty parameter lists"

outputFileMultiInputImplicitFunctionVoidRetypeMsg :: T.Text
outputFileMultiInputImplicitFunctionVoidRetypeMsg = "CLI -o rejects implicit calls whose merged target later resolves to a void prototype"

outputFileMultiInputFunctionDesignatorAssignmentRetypeMsg :: T.Text
outputFileMultiInputFunctionDesignatorAssignmentRetypeMsg = "CLI -o rechecks function-designator assignments after merged prototypes"

outputFileMultiInputFunctionDesignatorReturnRetypeMsg :: T.Text
outputFileMultiInputFunctionDesignatorReturnRetypeMsg = "CLI -o rechecks function-pointer returns after merged prototypes"

outputFileMultiInputObjectPointerReturnRetypeMsg :: T.Text
outputFileMultiInputObjectPointerReturnRetypeMsg = "CLI -o rechecks object-pointer returns after merged tentative-array completion"

outputFileMultiInputFunctionDesignatorInitializerRetypeMsg :: T.Text
outputFileMultiInputFunctionDesignatorInitializerRetypeMsg = "CLI -o rechecks function-pointer initializers after merged prototypes"

outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeMsg :: T.Text
outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeMsg = "CLI -o rejects merged function-pointer initializers when parameter declarations conflict on pointer-to-array bounds"

outputFileMultiInputObjectPointerAssignmentRetypeMsg :: T.Text
outputFileMultiInputObjectPointerAssignmentRetypeMsg = "CLI -o rechecks object-pointer assignments after merged tentative-array completion"

outputFileMultiInputObjectPointerInitializerRetypeMsg :: T.Text
outputFileMultiInputObjectPointerInitializerRetypeMsg = "CLI -o rechecks object-pointer initializers after merged tentative-array completion"

outputFileMultiInputPointerPointeeArrayConflictMsg :: T.Text
outputFileMultiInputPointerPointeeArrayConflictMsg = "CLI -o rejects extern pointer-to-array declarations that disagree on pointee bounds"

outputFileMultiInputAggregateFunctionDesignatorInitializerMsg :: T.Text
outputFileMultiInputAggregateFunctionDesignatorInitializerMsg = "CLI -o accepts aggregate function-pointer initializers during merged global revalidation"

outputFileMultiInputSameInputImplicitFunctionDefinitionWarningMsg :: T.Text
outputFileMultiInputSameInputImplicitFunctionDefinitionWarningMsg = "CLI -o keeps same-input implicit-function warnings even when another input enables multi-input merging"

outputFileMultiInputSameInputImplicitFunctionConflictMsg :: T.Text
outputFileMultiInputSameInputImplicitFunctionConflictMsg = "CLI -o rejects same-input implicit-function/global collisions even when another input is present"

outputFileMultiInputSameInputFunctionDeclarationConflictMsg :: T.Text
outputFileMultiInputSameInputFunctionDeclarationConflictMsg = "CLI -o rejects same-input prototype/global collisions even when another input is present"

outputFileMultiInputSameInputStaticImplicitFunctionConflictMsg :: T.Text
outputFileMultiInputSameInputStaticImplicitFunctionConflictMsg = "CLI -o rejects same-input implicit-function/static-global collisions even when another input is present"

outputFileMultiInputSameInputInternalLinkageConflictMsg :: T.Text
outputFileMultiInputSameInputInternalLinkageConflictMsg = "CLI -o rejects same-input object/function collisions involving internal linkage even when another input is present"

outputFileMultiInputTentativeGlobalMsg :: T.Text
outputFileMultiInputTentativeGlobalMsg = "CLI -o coalesces tentative globals across multiple inputs"

outputFileMultiInputExternGlobalDeclarationMergeMsg :: T.Text
outputFileMultiInputExternGlobalDeclarationMergeMsg = "CLI -o treats extern-only globals as declarations during multi-input merges"

outputFileMultiInputTentativeArrayMsg :: T.Text
outputFileMultiInputTentativeArrayMsg = "CLI -o coalesces tentative array declarations across multiple inputs"

outputFileMultiInputTentativeArrayDecayRetypeMsg :: T.Text
outputFileMultiInputTentativeArrayDecayRetypeMsg = "CLI -o retypes earlier array-decay uses after cross-input tentative-array completion"

outputFileMultiInputTentativeIncompleteArrayMsg :: T.Text
outputFileMultiInputTentativeIncompleteArrayMsg = "CLI -o materializes merged tentative incomplete arrays as one element"

outputFileMultiInputTentativeNestedIncompleteArrayMsg :: T.Text
outputFileMultiInputTentativeNestedIncompleteArrayMsg = "CLI -o rejects address arithmetic on tentative nested arrays before any cross-input merge"

outputFileMultiInputTentativeNestedArrayExtentInferenceMsg :: T.Text
outputFileMultiInputTentativeNestedArrayExtentInferenceMsg = "CLI -o keeps each input bound to its own incomplete tentative nested-array use sites"

outputFileMultiInputTentativeArrayRankConflictMsg :: T.Text
outputFileMultiInputTentativeArrayRankConflictMsg = "CLI -o rejects tentative array merges that change array rank across multiple inputs"

outputFileMultiInputTentativeArrayInnerExtentConflictMsg :: T.Text
outputFileMultiInputTentativeArrayInnerExtentConflictMsg = "CLI -o rejects tentative array merges that disagree on inner extents across multiple inputs"

outputFileMultiInputTentativeArrayUseSiteMsg :: T.Text
outputFileMultiInputTentativeArrayUseSiteMsg = "CLI -o rejects sizeof on tentative arrays before cross-input completion"

outputFileMultiInputTentativeArrayAddressUseSiteMsg :: T.Text
outputFileMultiInputTentativeArrayAddressUseSiteMsg = "CLI -o rejects address-based pointer arithmetic on tentative arrays before cross-input completion"

outputFileMultiInputTentativeArrayInitializerRetypeMsg :: T.Text
outputFileMultiInputTentativeArrayInitializerRetypeMsg = "CLI -o rejects global initializers that depend on later cross-input tentative-array completion"

outputFileMultiInputStaticMsg :: T.Text
outputFileMultiInputStaticMsg = "CLI -o namespaces internal-linkage symbols across multiple inputs"

outputFileMultiInputStaticFunctionMsg :: T.Text
outputFileMultiInputStaticFunctionMsg = "CLI -o namespaces internal-linkage functions across multiple inputs"

outputFileMultiInputStaticFunctionPointerMsg :: T.Text
outputFileMultiInputStaticFunctionPointerMsg = "CLI -o namespaces internal-linkage function designators across multiple inputs"

outputFilePreservesExistingModeMsg :: T.Text
outputFilePreservesExistingModeMsg = "CLI -o preserves the existing output file mode when replacing it"

outputFileClearsSpecialBitsMsg :: T.Text
outputFileClearsSpecialBitsMsg = "CLI -o clears setuid/setgid/sticky bits when replacing an existing output"

outputFileFollowsSymlinkTargetMsg :: T.Text
outputFileFollowsSymlinkTargetMsg = "CLI -o updates symlink targets without replacing the symlink itself"

outputFileSpecialPathDevNullMsg :: T.Text
outputFileSpecialPathDevNullMsg = "CLI -o writes directly to special output paths such as /dev/null"

outputFileSamePathMsg :: T.Text
outputFileSamePathMsg = "CLI -o rejects same-path input/output aliases before overwriting source files"

outputFileMissingInputMatchingOutputPathMsg :: T.Text
outputFileMissingInputMatchingOutputPathMsg = "CLI -o reports missing same-path inputs instead of misclassifying them as output collisions"

outputFileHardLinkAliasMsg :: T.Text
outputFileHardLinkAliasMsg = "CLI -o rejects hard-linked input/output aliases before overwriting source files"

outputFileParseFailurePreservesExistingOutputMsg :: T.Text
outputFileParseFailurePreservesExistingOutputMsg = "CLI -o preserves existing outputs when parsing fails before opening the destination"

outputFileReadFailurePreservesExistingOutputMsg :: T.Text
outputFileReadFailurePreservesExistingOutputMsg = "CLI -o preserves existing outputs when reading an input fails before opening the destination"

outputFileOpenFailurePreservesExistingOutputMsg :: T.Text
outputFileOpenFailurePreservesExistingOutputMsg = "CLI -o preserves existing outputs when opening the destination fails"

outputFileReadOnlyParentWritableTargetMsg :: T.Text
outputFileReadOnlyParentWritableTargetMsg = "CLI -o falls back to in-place writes when an existing output is writable but its parent directory is not"

outputFileReadOnlyParentWriteOnlyTargetMsg :: T.Text
outputFileReadOnlyParentWriteOnlyTargetMsg = "CLI -o falls back to in-place writes when an existing write-only output has a read-only parent directory"

outputFileHardLinkedRenameReplacementPreservesAliasMsg :: T.Text
outputFileHardLinkedRenameReplacementPreservesAliasMsg = "CLI -o replaces hard-linked outputs via rename without rewriting sibling aliases"

outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputMsg :: T.Text
outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputMsg = "CLI -o refuses read-only-parent fallback writes that would overwrite hard-linked aliases"

outputFileReadOnlyParentWriteFailurePreservesExistingOutputMsg :: T.Text
outputFileReadOnlyParentWriteFailurePreservesExistingOutputMsg = "CLI -o preserves existing outputs when fallback writes fail under a read-only parent directory"

outputFileWriteFailurePreservesExistingOutputMsg :: T.Text
outputFileWriteFailurePreservesExistingOutputMsg = "CLI -o preserves existing outputs when asm emission fails after opening the replacement file"

outputFileFreshOutputRestrictiveUmaskMsg :: T.Text
outputFileFreshOutputRestrictiveUmaskMsg = "CLI -o creates fresh outputs under restrictive umasks and applies the final mode after writing"

runAsmMsg :: T.Text
runAsmMsg = "CLI -r keeps asm off stdout and still produces a runnable binary"

runAsmSingleInputImplicitFunctionConflictMsg :: T.Text
runAsmSingleInputImplicitFunctionConflictMsg = "CLI -r rejects single-input implicit-function/global collisions before invoking the assembler"

runAsmSpecialPathDevNullMsg :: T.Text
runAsmSpecialPathDevNullMsg = "CLI -r links directly to special output paths such as /dev/null"

runAsmSpecialPathDevNullSkipsPostLinkValidationWaitMsg :: T.Text
runAsmSpecialPathDevNullSkipsPostLinkValidationWaitMsg = "CLI -r still waits for delayed HTCC_ASSEMBLER linker children on special outputs such as /dev/null"

runAsmPreservesExecutableBitsMsg :: T.Text
runAsmPreservesExecutableBitsMsg = "CLI -r preserves execute bits when replacing a non-executable output"

runAsmPreservesExistingExecuteMaskMsg :: T.Text
runAsmPreservesExistingExecuteMaskMsg = "CLI -r preserves the existing execute mask when replacing a private executable"

runAsmRestoresOwnerExecuteBitMsg :: T.Text
runAsmRestoresOwnerExecuteBitMsg = "CLI -r restores owner execute when replacing outputs that were executable only for group/other"

runAsmFreshOutputInPlaceLinkDriverMsg :: T.Text
runAsmFreshOutputInPlaceLinkDriverMsg = "CLI -r preserves execute bits for fresh outputs when the link driver rewrites -o in place"

runAsmProbePreservesPrecreatedOutputMsg :: T.Text
runAsmProbePreservesPrecreatedOutputMsg = "CLI -r probes link drivers against a pre-created output file"

runAsmClearsSpecialBitsMsg :: T.Text
runAsmClearsSpecialBitsMsg = "CLI -r clears setuid/setgid/sticky bits when replacing an existing output"

runAsmLinkUsesResolvedDriverMsg :: T.Text
runAsmLinkUsesResolvedDriverMsg = "CLI -r reuses the resolved HTCC_ASSEMBLER driver when linking"

runAsmBareLocalAssemblerPathMsg :: T.Text
runAsmBareLocalAssemblerPathMsg = "CLI -r prefers PATH bare HTCC_ASSEMBLER drivers over ./<name>"

runAsmQuotedCompilerMsg :: T.Text
runAsmQuotedCompilerMsg = "CLI -r quotes assembler paths selected from HTCC_ASSEMBLER"

runAsmWrappedAssemblerMsg :: T.Text
runAsmWrappedAssemblerMsg = "CLI -r preserves wrapper args selected from HTCC_ASSEMBLER"

runAsmWrappedAssemblerFirstWordDriverMsg :: T.Text
runAsmWrappedAssemblerFirstWordDriverMsg = "CLI -r resolves only the first HTCC_ASSEMBLER shell word as the driver executable"

runAsmWrappedAssemblerProbeFallbackMsg :: T.Text
runAsmWrappedAssemblerProbeFallbackMsg = "CLI -r accepts wrapper commands whose probe flags fail when assemble/link forwarding still works"

runAsmWrappedAssemblerHostMetadataFallbackMsg :: T.Text
runAsmWrappedAssemblerHostMetadataFallbackMsg = "CLI -r accepts wrapped drivers whose metadata probes report the host target while assemble/link forwarding stays x86_64-ELF"

runAsmWrappedAssemblerDelayedChildHostMetadataMsg :: T.Text
runAsmWrappedAssemblerDelayedChildHostMetadataMsg = "CLI -r preserves delayed HTCC_ASSEMBLER probe metadata emitted by a wrapper child after the wrapper exits"

runAsmWrappedAssemblerDelayedChildFinalOutputMsg :: T.Text
runAsmWrappedAssemblerDelayedChildFinalOutputMsg = "CLI -r waits for delayed HTCC_ASSEMBLER wrapper children before validating the final output"

runAsmDoesNotWaitOnBackgroundProcessGroupHelpersMsg :: T.Text
runAsmDoesNotWaitOnBackgroundProcessGroupHelpersMsg = "CLI -r does not wait for unrelated HTCC_ASSEMBLER background helpers that stay in the wrapper process group"

runAsmProbeDoesNotHangOnInheritedPipeHandlesMsg :: T.Text
runAsmProbeDoesNotHangOnInheritedPipeHandlesMsg = "CLI -r returns even when HTCC_ASSEMBLER probe wrappers leave inherited pipe handles open after exit"

runAsmProbeDoesNotHangOnEarlyClosedStdoutMsg :: T.Text
runAsmProbeDoesNotHangOnEarlyClosedStdoutMsg = "CLI -r keeps HTCC_ASSEMBLER probes draining stderr after wrappers close stdout early"

runAsmTildeExpandedAssemblerPathMsg :: T.Text
runAsmTildeExpandedAssemblerPathMsg = "CLI -r expands leading ~ in HTCC_ASSEMBLER executable words on POSIX hosts"

runAsmLeadingEnvAssignmentMsg :: T.Text
runAsmLeadingEnvAssignmentMsg = "CLI -r applies leading PATH env assignments in HTCC_ASSEMBLER before resolving the driver"

runAsmLeadingEnvAssignmentExpandsTildePathMsg :: T.Text
runAsmLeadingEnvAssignmentExpandsTildePathMsg = "CLI -r expands leading ~ inside HTCC_ASSEMBLER PATH overrides on POSIX hosts"

runAsmExpandedAssignmentWordFailsMsg :: T.Text
runAsmExpandedAssignmentWordFailsMsg = "CLI -r does not reinterpret assignment-like fields produced by HTCC_ASSEMBLER expansion as env overrides"

runAsmQuotedAssignmentLikeExecutableMsg :: T.Text
runAsmQuotedAssignmentLikeExecutableMsg = "CLI -r treats quoted assignment-looking HTCC_ASSEMBLER executables as literal argv[0] values"

runAsmEscapedAssignmentLikeExecutableMsg :: T.Text
runAsmEscapedAssignmentLikeExecutableMsg = "CLI -r treats escaped assignment-looking HTCC_ASSEMBLER executables as literal argv[0] values"

runAsmLeadingEnvAssignmentWithoutEnvPathMsg :: T.Text
runAsmLeadingEnvAssignmentWithoutEnvPathMsg = "CLI -r executes PATH-assigned HTCC_ASSEMBLER drivers without depending on env in the parent PATH"

runAsmLeadingEnvAssignmentPreservesPathOverrideMsg :: T.Text
runAsmLeadingEnvAssignmentPreservesPathOverrideMsg = "CLI -r preserves leading PATH env assignments in HTCC_ASSEMBLER exactly during invocation"

runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangMsg :: T.Text
runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangMsg = "CLI -r keeps literal %...% and !...! env override values unchanged on POSIX hosts"

runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionMsg :: T.Text
runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionMsg = "CLI -r performs only a single round of POSIX env expansion for HTCC_ASSEMBLER overrides"

runAsmLeadingEnvAssignmentPreservesInheritedPwdMsg :: T.Text
runAsmLeadingEnvAssignmentPreservesInheritedPwdMsg = "CLI -r preserves caller-provided PWD during HTCC_ASSEMBLER expansion and invocation"

runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralMsg :: T.Text
runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralMsg = "CLI -r keeps escaped POSIX variable references literal in HTCC_ASSEMBLER env overrides"

runAsmEnvPathOverrideEmptyEntryMsg :: T.Text
runAsmEnvPathOverrideEmptyEntryMsg = "CLI -r keeps empty PATH entries in HTCC_ASSEMBLER overrides when resolving bare drivers"

runAsmEnvPathOverrideNoLocalFallbackMsg :: T.Text
runAsmEnvPathOverrideNoLocalFallbackMsg = "CLI -r does not fall back to ./<name> when HTCC_ASSEMBLER overrides PATH"

runAsmQuotedBackslashArgMsg :: T.Text
runAsmQuotedBackslashArgMsg = "CLI -r preserves backslashes in quoted HTCC_ASSEMBLER args"

runAsmIgnoresCcMsg :: T.Text
runAsmIgnoresCcMsg = "CLI -r ignores inherited CC and falls back to gcc"

runAsmGccPrefersPathMsg :: T.Text
runAsmGccPrefersPathMsg = "CLI -r prefers PATH gcc over ./gcc when HTCC_ASSEMBLER is unset"

runAsmFailurePreservesExistingOutputMsg :: T.Text
runAsmFailurePreservesExistingOutputMsg = "CLI -r preserves existing outputs when the assembler command fails before linking"

runAsmParseFailurePreservesExistingOutputMsg :: T.Text
runAsmParseFailurePreservesExistingOutputMsg = "CLI -r preserves existing outputs when parsing fails before assembler invocation"

runAsmReadFailurePreservesExistingOutputMsg :: T.Text
runAsmReadFailurePreservesExistingOutputMsg = "CLI -r preserves existing outputs when reading an input fails before assembler invocation"

runAsmReadOnlyParentWritableTargetMsg :: T.Text
runAsmReadOnlyParentWritableTargetMsg = "CLI -r falls back to in-place linking when an existing output is writable but its parent directory is not"

runAsmReadOnlyParentWriteOnlyTargetMsg :: T.Text
runAsmReadOnlyParentWriteOnlyTargetMsg = "CLI -r falls back to in-place linking when an existing write-only output has a read-only parent directory"

runAsmReadOnlyParentExecutableOnlyTargetMsg :: T.Text
runAsmReadOnlyParentExecutableOnlyTargetMsg = "CLI -r falls back to in-place linking when an existing executable-only output has a read-only parent directory"

runAsmHardLinkedRenameReplacementPreservesAliasMsg :: T.Text
runAsmHardLinkedRenameReplacementPreservesAliasMsg = "CLI -r replaces hard-linked outputs via rename without rewriting sibling aliases"

runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputMsg :: T.Text
runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputMsg = "CLI -r refuses read-only-parent fallback linking that would overwrite hard-linked aliases"

runAsmReadOnlyParentLinkFailurePreservesExistingOutputMsg :: T.Text
runAsmReadOnlyParentLinkFailurePreservesExistingOutputMsg = "CLI -r preserves existing outputs when fallback linking fails under a read-only parent directory"

runAsmFailurePreservesInputOutputAliasMsg :: T.Text
runAsmFailurePreservesInputOutputAliasMsg = "CLI -r rejects input/output aliasing before invoking the assembler and preserves input files"

runAsmMissingInputMatchingOutputPathMsg :: T.Text
runAsmMissingInputMatchingOutputPathMsg = "CLI -r reports missing same-path inputs instead of misclassifying them as output collisions"

runAsmFailurePreservesHardLinkInputOutputAliasMsg :: T.Text
runAsmFailurePreservesHardLinkInputOutputAliasMsg = "CLI -r rejects hard-linked input/output aliases before invoking the assembler"

runAsmMalformedAssemblerPreservesExistingOutputMsg :: T.Text
runAsmMalformedAssemblerPreservesExistingOutputMsg = "CLI -r preserves existing outputs when HTCC_ASSEMBLER is malformed"

runAsmMalformedAssemblerMsg :: T.Text
runAsmMalformedAssemblerMsg = "CLI -r does not leak temp asm files when HTCC_ASSEMBLER is malformed"

runAsmAcceptsFreeBsdElfTargetDriverMsg :: T.Text
runAsmAcceptsFreeBsdElfTargetDriverMsg = "CLI -r accepts x86_64 FreeBSD targets reported by HTCC_ASSEMBLER drivers"

runAsmRejectsMissingAssemblerDriverMsg :: T.Text
runAsmRejectsMissingAssemblerDriverMsg = "CLI -r reports a user-facing error when HTCC_ASSEMBLER names a missing command"

runAsmRejectsNonExecutableAssemblerDriverMsg :: T.Text
runAsmRejectsNonExecutableAssemblerDriverMsg = "CLI -r reports a user-facing error when HTCC_ASSEMBLER names a non-executable command"

runAsmRejectsAssemblerWithoutLinkDriverMsg :: T.Text
runAsmRejectsAssemblerWithoutLinkDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands that can assemble but cannot link"

runAsmRejectsSingleObjectOnlyLinkDriverMsg :: T.Text
runAsmRejectsSingleObjectOnlyLinkDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link support only works for single-object inputs"

runAsmRejectsScriptLinkProbeDriverMsg :: T.Text
runAsmRejectsScriptLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe only emits executable scripts"

runAsmRejectsSharedLinkProbeDriverMsg :: T.Text
runAsmRejectsSharedLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe emits ET_DYN shared-library outputs"

runAsmRejectsForeignAbiLinkProbeDriverMsg :: T.Text
runAsmRejectsForeignAbiLinkProbeDriverMsg = "CLI -r accepts HTCC_ASSEMBLER commands whose x86_64-ELF link probe emits a foreign ELF ABI"

runAsmRejectsBlobLinkProbeDriverMsg :: T.Text
runAsmRejectsBlobLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe copies an unrelated x86_64 ELF blob"

runAsmRejectsSymlinkLinkProbeDriverMsg :: T.Text
runAsmRejectsSymlinkLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe leaves the requested output as a symlink"

runAsmRejectsSymlinkSpecialLinkProbeDriverMsg :: T.Text
runAsmRejectsSymlinkSpecialLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe leaves the requested output as a symlink to a non-regular special file"

runAsmRejectsNonExecutableLinkProbeDriverMsg :: T.Text
runAsmRejectsNonExecutableLinkProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe emits a structurally valid ELF without execute bits"

runAsmDoesNotInjectValidationMarkerIntoFinalAsmMsg :: T.Text
runAsmDoesNotInjectValidationMarkerIntoFinalAsmMsg = "CLI -r keeps validation markers out of the final assembly it passes to HTCC_ASSEMBLER"

runAsmAcceptsUserDefinedValidationMarkerSymbolMsg :: T.Text
runAsmAcceptsUserDefinedValidationMarkerSymbolMsg = "CLI -r accepts user globals named htcc_runnable_output_marker"

runAsmAcceptsGcSectionsLinkerMsg :: T.Text
runAsmAcceptsGcSectionsLinkerMsg = "CLI -r keeps the validation marker reachable when HTCC_ASSEMBLER enables linker section garbage collection"

runAsmRejectsMarkerlessFinalOutputMsg :: T.Text
runAsmRejectsMarkerlessFinalOutputMsg = "CLI -r rejects structurally valid final outputs that do not carry the authenticated link marker"

runAsmRejectsBogusFinalLinkOutputMsg :: T.Text
runAsmRejectsBogusFinalLinkOutputMsg = "CLI -r revalidates the final linked output even when the HTCC_ASSEMBLER probe succeeded"

runAsmRejectsNamedPipeFinalLinkOutputMsg :: T.Text
runAsmRejectsNamedPipeFinalLinkOutputMsg = "CLI -r rejects HTCC_ASSEMBLER commands that replace the staged final output with a FIFO"

runAsmRejectsExecutableObjectProbeDriverMsg :: T.Text
runAsmRejectsExecutableObjectProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose assembly probe emits non-relocatable x86_64 ELF files"

runAsmRejectsSymlinkObjectProbeDriverMsg :: T.Text
runAsmRejectsSymlinkObjectProbeDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose assembly probe emits the object file as a symlink"

runAsmRejectsTouchingLinkDriverMsg :: T.Text
runAsmRejectsTouchingLinkDriverMsg = "CLI -r rejects HTCC_ASSEMBLER commands whose link probe only touches the requested output"

runAsmRejectsIncompatibleTargetDriverMsg :: T.Text
runAsmRejectsIncompatibleTargetDriverMsg = "CLI -r rejects HTCC_ASSEMBLER drivers that do not target x86_64-ELF"

runAsmRejectsMetadataSpoofingDriverMsg :: T.Text
runAsmRejectsMetadataSpoofingDriverMsg = "CLI -r rejects HTCC_ASSEMBLER drivers whose metadata target is x86_64-ELF but whose effective assembly probe is not"

runAsmRejectsWrappedNonElfDriverMsg :: T.Text
runAsmRejectsWrappedNonElfDriverMsg = "CLI -r rejects probe-hiding HTCC_ASSEMBLER wrappers around non-ELF x86_64 drivers"

runAsmFreshOutputRestrictiveUmaskMsg :: T.Text
runAsmFreshOutputRestrictiveUmaskMsg = "CLI -r creates fresh outputs under restrictive umasks and applies the final mode after linking"

fakeAssemblerPath :: FilePath
fakeAssemblerPath = "tmp-assembler.sh"

fakeDriverPath :: FilePath
fakeDriverPath = "tmp-driver.sh"

fakeCombinedDriverPath :: FilePath
fakeCombinedDriverPath = fakeAssemblerPath <> " " <> fakeDriverPath

fakeProbeWrapperPath :: FilePath
fakeProbeWrapperPath = "tmp-probe-wrapper.sh"

fakeHostMetadataWrapperPath :: FilePath
fakeHostMetadataWrapperPath = "tmp-host-metadata-wrapper.sh"

fakeWarningWrapperPath :: FilePath
fakeWarningWrapperPath = "tmp-warning-wrapper.sh"

fakeStdoutWarningWrapperPath :: FilePath
fakeStdoutWarningWrapperPath = "tmp-stdout-warning-wrapper.sh"

fakeBinaryWarningWrapperPath :: FilePath
fakeBinaryWarningWrapperPath = "tmp-binary-warning-wrapper.sh"

fakeUnterminatedStderrWrapperPath :: FilePath
fakeUnterminatedStderrWrapperPath = "tmp-unterminated-stderr-wrapper.sh"

fakeInterleavedOutputWrapperPath :: FilePath
fakeInterleavedOutputWrapperPath = "tmp-interleaved-output-wrapper.sh"

fakeSameReadStdoutInterleavingWrapperPath :: FilePath
fakeSameReadStdoutInterleavingWrapperPath = "tmp-same-read-stdout-interleaving-wrapper.sh"

fakeSameReadStderrChunksWrapperPath :: FilePath
fakeSameReadStderrChunksWrapperPath = "tmp-same-read-stderr-chunks-wrapper.sh"

fakeStreamScopedWarningWrapperPath :: FilePath
fakeStreamScopedWarningWrapperPath = "tmp-stream-scoped-warning-wrapper.sh"

fakeSplitWarningInterleavedStdoutWrapperPath :: FilePath
fakeSplitWarningInterleavedStdoutWrapperPath = "tmp-split-warning-interleaved-stdout-wrapper.sh"

fakeSplitWarningPrefixWrapperPath :: FilePath
fakeSplitWarningPrefixWrapperPath = "tmp-split-warning-prefix-wrapper.sh"

fakeCrossStreamWarningPreambleWrapperPath :: FilePath
fakeCrossStreamWarningPreambleWrapperPath = "tmp-cross-stream-warning-preamble-wrapper.sh"

fakeStdoutPrefixBeforeStderrWrapperPath :: FilePath
fakeStdoutPrefixBeforeStderrWrapperPath = "tmp-stdout-prefix-before-stderr-wrapper.sh"

fakeStderrPrefixBeforeStdoutWrapperPath :: FilePath
fakeStderrPrefixBeforeStdoutWrapperPath = "tmp-stderr-prefix-before-stdout-wrapper.sh"

fakeWarningPreambleWrapperPath :: FilePath
fakeWarningPreambleWrapperPath = "tmp-warning-preamble-wrapper.sh"

fakeAnsiWarningPreambleWrapperPath :: FilePath
fakeAnsiWarningPreambleWrapperPath = "tmp-ansi-warning-preamble-wrapper.sh"

fakeWarningLeadingNoteWrapperPath :: FilePath
fakeWarningLeadingNoteWrapperPath = "tmp-warning-leading-note-wrapper.sh"

fakeWarningContextWrapperPath :: FilePath
fakeWarningContextWrapperPath = "tmp-warning-context-wrapper.sh"

fakeWarningSnippetWrapperPath :: FilePath
fakeWarningSnippetWrapperPath = "tmp-warning-snippet-wrapper.sh"

fakeMultiLineWarningSnippetWrapperPath :: FilePath
fakeMultiLineWarningSnippetWrapperPath = "tmp-multiline-warning-snippet-wrapper.sh"

fakeSnippetOnlyWarningWrapperPath :: FilePath
fakeSnippetOnlyWarningWrapperPath = "tmp-warning-snippet-only-wrapper.sh"

fakeCrLfWarningWrapperPath :: FilePath
fakeCrLfWarningWrapperPath = "tmp-crlf-warning-wrapper.sh"

fakeStandaloneLeadingNoteWrapperPath :: FilePath
fakeStandaloneLeadingNoteWrapperPath = "tmp-standalone-leading-note-wrapper.sh"

fakeIndentedPostWarningStderrWrapperPath :: FilePath
fakeIndentedPostWarningStderrWrapperPath = "tmp-indented-post-warning-wrapper.sh"

fakePunctuatedPostWarningStderrWrapperPath :: FilePath
fakePunctuatedPostWarningStderrWrapperPath = "tmp-punctuated-post-warning-wrapper.sh"

fakeDirectivePostWarningStderrWrapperPath :: FilePath
fakeDirectivePostWarningStderrWrapperPath = "tmp-directive-post-warning-wrapper.sh"

fakeStandalonePreSummaryNoteWrapperPath :: FilePath
fakeStandalonePreSummaryNoteWrapperPath = "tmp-standalone-pre-summary-note-wrapper.sh"

fakePostWarningNoteWrapperPath :: FilePath
fakePostWarningNoteWrapperPath = "tmp-post-warning-note-wrapper.sh"

fakePostWarningNoteWithoutSummaryWrapperPath :: FilePath
fakePostWarningNoteWithoutSummaryWrapperPath = "tmp-post-warning-note-without-summary-wrapper.sh"

fakeFailingWarningWrapperPath :: FilePath
fakeFailingWarningWrapperPath = "tmp-failing-warning-wrapper.sh"

fakeErrorPreambleAfterWarningWrapperPath :: FilePath
fakeErrorPreambleAfterWarningWrapperPath = "tmp-error-preamble-after-warning-wrapper.sh"

fakeErrorSnippetWarningTokenWrapperPath :: FilePath
fakeErrorSnippetWarningTokenWrapperPath = "tmp-error-snippet-warning-token-wrapper.sh"

fakeWarningLabelErrorSnippetWrapperPath :: FilePath
fakeWarningLabelErrorSnippetWrapperPath = "tmp-warning-label-error-snippet-wrapper.sh"

fakeInheritedPipeHandlesWrapperPath :: FilePath
fakeInheritedPipeHandlesWrapperPath = "tmp-inherited-pipe-handles-wrapper.sh"

fakeProbeInheritedPipeHandlesWrapperPath :: FilePath
fakeProbeInheritedPipeHandlesWrapperPath = "tmp-probe-inherited-pipe-handles-wrapper.sh"

fakeEarlyClosedStdoutProbeWrapperPath :: FilePath
fakeEarlyClosedStdoutProbeWrapperPath = "tmp-early-closed-stdout-probe-wrapper.sh"

fakeDelayedChildHostMetadataWrapperPath :: FilePath
fakeDelayedChildHostMetadataWrapperPath = "tmp-delayed-child-host-metadata-wrapper.sh"

fakeDelayedChildFinalOutputWrapperPath :: FilePath
fakeDelayedChildFinalOutputWrapperPath = "tmp-delayed-child-final-output-wrapper.sh"

fakeBackgroundProcessGroupHelperWrapperPath :: FilePath
fakeBackgroundProcessGroupHelperWrapperPath = "tmp-background-process-group-helper-wrapper.sh"

fakeDelayedChildFinalLinkOnlyWrapperPath :: FilePath
fakeDelayedChildFinalLinkOnlyWrapperPath = "tmp-delayed-child-final-link-only-wrapper.sh"

fakeStreamingPromptWrapperPath :: FilePath
fakeStreamingPromptWrapperPath = "tmp-streaming-prompt-wrapper.sh"

fakeStreamingPromptWithoutNewlineWrapperPath :: FilePath
fakeStreamingPromptWithoutNewlineWrapperPath = "tmp-streaming-prompt-without-newline-wrapper.sh"

fakeCrossStreamStreamingRetainedLineWrapperPath :: FilePath
fakeCrossStreamStreamingRetainedLineWrapperPath = "tmp-cross-stream-streaming-retained-line-wrapper.sh"

fakeProbeCrossStreamWarningPreambleWrapperPath :: FilePath
fakeProbeCrossStreamWarningPreambleWrapperPath = "tmp-probe-cross-stream-warning-preamble-wrapper.sh"

fakeProbePartialCrossStreamWarningPreambleWrapperPath :: FilePath
fakeProbePartialCrossStreamWarningPreambleWrapperPath = "tmp-probe-partial-cross-stream-warning-preamble-wrapper.sh"

fakeProbeReadsStdinWrapperPath :: FilePath
fakeProbeReadsStdinWrapperPath = "tmp-probe-reads-stdin-wrapper.sh"

fakeRealInvocationReadsStdinWrapperPath :: FilePath
fakeRealInvocationReadsStdinWrapperPath = "tmp-real-invocation-reads-stdin-wrapper.sh"

fakeAssemblerQuotedPath :: FilePath
fakeAssemblerQuotedPath = "tmp assembler.sh"

fakeAssemblerLogPath :: FilePath
fakeAssemblerLogPath = "tmp-assembler.log"

fakeDriverLogPath :: FilePath
fakeDriverLogPath = "tmp-driver.log"

fakeCombinedDriverLogPath :: FilePath
fakeCombinedDriverLogPath = "tmp-combined-driver.log"

fakeProbeWrapperLogPath :: FilePath
fakeProbeWrapperLogPath = "tmp-probe-wrapper.log"

fakeHostMetadataWrapperLogPath :: FilePath
fakeHostMetadataWrapperLogPath = "tmp-host-metadata-wrapper.log"

fakeDelayedChildHostMetadataWrapperLogPath :: FilePath
fakeDelayedChildHostMetadataWrapperLogPath = "tmp-delayed-child-host-metadata-wrapper.log"

fakeProbeInheritedPipeHandlesWrapperLogPath :: FilePath
fakeProbeInheritedPipeHandlesWrapperLogPath = "tmp-probe-inherited-pipe-handles-wrapper.log"

fakeBackgroundProcessGroupHelperPidPath :: FilePath
fakeBackgroundProcessGroupHelperPidPath = "tmp-background-process-group-helper-wrapper.pids"

fakeEarlyClosedStdoutProbeWrapperLogPath :: FilePath
fakeEarlyClosedStdoutProbeWrapperLogPath = "tmp-early-closed-stdout-probe-wrapper.log"

fakeProbeReadsStdinWrapperLogPath :: FilePath
fakeProbeReadsStdinWrapperLogPath = "tmp-probe-reads-stdin-wrapper.log"

fakeRealInvocationReadsStdinWrapperLogPath :: FilePath
fakeRealInvocationReadsStdinWrapperLogPath = "tmp-real-invocation-reads-stdin-wrapper.log"

fakeAssemblerAsmPath :: FilePath
fakeAssemblerAsmPath = "tmp-assembler.s"

fakeProbeHostAssemblerPath :: FilePath
fakeProbeHostAssemblerPath = "tmp-probe-host-assembler.sh"

fakeProbeHostAssemblerLogPath :: FilePath
fakeProbeHostAssemblerLogPath = "tmp-probe-host-assembler.log"

fakeProbeHostAssemblerAsmPath :: FilePath
fakeProbeHostAssemblerAsmPath = "tmp-probe-host-assembler.s"

fakeProbeHostWrapperPath :: FilePath
fakeProbeHostWrapperPath = "tmp-probe-host-wrapper.sh"

fakeProbeHostWrapperLogPath :: FilePath
fakeProbeHostWrapperLogPath = "tmp-probe-host-wrapper.log"

fakeAssemblerWrapperArg :: T.Text
fakeAssemblerWrapperArg = "--wrapper-flag"

fakeAssemblerBackslashArg :: T.Text
fakeAssemblerBackslashArg = "a\\nb"

fakeGccPath :: FilePath
fakeGccPath = "gcc"

fakeBadCcPath :: FilePath
fakeBadCcPath = "tmp-bad-cc.sh"

fakeBadCcLogPath :: FilePath
fakeBadCcLogPath = "tmp-bad-cc.log"

fakeHostDriverPath :: FilePath
fakeHostDriverPath = "tmp-host-driver.sh"

fakeHostDriverLogPath :: FilePath
fakeHostDriverLogPath = "tmp-host-driver.log"

fakeSpoofedTargetDriverPath :: FilePath
fakeSpoofedTargetDriverPath = "tmp-spoofed-target-driver.sh"

fakeSpoofedTargetDriverLogPath :: FilePath
fakeSpoofedTargetDriverLogPath = "tmp-spoofed-target-driver.log"

fakeFreeBsdDriverPath :: FilePath
fakeFreeBsdDriverPath = "tmp-freebsd-driver.sh"

fakeFreeBsdDriverLogPath :: FilePath
fakeFreeBsdDriverLogPath = "tmp-freebsd-driver.log"

fakeFreeBsdDriverAsmPath :: FilePath
fakeFreeBsdDriverAsmPath = "tmp-freebsd-driver.s"

fakeMissingAssemblerDriverPath :: FilePath
fakeMissingAssemblerDriverPath = "tmp-missing-assembler-driver"

fakeNonExecutableAssemblerDriverPath :: FilePath
fakeNonExecutableAssemblerDriverPath = "tmp-non-executable-assembler-driver.sh"

fakeAssembleOnlyDriverPath :: FilePath
fakeAssembleOnlyDriverPath = "tmp-assemble-only-driver.sh"

fakeAssembleOnlyDriverLogPath :: FilePath
fakeAssembleOnlyDriverLogPath = "tmp-assemble-only-driver.log"

fakeSingleObjectOnlyLinkDriverPath :: FilePath
fakeSingleObjectOnlyLinkDriverPath = "tmp-single-object-only-link-driver.sh"

fakeSingleObjectOnlyLinkDriverLogPath :: FilePath
fakeSingleObjectOnlyLinkDriverLogPath = "tmp-single-object-only-link-driver.log"

fakeInPlaceLinkDriverPath :: FilePath
fakeInPlaceLinkDriverPath = "tmp-in-place-link-driver.sh"

fakeInPlaceLinkDriverLogPath :: FilePath
fakeInPlaceLinkDriverLogPath = "tmp-in-place-link-driver.log"

fakeProbePrecreatedOutputDriverPath :: FilePath
fakeProbePrecreatedOutputDriverPath = "tmp-probe-precreated-output-driver.sh"

fakeProbePrecreatedOutputDriverLogPath :: FilePath
fakeProbePrecreatedOutputDriverLogPath = "tmp-probe-precreated-output-driver.log"

fakeTouchingLinkDriverPath :: FilePath
fakeTouchingLinkDriverPath = "tmp-touching-link-driver.sh"

fakeTouchingLinkDriverLogPath :: FilePath
fakeTouchingLinkDriverLogPath = "tmp-touching-link-driver.log"

fakeScriptLinkProbeDriverPath :: FilePath
fakeScriptLinkProbeDriverPath = "tmp-script-link-probe-driver.sh"

fakeScriptLinkProbeDriverLogPath :: FilePath
fakeScriptLinkProbeDriverLogPath = "tmp-script-link-probe-driver.log"

fakeSharedLinkProbeDriverPath :: FilePath
fakeSharedLinkProbeDriverPath = "tmp-shared-link-probe-driver.sh"

fakeSharedLinkProbeDriverLogPath :: FilePath
fakeSharedLinkProbeDriverLogPath = "tmp-shared-link-probe-driver.log"

fakeForeignAbiLinkProbeDriverPath :: FilePath
fakeForeignAbiLinkProbeDriverPath = "tmp-foreign-abi-link-probe-driver.sh"

fakeForeignAbiLinkProbeDriverLogPath :: FilePath
fakeForeignAbiLinkProbeDriverLogPath = "tmp-foreign-abi-link-probe-driver.log"

fakeBlobLinkProbeDriverPath :: FilePath
fakeBlobLinkProbeDriverPath = "tmp-blob-link-probe-driver.sh"

fakeBlobLinkProbeDriverLogPath :: FilePath
fakeBlobLinkProbeDriverLogPath = "tmp-blob-link-probe-driver.log"

fakeSymlinkLinkProbeDriverPath :: FilePath
fakeSymlinkLinkProbeDriverPath = "tmp-symlink-link-probe-driver.sh"

fakeSymlinkLinkProbeDriverLogPath :: FilePath
fakeSymlinkLinkProbeDriverLogPath = "tmp-symlink-link-probe-driver.log"

fakeSymlinkLinkProbeTargetPath :: FilePath
fakeSymlinkLinkProbeTargetPath = "tmp-symlink-link-probe-target.bin"

fakeSymlinkSpecialLinkProbeDriverPath :: FilePath
fakeSymlinkSpecialLinkProbeDriverPath = "tmp-symlink-special-link-probe-driver.sh"

fakeSymlinkSpecialLinkProbeDriverLogPath :: FilePath
fakeSymlinkSpecialLinkProbeDriverLogPath = "tmp-symlink-special-link-probe-driver.log"

fakeNonExecutableLinkProbeDriverPath :: FilePath
fakeNonExecutableLinkProbeDriverPath = "tmp-non-executable-link-probe-driver.sh"

fakeNonExecutableLinkProbeDriverLogPath :: FilePath
fakeNonExecutableLinkProbeDriverLogPath = "tmp-non-executable-link-probe-driver.log"

fakeNonExecutableLinkProbeObjectPath :: FilePath
fakeNonExecutableLinkProbeObjectPath = "tmp-non-executable-link-probe.o"

fakeNonExecutableLinkProbeOutputPath :: FilePath
fakeNonExecutableLinkProbeOutputPath = "tmp-non-executable-link-probe.out"

fakeBogusFinalLinkDriverPath :: FilePath
fakeBogusFinalLinkDriverPath = "tmp-bogus-final-link-driver.sh"

fakeBogusFinalLinkDriverLogPath :: FilePath
fakeBogusFinalLinkDriverLogPath = "tmp-bogus-final-link-driver.log"

fakeBogusFinalLinkTargetPath :: FilePath
fakeBogusFinalLinkTargetPath = "tmp-bogus-final-link-target.bin"

fakeMarkerStrippingFinalLinkDriverPath :: FilePath
fakeMarkerStrippingFinalLinkDriverPath = "tmp-marker-stripping-final-link-driver.sh"

fakeMarkerStrippingFinalLinkDriverLogPath :: FilePath
fakeMarkerStrippingFinalLinkDriverLogPath = "tmp-marker-stripping-final-link-driver.log"

fakeGcSectionsLinkDriverPath :: FilePath
fakeGcSectionsLinkDriverPath = "tmp-gc-sections-link-driver.sh"

fakeGcSectionsLinkDriverLogPath :: FilePath
fakeGcSectionsLinkDriverLogPath = "tmp-gc-sections-link-driver.log"

fakeNamedPipeFinalLinkDriverPath :: FilePath
fakeNamedPipeFinalLinkDriverPath = "tmp-named-pipe-final-link-driver.sh"

fakeNamedPipeFinalLinkDriverLogPath :: FilePath
fakeNamedPipeFinalLinkDriverLogPath = "tmp-named-pipe-final-link-driver.log"

fakeExecutableObjectProbeDriverPath :: FilePath
fakeExecutableObjectProbeDriverPath = "tmp-executable-object-probe-driver.sh"

fakeExecutableObjectProbeDriverLogPath :: FilePath
fakeExecutableObjectProbeDriverLogPath = "tmp-executable-object-probe-driver.log"

fakeSymlinkObjectProbeDriverPath :: FilePath
fakeSymlinkObjectProbeDriverPath = "tmp-symlink-object-probe-driver.sh"

fakeSymlinkObjectProbeDriverLogPath :: FilePath
fakeSymlinkObjectProbeDriverLogPath = "tmp-symlink-object-probe-driver.log"

fakeSymlinkObjectProbeTargetPath :: FilePath
fakeSymlinkObjectProbeTargetPath = "tmp-symlink-object-probe-target.o"

fakeFailingLinkDriverPath :: FilePath
fakeFailingLinkDriverPath = "tmp-failing-link-driver.sh"

fakeFailingLinkDriverLogPath :: FilePath
fakeFailingLinkDriverLogPath = "tmp-failing-link-driver.log"

fakePathBinDir :: FilePath
fakePathBinDir = "tmp-bin"

fakePathGccPath :: FilePath
fakePathGccPath = fakePathBinDir </> "gcc"

fakePathGccLogPath :: FilePath
fakePathGccLogPath = "tmp-path-gcc.log"

fakePathGccAsmPath :: FilePath
fakePathGccAsmPath = "tmp-path-gcc.s"

fakePathAssemblerPath :: FilePath
fakePathAssemblerPath = fakePathBinDir </> fakeAssemblerPath

fakePathAssemblerLogPath :: FilePath
fakePathAssemblerLogPath = "tmp-path-assembler.log"

fakePathWrapperLogPath :: FilePath
fakePathWrapperLogPath = "tmp-path-wrapper.log"

fakeEnvLoggingWrapperPath :: FilePath
fakeEnvLoggingWrapperPath = "tmp-env-logging-wrapper.sh"

fakeEnvLoggingWrapperLogPath :: FilePath
fakeEnvLoggingWrapperLogPath = "tmp-env-logging-wrapper.log"

fakeLocalAssemblerLogPath :: FilePath
fakeLocalAssemblerLogPath = "tmp-local-assembler.log"

fakeHostPathHelperPath :: FilePath
fakeHostPathHelperPath = "tmp-path-helper.sh"

fakeHostPathHelperLogPath :: FilePath
fakeHostPathHelperLogPath = "tmp-host-path-helper.log"

fakeHostPathHelperAsmPath :: FilePath
fakeHostPathHelperAsmPath = "tmp-host-path-helper.s"

fakeLocalGccLogPath :: FilePath
fakeLocalGccLogPath = "tmp-local-gcc.log"

fakeMalformedAssemblerTmpDir :: FilePath
fakeMalformedAssemblerTmpDir = "tmp-htcc-tmp"

specialFileModeMask :: FileMode
specialFileModeMask = foldr1 unionFileModes
    [ setUserIDMode
    , setGroupIDMode
    , 0o1000
    ]

permissionFileModeMask :: FileMode
permissionFileModeMask = foldr1 unionFileModes
    [ ownerReadMode
    , ownerWriteMode
    , ownerExecuteMode
    , groupReadMode
    , groupWriteMode
    , groupExecuteMode
    , otherReadMode
    , otherWriteMode
    , otherExecuteMode
    , specialFileModeMask
    ]

permissionBits :: FileMode -> FileMode
permissionBits = intersectFileModes permissionFileModeMask

runnableElfHex :: T.Text
runnableElfHex =
    "7f454c4602010100000000000000000002003e000100000078004000000000004000000000000000000000000000000000000000400038000100000000000000010000000500000000000000000000000000400000000000000040000000000079000000000000007900000000000000001000000000000090"

foreignElfOsAbiForHost :: Word8
foreignElfOsAbiForHost
    | os == "freebsd" = 3
    | otherwise = 9

setElfOsAbiForProbe :: Word8 -> B.ByteString -> B.ByteString
setElfOsAbiForProbe osAbi bytes =
    B.take 7 bytes <> B.singleton osAbi <> B.drop 8 bytes

byteStringToHex :: B.ByteString -> T.Text
byteStringToHex =
    T.pack . concatMap renderByte . B.unpack
    where
        renderByte byte =
            case showHex byte "" of
                [singleDigit] ->
                    ['0', singleDigit]
                hexDigits ->
                    hexDigits

foreignAbiRunnableElfHex :: T.Text
foreignAbiRunnableElfHex =
    byteStringToHex $
        setElfOsAbiForProbe foreignElfOsAbiForHost $
            linkedElfHeaderWithInterpreterForProbeBytes
                2
                (map (fromIntegral . fromEnum) "/lib64/ld-linux-x86-64.so.2" <> [0])

runnableElfWriter :: [T.Text]
runnableElfWriter =
    [ "write_runnable_elf() {"
    , "  perl -e 'print pack(\"H*\", $ARGV[0])' '" <> runnableElfHex <> "' > \"$1\""
    , "  chmod +x \"$1\""
    , "}"
    ]

foreignAbiRunnableElfWriter :: [T.Text]
foreignAbiRunnableElfWriter =
    [ "write_foreign_abi_runnable_elf() {"
    , "  perl -e 'print pack(\"H*\", $ARGV[0])' '" <> foreignAbiRunnableElfHex <> "' > \"$1\""
    , "  chmod +x \"$1\""
    , "}"
    ]

probeLinkedOutputWriter :: [T.Text]
probeLinkedOutputWriter =
    [ "      test -n \"$input\""
    , "      write_runnable_elf \"$out\""
    , "      cat \"$input\" >> \"$out\""
    ]

foreignAbiProbeLinkedOutputWriter :: [T.Text]
foreignAbiProbeLinkedOutputWriter =
    [ "      test -n \"$input\""
    , "      write_foreign_abi_runnable_elf \"$out\""
    , "      cat \"$input\" >> \"$out\""
    ]

writeFakeAssembler :: FilePath -> IO ()
writeFakeAssembler = writeFakeAssemblerWithLogs fakeAssemblerLogPath fakeAssemblerAsmPath

writeForwardingDriverWrapper :: FilePath -> IO ()
writeForwardingDriverWrapper wrapperPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "wrapped=$1"
        , "shift"
        , "exec ./\"$wrapped\" \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeLoggingDriver :: FilePath -> FilePath -> IO ()
writeLoggingDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if [ \"$arg\" = '-c' ]; then"
        , "    mode=assemble"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s:%s\\n' \"$mode\" \"$*\" >> " <> T.pack logPath
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
        , "      test -n \"$input\""
        , "      write_runnable_elf \"$out\""
        , "      cat \"$input\" >> \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSpecialOutputDriver :: FilePath -> IO ()
writeSpecialOutputDriver driverPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  if [ \"$out\" = '/dev/null' ]; then"
        , "    : > \"$out\""
        , "  else"
        , "    case \"$out\" in"
        , "      *htcc-probe-*)"
        ]
            <> map ("  " <>) probeLinkedOutputWriter
            <> [ "        ;;"
               , "      *)"
        , "        printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "        chmod +x \"$out\""
        , "        ;;"
        , "    esac"
        , "  fi"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeProbeRejectingWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeProbeRejectingWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      exit 64"
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeHostMetadataWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeHostMetadataWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'arm64-apple-darwin23.5.0'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeDelayedChildHostMetadataWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeDelayedChildHostMetadataWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      perl -e 'if (fork() == 0) { print qq(x86_64-); select undef, undef, undef, 0.6; print qq(linux-gnu\\n); exit 0 } exit 0'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeDelayedChildFinalOutputWrapper :: FilePath -> FilePath -> IO ()
writeDelayedChildFinalOutputWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "inputs=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    mode=assemble"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "    -x)"
        , "      expect_lang=true"
        , "      ;;"
        , "    -c)"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      inputs=\"$inputs $arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  ("
        , "    printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "    perl -e 'select undef, undef, undef, 0.6'"
        , "    cat \"$input\" >> \"$out\""
        , "  ) &"
        , "  exit 0"
        , "fi"
        , "for input_path in $inputs; do"
        , "  if [ \"$(wc -c < \"$input_path\")\" -le 20 ]; then"
        , "    exit 98"
        , "  fi"
        , "done"
        , "( perl -e 'select undef, undef, undef, 0.6'; exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\" ) &"
        , "exit 0"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeBackgroundProcessGroupHelperWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeBackgroundProcessGroupHelperWrapper wrapperPath pidPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "spawn_helper=false"
        , "expect_lang=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    case \"$arg\" in"
        , "      assembler)"
        , "        spawn_helper=true"
        , "        ;;"
        , "    esac"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    *htcc-probe-*)"
        , "      spawn_helper=true"
        , "      ;;"
        , "    -x)"
        , "      expect_lang=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "if $spawn_helper; then"
        , "  sh -c 'sleep 8' &"
        , "  printf '%s\\n' \"$!\" >> " <> T.pack pidPath
        , "fi"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeDelayedChildFinalLinkOnlyWrapper :: FilePath -> FilePath -> IO ()
writeDelayedChildFinalLinkOnlyWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*|-x)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "( perl -e 'select undef, undef, undef, 2.0'; exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\" ) &"
        , "exit 0"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeProbeReadsStdinWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeProbeReadsStdinWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      cat >/dev/null"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeRealInvocationReadsStdinWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeRealInvocationReadsStdinWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"args:$*\" >> " <> T.pack logPath
        , "prev=''"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "  if [ \"$prev\" = '-x' ] && [ \"$arg\" = 'assembler' ]; then"
        , "    if IFS= read -r token; then"
        , "      printf '%s\\n' \"stdin:$token\" >> " <> T.pack logPath
        , "    else"
        , "      printf '%s\\n' 'stdin:<eof>' >> " <> T.pack logPath
        , "    fi"
        , "    break"
        , "  fi"
        , "  prev=$arg"
        , "done"
        , "exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStdoutWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeStdoutWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning'"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note'"
        , "printf '%s\\n' '1 warning generated.'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeBinaryWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeBinaryWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '\\377\\n' >&2"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "printf '\\376'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeUnterminatedStderrDriverWrapper :: FilePath -> FilePath -> IO ()
writeUnterminatedStderrDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s' 'XYZ' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeInterleavedOutputDriverWrapper :: FilePath -> FilePath -> IO ()
writeInterleavedOutputDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "perl -e 'syswrite STDOUT, qq(stdout: wrapper start\\n); syswrite STDERR, qq(stderr: wrapper note\\n); syswrite STDOUT, qq(stdout: wrapper end\\n);'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeSameReadStdoutInterleavingDriverWrapper :: FilePath -> FilePath -> IO ()
writeSameReadStdoutInterleavingDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "perl -e 'syswrite STDOUT, qq(stdout: first\\n); syswrite STDERR, qq(stderr: middle\\n); syswrite STDOUT, qq(stdout: second\\n);'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeSameReadStderrChunksDriverWrapper :: FilePath -> FilePath -> IO ()
writeSameReadStderrChunksDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "perl -e 'syswrite STDERR, qq(stderr: first\\nstderr: second\\n); select undef, undef, undef, 0.1; syswrite STDOUT, qq(stdout: after\\n);'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStreamScopedWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeStreamScopedWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' 'stdout: retained first'"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stdout: retained second'"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeSplitWarningInterleavedStdoutDriverWrapper :: FilePath -> FilePath -> IO ()
writeSplitWarningInterleavedStdoutDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s' 'warn' >&2"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stdout: wrapper progress'"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'ing: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeSplitWarningPrefixDriverWrapper :: FilePath -> FilePath -> IO ()
writeSplitWarningPrefixDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s' '{standard input}:1:1: warn' >&2"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stdout: wrapper progress'"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'ing: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeCrossStreamWarningPreambleDriverWrapper :: FilePath -> FilePath -> IO ()
writeCrossStreamWarningPreambleDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' 'In file included from fake-header.h:1:'"
        , "printf '%s\\n' '                 from fake-source.c:2:'"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStdoutPrefixBeforeStderrDriverWrapper :: FilePath -> FilePath -> IO ()
writeStdoutPrefixBeforeStderrDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s' 'stdout: prefix'"
        , "printf '%s\\n' 'stderr: note' >&2"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stdout: suffix'"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStderrPrefixBeforeStdoutDriverWrapper :: FilePath -> FilePath -> IO ()
writeStderrPrefixBeforeStdoutDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s' 'stderr: prefix' >&2"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stdout: note'"
        , "perl -e 'select undef, undef, undef, 0.1'"
        , "printf '%s\\n' 'stderr: suffix' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningPreambleDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningPreambleDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'In file included from fake-header.h:1:' >&2"
        , "printf '%s\\n' '                 from fake-source.c:2:' >&2"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeAnsiWarningPreambleDriverWrapper :: FilePath -> FilePath -> IO ()
writeAnsiWarningPreambleDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '\\033[1;36mIn file included from fake-header.h:1:\\033[0m\\n' >&2"
        , "printf '\\033[1;36m                 from fake-source.c:2:\\033[0m\\n' >&2"
        , "printf '\\033[1;33mwarning: fake HTCC_ASSEMBLER warning\\033[0m\\n' >&2"
        , "printf '\\033[1;35m{standard input}:1:1: note: fake HTCC_ASSEMBLER note\\033[0m\\n' >&2"
        , "printf '\\033[1;33m1 warning generated.\\033[0m\\n' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningLeadingNoteDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningLeadingNoteDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"fake-macro.h:1:1: note: expanded from macro 'FAKE_MACRO'\" >&2"
        , "printf '%s\\n' '.globl main' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~' >&2"
        , "printf '%s\\n' '{standard input}:1:1: warning: fake HTCC_ASSEMBLER warning [-Wasm-operand-widths]' >&2"
        , "printf '%s\\n' '.globl main' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningContextDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningContextDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"fake-source.c: In function 'main':\" >&2"
        , "printf '%s\\n' '{standard input}: Assembler messages:' >&2"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningSnippetDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningSnippetDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' '{standard input}:1:1: warning: fake HTCC_ASSEMBLER warning [-Wasm-operand-widths]' >&2"
        , "printf '%s\\n' '.globl main' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeMultiLineWarningSnippetDriverWrapper :: FilePath -> FilePath -> IO ()
writeMultiLineWarningSnippetDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' '{standard input}:1:1: warning: fake HTCC_ASSEMBLER warning [-Wasm-operand-widths]' >&2"
        , "printf '%s\\n' 'movl $0, %eax' >&2"
        , "printf '%s\\n' 'retq' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeSnippetOnlyWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeSnippetOnlyWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' 'int x;' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeCrLfWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeCrLfWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\r\\n' \"fake-source.c: In function 'main':\" >&2"
        , "printf '%s\\r\\n' '{standard input}: Assembler messages:' >&2"
        , "printf '%s\\r\\n' '{standard input}:1:1: warning: fake HTCC_ASSEMBLER warning [-Wasm-operand-widths]' >&2"
        , "printf '%s\\r\\n' '.globl main' >&2"
        , "printf '%s\\r\\n' '^~~~~~~~~~~' >&2"
        , "printf '%s\\r\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\r\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStandaloneLeadingNoteDriverWrapper :: FilePath -> FilePath -> IO ()
writeStandaloneLeadingNoteDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'note: using fallback linker' >&2"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeIndentedPostWarningStderrDriverWrapper :: FilePath -> FilePath -> IO ()
writeIndentedPostWarningStderrDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '    cache hit: using wrapped assembler output' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writePunctuatedPostWarningStderrDriverWrapper :: FilePath -> FilePath -> IO ()
writePunctuatedPostWarningStderrDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '(cached result)' >&2"
        , "printf '%s\\n' 'status; retrying' >&2"
        , "printf '%s\\n' 'cache=hit;' >&2"
        , "printf '%s\\n' 'wrapper block {' >&2"
        , "printf '%s\\n' 'wrapper block }' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeDirectivePostWarningStderrDriverWrapper :: FilePath -> FilePath -> IO ()
writeDirectivePostWarningStderrDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '.section keep' >&2"
        , "printf '%s\\n' '# generated by fake HTCC_ASSEMBLER wrapper' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStandalonePreSummaryNoteDriverWrapper :: FilePath -> FilePath -> IO ()
writeStandalonePreSummaryNoteDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' 'note: using fallback linker' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writePostWarningNoteDriverWrapper :: FilePath -> FilePath -> IO ()
writePostWarningNoteDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "printf '%s' 'note: using fallback linker' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writePostWarningNoteWithoutSummaryDriverWrapper :: FilePath -> FilePath -> IO ()
writePostWarningNoteWithoutSummaryDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' '{standard input}:1:1: warning: fake HTCC_ASSEMBLER warning [-Wasm-operand-widths]' >&2"
        , "printf '%s' 'note: using fallback linker' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeFailingWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeFailingWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' 'In file included from fake-header.h:1:' >&2"
        , "printf '%s\\n' '                 from fake-source.c:2:' >&2"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' 'note: fake HTCC_ASSEMBLER note' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "printf '%s\\n' 'error: fake HTCC_ASSEMBLER failure' >&2"
        , "exit 1"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeErrorPreambleAfterWarningDriverWrapper :: FilePath -> FilePath -> IO ()
writeErrorPreambleAfterWarningDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' \"fake-source.c: In function 'main':\" >&2"
        , "printf '%s\\n' '{standard input}: Assembler messages:' >&2"
        , "printf '%s\\n' 'error: fake HTCC_ASSEMBLER failure' >&2"
        , "exit 1"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeErrorSnippetContainingWarningTokenDriverWrapper :: FilePath -> FilePath -> IO ()
writeErrorSnippetContainingWarningTokenDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' '{standard input}:1:1: error: fake HTCC_ASSEMBLER failure' >&2"
        , "printf '%s\\n' 'printf(\"warning:\");' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~~~~~~~~~' >&2"
        , "exit 1"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeWarningLabelErrorSnippetDriverWrapper :: FilePath -> FilePath -> IO ()
writeWarningLabelErrorSnippetDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' '{standard input}:1:1: error: fake HTCC_ASSEMBLER failure' >&2"
        , "printf '%s\\n' 'warning: return 1;' >&2"
        , "printf '%s\\n' '^~~~~~~~~~~~~~~~~' >&2"
        , "exit 1"
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeInheritedPipeHandlesDriverWrapper :: FilePath -> FilePath -> IO ()
writeInheritedPipeHandlesDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "sh -c 'sleep 2' &"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeProbeInheritedPipeHandlesDriverWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeProbeInheritedPipeHandlesDriverWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      perl -e 'select undef, undef, undef, 0.5' &"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeEarlyClosedStdoutProbeDriverWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writeEarlyClosedStdoutProbeDriverWrapper wrapperPath logPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exec 1>&-"
        , "      perl -e 'print STDERR qq(probe-stderr-flood\\n) x 8192'"
        , "      exit 0"
        , "      ;;"
        , "    *htcc-probe-*)"
        , "      exec 1>&-"
        , "      perl -e 'print STDERR qq(probe-stderr-flood\\n) x 8192'"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStreamingPromptDriverWrapper :: FilePath -> FilePath -> IO ()
writeStreamingPromptDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "assemble_invocation=false"
        , "expect_lang=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    case \"$arg\" in"
        , "      assembler)"
        , "        assemble_invocation=true"
        , "        ;;"
        , "    esac"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "    -x)"
        , "      expect_lang=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "if ! $assemble_invocation; then"
        , "  exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "fi"
        , "printf '%s\\n' 'stdout: wrapper prompt'"
        , "perl -e 'select undef, undef, undef, 1.0'"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeStreamingPromptWithoutNewlineDriverWrapper :: FilePath -> FilePath -> IO ()
writeStreamingPromptWithoutNewlineDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "assemble_invocation=false"
        , "expect_lang=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    case \"$arg\" in"
        , "      assembler)"
        , "        assemble_invocation=true"
        , "        ;;"
        , "    esac"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "    -x)"
        , "      expect_lang=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "if ! $assemble_invocation; then"
        , "  exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "fi"
        , "perl -e '$| = 1; print qq(stdout: wrapper prompt)'"
        , "perl -e 'select undef, undef, undef, 1.0'"
        , "printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "printf '%s\\n' '1 warning generated.' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeCrossStreamStreamingRetainedLineDriverWrapper :: FilePath -> FilePath -> IO ()
writeCrossStreamStreamingRetainedLineDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      exec " <> shellQuote (T.pack ("./" <> wrappedPath)) <> " \"$@\""
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s' 'stderr: wrapper prompt' >&2"
        , "perl -e 'select undef, undef, undef, 0.2'"
        , "printf '%s\\n' 'stdout: retained line'"
        , "perl -e 'select undef, undef, undef, 1.0'"
        , "printf '%s\\n' '' >&2"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeProbeCrossStreamWarningPreambleDriverWrapper :: FilePath -> FilePath -> IO ()
writeProbeCrossStreamWarningPreambleDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "probe_invocation=false"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      probe_invocation=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "if $probe_invocation; then"
        , "  printf '%s\\n' 'In file included from fake-header.h:1:'"
        , "  printf '%s\\n' '                 from fake-source.c:2:'"
        , "  printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning' >&2"
        , "  printf '%s\\n' '{standard input}:1:1: note: fake HTCC_ASSEMBLER note' >&2"
        , "  printf '%s\\n' '1 warning generated.' >&2"
        , "fi"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeProbePartialCrossStreamWarningPreambleDriverWrapper :: FilePath -> FilePath -> IO ()
writeProbePartialCrossStreamWarningPreambleDriverWrapper wrapperPath wrappedPath = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "probe_invocation=false"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple|*htcc-probe-*)"
        , "      probe_invocation=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "if $probe_invocation; then"
        , "  printf '%s' 'In file included from fake-header.h:1:' >&2"
        , "  printf '%s\\n' 'warning: fake HTCC_ASSEMBLER warning'"
        , "  printf '%s\\n' '1 warning generated.'"
        , "fi"
        , "exec ./" <> T.pack wrappedPath <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writePathLoggingWrapper :: FilePath -> FilePath -> FilePath -> IO ()
writePathLoggingWrapper wrapperPath logPath helperName = do
    T.writeFile wrapperPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$PATH\" > " <> T.pack logPath
        , "exec " <> T.pack helperName <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"

writeEnvLoggingForwardingWrapper :: FilePath -> FilePath -> [String] -> IO ()
writeEnvLoggingForwardingWrapper wrapperPath logPath envNames = do
    T.writeFile wrapperPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> concatMap renderEnvLogger envNames
            <> [ "wrapped=$1"
               , "shift"
               , "exec ./\"$wrapped\" \"$@\""
               ]
    execErrFin $ "chmod +x '" <> T.pack wrapperPath <> "'"
    where
        renderEnvLogger envName =
            [ "printf '%s=' " <> shellQuote (T.pack envName) <> " >> " <> T.pack logPath
            , "printf '%s\\n' \"$" <> T.pack envName <> "\" >> " <> T.pack logPath
            ]

shellQuote :: T.Text -> T.Text
shellQuote word = "'" <> T.replace "'" "'\"'\"'" word <> "'"

cleanupBackgroundProcessGroupHelperPids :: FilePath -> IO ()
cleanupBackgroundProcessGroupHelperPids pidPath = do
    pidFileExists <- doesFileExist pidPath
    when pidFileExists $ do
        helperPids <- filter (not . T.null) . map T.strip . T.lines <$> T.readFile pidPath
        mapM_
            (\helperPid -> do
                _ <- exec $ "kill " <> helperPid <> " > /dev/null 2>&1 || true"
                pure ()
            )
            helperPids

hostCanExecuteGeneratedElf :: Bool
hostCanExecuteGeneratedElf =
    os /= "mingw32" && os /= "darwin"

linkedElfHeaderForProbe :: Int -> B.ByteString
linkedElfHeaderForProbe elfType =
    linkedElfHeaderWithInterpreterForProbeBytes elfType []

linkedElfHeaderWithInterpreterForProbeBytes :: Int -> [Word8] -> B.ByteString
linkedElfHeaderWithInterpreterForProbeBytes elfType interpreterBytes =
    linkedElfHeaderWithInterpreterAndDynamicEntriesForProbe
        elfType
        interpreterBytes
        (defaultDynamicEntriesForProbe elfType interpreterBytes)

linkedElfHeaderWithInterpreterAndDynamicEntriesForProbe :: Int -> [Word8] -> [(Int, Int)] -> B.ByteString
linkedElfHeaderWithInterpreterAndDynamicEntriesForProbe elfType interpreterBytes dynamicEntries =
    B.pack $
        [ 0x7f, 0x45, 0x4c, 0x46
        , 0x02, 0x01, 0x01, 0x00
        , 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00
        ]
            <> word16leForProbe elfType
            <> word16leForProbe 0x3e
            <> word32leForProbe 0x1
            <> word64leForProbe (0x400000 + fromIntegral codeOffset)
            <> word64leForProbe 0x40
            <> word64leForProbe 0x0
            <> word32leForProbe 0x0
            <> word16leForProbe 0x40
            <> word16leForProbe 0x38
            <> word16leForProbe programHeaderCount
            <> word16leForProbe 0x0
            <> word16leForProbe 0x0
            <> word16leForProbe 0x0
            <> word32leForProbe 0x1
            <> word32leForProbe 0x5
            <> word64leForProbe 0x0
            <> word64leForProbe 0x400000
            <> word64leForProbe 0x400000
            <> word64leForProbe totalFileSize
            <> word64leForProbe totalFileSize
            <> word64leForProbe 0x1000
            <> interpreterProgramHeader
            <> dynamicProgramHeader
            <> [0x90]
            <> interpreterBytes
            <> dynamicBytes
    where
        hasInterpreter = not (null interpreterBytes)
        dynamicBytes =
            concatMap
                (\(tag, value) -> word64leForProbe (fromIntegral tag) <> word64leForProbe (fromIntegral value))
                dynamicEntries
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
                word32leForProbe 0x3
                    <> word32leForProbe 0x0
                    <> word64leForProbe (fromIntegral interpreterOffset)
                    <> word64leForProbe 0x0
                    <> word64leForProbe 0x0
                    <> word64leForProbe (fromIntegral $ length interpreterBytes)
                    <> word64leForProbe (fromIntegral $ length interpreterBytes)
                    <> word64leForProbe 0x1
            | otherwise =
                []
        dynamicProgramHeader
            | hasDynamic =
                word32leForProbe 0x2
                    <> word32leForProbe 0x0
                    <> word64leForProbe (fromIntegral dynamicOffset)
                    <> word64leForProbe (0x400000 + fromIntegral dynamicOffset)
                    <> word64leForProbe 0x0
                    <> word64leForProbe (fromIntegral $ length dynamicBytes)
                    <> word64leForProbe (fromIntegral $ length dynamicBytes)
                    <> word64leForProbe 0x8
            | otherwise =
                []

defaultDynamicEntriesForProbe :: Int -> [Word8] -> [(Int, Int)]
defaultDynamicEntriesForProbe elfType interpreterBytes
    | elfType == 3 && null interpreterBytes =
        [ (elfDynamicTagFlags1ForProbe, elfDynamicFlag1PieForProbe)
        , (elfDynamicTagNullForProbe, 0)
        ]
    | otherwise =
        []

asciiBytesForProbe :: String -> B.ByteString
asciiBytesForProbe =
    B.pack . map (fromIntegral . fromEnum)

elfDynamicTagNullForProbe :: Int
elfDynamicTagNullForProbe = 0

elfDynamicTagFlags1ForProbe :: Int
elfDynamicTagFlags1ForProbe = 0x6ffffffb

elfDynamicFlag1PieForProbe :: Int
elfDynamicFlag1PieForProbe = 0x08000000

word16leForProbe :: Int -> [Word8]
word16leForProbe value =
    [ fromIntegral value
    , fromIntegral $ value `div` 0x100
    ]

word32leForProbe :: Int -> [Word8]
word32leForProbe value =
    [ fromIntegral value
    , fromIntegral $ value `div` 0x100
    , fromIntegral $ value `div` 0x10000
    , fromIntegral $ value `div` 0x1000000
    ]

word64leForProbe :: Integer -> [Word8]
word64leForProbe value =
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

createFreshTempDir :: String -> IO FilePath
createFreshTempDir prefix = do
    (path, handle) <- openTempFile "." prefix
    hClose handle
    removeFile path
    createDirectoryIfMissing False path
    pure path

writeExecutableProxy :: FilePath -> FilePath -> IO ()
writeExecutableProxy proxyPath targetPath = do
    T.writeFile proxyPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "exec " <> shellQuote (T.pack targetPath) <> " \"$@\""
        ]
    execErrFin $ "chmod +x '" <> T.pack proxyPath <> "'"

writeFakeAssemblerWithLogs :: FilePath -> FilePath -> FilePath -> IO ()
writeFakeAssemblerWithLogs = writeFakeAssemblerWithTarget "x86_64-linux-gnu"

writeFakeAssemblerWithTarget :: T.Text -> FilePath -> FilePath -> FilePath -> IO ()
writeFakeAssemblerWithTarget targetTriple logPath asmPath assemblerPath = do
    T.writeFile assemblerPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "copy_file() {"
        , "  src=$1"
        , "  dst=$2"
        , "  while IFS= read -r line || [ -n \"$line\" ]; do"
        , "    printf '%s\\n' \"$line\""
        , "  done < \"$src\" > \"$dst\""
        , "}"
        , "read_file() {"
        , "  while IFS= read -r line || [ -n \"$line\" ]; do"
        , "    printf '%s\\n' \"$line\""
        , "  done < \"$1\""
        , "}"
        , "write_probe_object() {"
        ]
            <> probeObjectWriter
            <> [ "}"
               , "for arg in \"$@\"; do"
               , "  case \"$arg\" in"
               , "    -dumpmachine|-print-target-triple)"
               , "      printf '%s\\n' '" <> targetTriple <> "'"
               , "      exit 0"
               , "      ;;"
               , "  esac"
               , "done"
               , "assemble=false"
               , "out=''"
               , "input=''"
               , "expect_lang=false"
               , "expect_out=false"
               , "for arg in \"$@\"; do"
               , "  if $expect_lang; then"
               , "    expect_lang=false"
               , "    continue"
               , "  fi"
               , "  if $expect_out; then"
               , "    out=\"$arg\""
               , "    expect_out=false"
               , "    continue"
               , "  fi"
               , "  case \"$arg\" in"
               , "    -x)"
               , "      assemble=true"
               , "      expect_lang=true"
               , "      ;;"
               , "    -c)"
               , "      ;;"
               , "    -o)"
               , "      expect_out=true"
               , "      ;;"
               , "    -*)"
               , "      ;;"
               , "    *)"
               , "      input=\"$arg\""
               , "      ;;"
               , "  esac"
               , "done"
               , "if $assemble; then"
               , "  printf '%s\\n' \"$@\" > " <> T.pack logPath
               , "  test -n \"$out\""
               , "  test -n \"$input\""
               , "  copy_file \"$input\" " <> T.pack asmPath
               , "  input_contents=$(read_file \"$input\")"
               , "  case \"$input_contents\" in"
               , "    *\".intel_syntax noprefix\"*) : ;;"
               , "    *) exit 98 ;;"
               , "  esac"
               , "  case \"$input_contents\" in"
               , "    *\"main:\"*|*\".L.htcc_runnable_output_marker_ctor:\"*) : ;;"
               , "    *) exit 98 ;;"
               , "  esac"
               , "  while [ \"$1\" != '-x' ]; do"
               , "    shift"
               , "  done"
               , "fi"
               , "test -n \"$out\""
               , "if $assemble; then"
               , "  write_probe_object \"$out\""
               , "else"
               , "  case \"$out\" in"
               , "    *htcc-probe-*)"
               ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
        , "      test -n \"$input\""
        , "      write_runnable_elf \"$out\""
        , "      cat \"$input\" >> \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack assemblerPath <> "'"
    where
        normalizedTarget = T.toLower targetTriple
        emitsElfObject =
            not $ any (`T.isInfixOf` normalizedTarget)
                [ "apple"
                , "cygwin"
                , "darwin"
                , "mingw"
                , "msvc"
                , "windows"
                ]

        probeObjectWriter
            | emitsElfObject =
                [ "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$1\""
                , "  cat \"$input\" >> \"$1\""
                ]
            | otherwise =
                [ "  printf 'MZfake-object\\n' > \"$1\""
                ]

writeFailingCompiler :: FilePath -> FilePath -> IO ()
writeFailingCompiler compilerPath logPath = do
    T.writeFile compilerPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "printf '%s\\n' \"$@\" > " <> T.pack logPath
        , "exit 97"
        ]
    execErrFin $ "chmod +x '" <> T.pack compilerPath <> "'"

writeAssembleOnlyDriver :: FilePath -> FilePath -> IO ()
writeAssembleOnlyDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  exit 97"
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSingleObjectOnlyLinkDriver :: FilePath -> FilePath -> IO ()
writeSingleObjectOnlyLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "printf '%s\\n' \"$*\" >> " <> T.pack logPath
               , "for arg in \"$@\"; do"
               , "  case \"$arg\" in"
               , "    -dumpmachine|-print-target-triple)"
               , "      printf '%s\\n' 'x86_64-linux-gnu'"
               , "      exit 0"
               , "      ;;"
               , "  esac"
               , "done"
               , "assemble=false"
               , "out=''"
               , "input=''"
               , "input_count=0"
               , "expect_lang=false"
               , "expect_out=false"
               , "for arg in \"$@\"; do"
               , "  if $expect_lang; then"
               , "    expect_lang=false"
               , "    continue"
               , "  fi"
               , "  if $expect_out; then"
               , "    out=\"$arg\""
               , "    expect_out=false"
               , "    continue"
               , "  fi"
               , "  case \"$arg\" in"
               , "    -x)"
               , "      assemble=true"
               , "      expect_lang=true"
               , "      ;;"
               , "    -o)"
               , "      expect_out=true"
               , "      ;;"
               , "    -*)"
               , "      ;;"
               , "    *)"
               , "      input=\"$arg\""
               , "      input_count=$((input_count + 1))"
               , "      ;;"
               , "  esac"
               , "done"
               , "test -n \"$out\""
               , "test -n \"$input\""
               , "if $assemble; then"
               , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
               , "  cat \"$input\" >> \"$out\""
               , "else"
               , "  if [ \"$input_count\" -gt 1 ]; then"
               , "    exit 97"
               , "  fi"
               , "  write_runnable_elf \"$out\""
               , "  cat \"$input\" >> \"$out\""
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeInPlaceLinkDriver :: FilePath -> FilePath -> IO ()
writeInPlaceLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
        , "      if [ -e \"$out\" ]; then"
        , "        printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "      else"
        , "        printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "        chmod +x \"$out\""
        , "      fi"
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeProbePrecreatedOutputDriver :: FilePath -> FilePath -> IO ()
writeProbePrecreatedOutputDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  test -e \"$out\""
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
        , "      printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "      chmod +x \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeScriptLinkProbeDriver :: FilePath -> FilePath -> IO ()
writeScriptLinkProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "  chmod +x \"$out\""
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSharedLinkProbeDriver :: FilePath -> FilePath -> IO ()
writeSharedLinkProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "test -n \"$input\""
        , "if $assemble; then"
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\003\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "fi"
        , "cat \"$input\" >> \"$out\""
        , "if ! $assemble; then"
        , "  chmod +x \"$out\""
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeForeignAbiLinkProbeDriver :: FilePath -> FilePath -> IO ()
writeForeignAbiLinkProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        ]
            <> foreignAbiRunnableElfWriter
            <> [ "test -n \"$out\""
               , "if $assemble; then"
               , "  test -n \"$input\""
               , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
               , "  cat \"$input\" >> \"$out\""
               , "else"
               , "  case \"$out\" in"
               , "    *htcc-probe-*)"
               ]
            <> map ("  " <>) foreignAbiProbeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
               , "      write_foreign_abi_runnable_elf \"$out\""
               , "      ;;"
               , "  esac"
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeBlobLinkProbeDriver :: FilePath -> FilePath -> IO ()
writeBlobLinkProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\002\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  printf '%s' 'unrelated-probe-blob' >> \"$out\""
        , "  chmod +x \"$out\""
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeTouchingLinkDriver :: FilePath -> FilePath -> IO ()
writeTouchingLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  : > \"$out\""
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSymlinkLinkProbeDriver :: FilePath -> FilePath -> FilePath -> IO ()
writeSymlinkLinkProbeDriver driverPath logPath targetPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  test -n \"$input\""
        , "  target=\"$(pwd)/" <> T.pack targetPath <> "\""
        , "  rm -f \"$target\" \"$out\""
        , "  write_runnable_elf \"$target\""
        , "  cat \"$input\" >> \"$target\""
        , "  ln -sf \"$target\" \"$out\""
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSymlinkSpecialLinkProbeDriver :: FilePath -> FilePath -> IO ()
writeSymlinkSpecialLinkProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        ]
            <> probeLinkedOutputWriter
            <> [ "else"
               , "  ln -sfn /dev/null \"$out\""
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeNonExecutableLinkProbeDriver :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
writeNonExecutableLinkProbeDriver driverPath logPath relocatableObjectPath cannedExecutablePath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "case \"$1\" in"
        , "  -dumpmachine|-print-target-triple)"
        , "    printf '%s\\n' 'x86_64-linux-gnu'"
        , "    exit 0"
        , "    ;;"
        , "esac"
        , "case \"$1\" in"
        , "  -x)"
        , "    cat " <> shellQuote (T.pack relocatableObjectPath) <> " > \"$5\""
        , "    ;;"
        , "  -no-pie)"
        , "    cat " <> shellQuote (T.pack cannedExecutablePath) <> " > \"$3\""
        , "    chmod 0644 \"$3\""
        , "    ;;"
        , "  *)"
        , "    exit 1"
        , "    ;;"
        , "esac"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeBogusFinalLinkDriver :: FilePath -> FilePath -> FilePath -> IO ()
writeBogusFinalLinkDriver driverPath logPath targetPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      target=\"$(pwd)/" <> T.pack targetPath <> "\""
               , "      cat \"$out\" > \"$target\""
               , "      chmod +x \"$target\""
               , "      ;;"
               , "    *)"
        , "      test -n \"$input\""
        , "      target=\"$(pwd)/" <> T.pack targetPath <> "\""
        , "      test -f \"$target\""
        , "      cat \"$target\" > \"$out\""
        , "      chmod +x \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeMarkerStrippingFinalLinkDriver :: FilePath -> FilePath -> IO ()
writeMarkerStrippingFinalLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
               , "      test -n \"$input\""
               , "      write_runnable_elf \"$out\""
               , "      cat \"$input\" >> \"$out\""
               , "      ;;"
               , "  esac"
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeGcSectionsLinkDriver :: FilePath -> FilePath -> IO ()
writeGcSectionsLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        ]
            <> runnableElfWriter
            <> [ "printf '%s\\n' \"$*\" >> " <> T.pack logPath
               , "for arg in \"$@\"; do"
               , "  case \"$arg\" in"
               , "    -dumpmachine|-print-target-triple)"
               , "      printf '%s\\n' 'x86_64-linux-gnu'"
               , "      exit 0"
               , "      ;;"
               , "  esac"
               , "done"
               , "mode=link"
               , "out=''"
               , "input=''"
               , "expect_lang=false"
               , "expect_out=false"
               , "for arg in \"$@\"; do"
               , "  if $expect_lang; then"
               , "    expect_lang=false"
               , "    continue"
               , "  fi"
               , "  if $expect_out; then"
               , "    out=\"$arg\""
               , "    expect_out=false"
               , "    continue"
               , "  fi"
               , "  case \"$arg\" in"
               , "    -x)"
               , "      mode=assemble"
               , "      expect_lang=true"
               , "      ;;"
               , "    -o)"
               , "      expect_out=true"
               , "      ;;"
               , "    -*)"
               , "      ;;"
               , "    *)"
               , "      input=\"$arg\""
               , "      ;;"
               , "  esac"
               , "done"
               , "test -n \"$out\""
               , "if [ \"$mode\" = 'assemble' ]; then"
               , "  test -n \"$input\""
               , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
               , "  cat \"$input\" >> \"$out\""
               , "else"
               , "  case \"$out\" in"
               , "    *htcc-probe-*)"
               ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
               , "      test -n \"$input\""
               , "      write_runnable_elf \"$out\""
               , "      if LC_ALL=C grep -Fq '.section .init_array,\"aw\",@init_array' \"$input\"; then"
               , "        cat \"$input\" >> \"$out\""
               , "      fi"
               , "      ;;"
               , "  esac"
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeNamedPipeFinalLinkDriver :: FilePath -> FilePath -> IO ()
writeNamedPipeFinalLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "mode=link"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      mode=assemble"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if [ \"$mode\" = 'assemble' ]; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
               , "      rm -f \"$out\""
               , "      mkfifo \"$out\""
               , "      ;;"
               , "  esac"
               , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeExecutableObjectProbeDriver :: FilePath -> FilePath -> IO ()
writeExecutableObjectProbeDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines $
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\002\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "  cat \"$input\" >> \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *htcc-probe-*)"
        ]
            <> probeLinkedOutputWriter
            <> [ "      ;;"
               , "    *)"
        , "      printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "      chmod +x \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
               ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeSymlinkObjectProbeDriver :: FilePath -> FilePath -> FilePath -> IO ()
writeSymlinkObjectProbeDriver driverPath logPath targetPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  target=\"$(pwd)/" <> T.pack targetPath <> "\""
        , "  rm -f \"$target\" \"$out\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$target\""
        , "  cat \"$input\" >> \"$target\""
        , "  ln -sf \"$target\" \"$out\""
        , "else"
        , "  printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "  chmod +x \"$out\""
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeFailingLinkDriver :: FilePath -> FilePath -> IO ()
writeFailingLinkDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "assemble=false"
        , "out=''"
        , "input=''"
        , "expect_lang=false"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_lang; then"
        , "    expect_lang=false"
        , "    continue"
        , "  fi"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -x)"
        , "      assemble=true"
        , "      expect_lang=true"
        , "      ;;"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "    -*)"
        , "      ;;"
        , "    *)"
        , "      input=\"$arg\""
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "if $assemble; then"
        , "  test -n \"$input\""
        , "  printf '\\177ELF\\002\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\001\\000\\076\\000\\001\\000\\000\\000' > \"$out\""
        , "else"
        , "  case \"$out\" in"
        , "    *tmp-read-only.out*.htcc-*)"
        , "      : > \"$out\""
        , "      exit 97"
        , "      ;;"
        , "    *)"
        , "      printf '%s\\n' '#!/bin/sh' 'exit 0' > \"$out\""
        , "      chmod +x \"$out\""
        , "      ;;"
        , "  esac"
        , "fi"
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeHostTargetDriver :: FilePath -> FilePath -> IO ()
writeHostTargetDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'arm64-apple-darwin23.5.0'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "out=''"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "printf 'MZfake-object\\n' > \"$out\""
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

writeMetadataSpoofingDriver :: FilePath -> FilePath -> IO ()
writeMetadataSpoofingDriver driverPath logPath = do
    T.writeFile driverPath $ T.unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "printf '%s\\n' \"$*\" >> " <> T.pack logPath
        , "for arg in \"$@\"; do"
        , "  case \"$arg\" in"
        , "    -dumpmachine|-print-target-triple)"
        , "      printf '%s\\n' 'x86_64-linux-gnu'"
        , "      exit 0"
        , "      ;;"
        , "  esac"
        , "done"
        , "out=''"
        , "expect_out=false"
        , "for arg in \"$@\"; do"
        , "  if $expect_out; then"
        , "    out=\"$arg\""
        , "    expect_out=false"
        , "    continue"
        , "  fi"
        , "  case \"$arg\" in"
        , "    -o)"
        , "      expect_out=true"
        , "      ;;"
        , "  esac"
        , "done"
        , "test -n \"$out\""
        , "printf 'MZfake-object\\n' > \"$out\""
        ]
    execErrFin $ "chmod +x '" <> T.pack driverPath <> "'"

mkResult :: T.Text -> Bool -> T.Text -> (Either T.Text T.Text, String)
mkResult msg ok details =
    ( if ok then Right msg else Left details
    , T.unpack msg
    )

outputFileTest :: IO (Either T.Text T.Text, String)
outputFileTest = flip finally (clean ["tmp.out", "tmp.s"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , htccCmd
        , " -o tmp.s /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasRequiredLabels =
            all (`T.isInfixOf` asm)
                [ ".L.return.main:"
                , ".L.label.main.done:"
                , ".L.case.main."
                ]
        ok = T.null stdoutLeak && hasRequiredLabels
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
            ]
    return $ mkResult outputFileMsg ok details

outputFileLegacyLongAliasTest :: IO (Either T.Text T.Text, String)
outputFileLegacyLongAliasTest = flip finally (clean ["tmp.out", "tmp.s"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , htccCmd
        , " --out tmp.s /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasRequiredLabels =
            all (`T.isInfixOf` asm)
                [ ".L.return.main:"
                , ".L.label.main.done:"
                , ".L.case.main."
                ]
        ok = T.null stdoutLeak && hasRequiredLabels
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
            ]
    return $ mkResult outputFileLegacyLongAliasMsg ok details

visualizeAstLegacyFlagsTest :: IO (Either T.Text T.Text, String)
visualizeAstLegacyFlagsTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.svg"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main() { return 0; }' | "
        , htccCmd
        , " --visualize-ast --img-resolution 320x240 --out tmp.svg /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrLeak <- T.readFile "tmp.err"
    svgExists <- doesFileExist "tmp.svg"
    svg <- if svgExists then T.readFile "tmp.svg" else pure T.empty
    let hasSvgHeader = "<svg" `T.isInfixOf` svg
        ok = T.null stdoutLeak && T.null stderrLeak && svgExists && hasSvgHeader
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrLeak
            , "svgExists: " <> T.pack (show svgExists)
            , "hasSvgHeader: " <> T.pack (show hasSvgHeader)
            ]
    return $ mkResult visualizeAstLegacyFlagsMsg ok details

visualizeAstPreservesParsedForSectionsTest :: IO (Either T.Text T.Text, String)
visualizeAstPreservesParsedForSectionsTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.svg"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main(){for(;;) return 1;}' | "
        , htccCmd
        , " --visualize-ast --out tmp.svg /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrLeak <- T.readFile "tmp.err"
    svgExists <- doesFileExist "tmp.svg"
    svg <- if svgExists then T.readFile "tmp.svg" else pure T.empty
    let emptySectionsPreserved = T.count "Null" svg == 3
        hasLoop = "for" `T.isInfixOf` svg
        hasReturn = "return" `T.isInfixOf` svg
        hasLiteral = "1 (" `T.isInfixOf` svg
        ok =
            T.null stdoutLeak
                && T.null stderrLeak
                && svgExists
                && emptySectionsPreserved
                && hasLoop
                && hasReturn
                && hasLiteral
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrLeak
            , "svgExists: " <> T.pack (show svgExists)
            , "emptySectionsPreserved: " <> T.pack (show emptySectionsPreserved)
            , "hasLoop: " <> T.pack (show hasLoop)
            , "hasReturn: " <> T.pack (show hasReturn)
            , "hasLiteral: " <> T.pack (show hasLiteral)
            ]
    return $ mkResult visualizeAstPreservesParsedForSectionsMsg ok details

visualizeAstAcceptsAsmNormalizationFailureTest :: IO (Either T.Text T.Text, String)
visualizeAstAcceptsAsmNormalizationFailureTest =
    flip finally (clean ["tmp.err", "tmp.out", "tmp.svg", "tmp-visualize-incomplete-inc.c"]) $ do
        htccCmd <- htccCommand
        let inputPath = "tmp-visualize-incomplete-inc.c"
        T.writeFile inputPath $
            T.unlines
                [ "int (*f(void))[];"
                , "int main(void) { int (*p)[] = f(); ++p; return 0; }"
                ]
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out tmp.svg "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        svgExists <- doesFileExist "tmp.svg"
        svg <- if svgExists then T.readFile "tmp.svg" else pure ""
        let succeeded = exitCode (const False) True result
            hasSvg = "<svg" `T.isInfixOf` svg
            hasReturn = "return" `T.isInfixOf` svg
            hasMain = "main" `T.isInfixOf` svg
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && svgExists
                    && hasSvg
                    && hasReturn
                    && hasMain
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "svgExists: " <> T.pack (show svgExists)
                , "hasSvg: " <> T.pack (show hasSvg)
                , "hasReturn: " <> T.pack (show hasReturn)
                , "hasMain: " <> T.pack (show hasMain)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstAcceptsAsmNormalizationFailureMsg ok details

visualizeAstRejectsNonFiniteResolutionTest :: IO (Either T.Text T.Text, String)
visualizeAstRejectsNonFiniteResolutionTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.svg"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main() { return 0; }' | "
        , htccCmd
        , " --visualize-ast --img-resolution 1e309x1e309 --out tmp.svg /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    svgExists <- doesFileExist "tmp.svg"
    svg <- if svgExists then T.readFile "tmp.svg" else pure T.empty
    let warnedAboutInvalidResolution =
            "warning: the specified resolution is invalid, so using default resolution." `T.isInfixOf` stderrOut
        hasSvgHeader = "<svg" `T.isInfixOf` svg
        hasFiniteDimensions =
            not ("Infinity" `T.isInfixOf` svg || "NaN" `T.isInfixOf` svg)
        ok =
            T.null stdoutLeak
                && warnedAboutInvalidResolution
                && svgExists
                && hasSvgHeader
                && hasFiniteDimensions
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "svgExists: " <> T.pack (show svgExists)
            , "hasSvgHeader: " <> T.pack (show hasSvgHeader)
            , "hasFiniteDimensions: " <> T.pack (show hasFiniteDimensions)
            ]
    return $ mkResult visualizeAstRejectsNonFiniteResolutionMsg ok details

visualizeAstRejectsNonPositiveResolutionTest :: IO (Either T.Text T.Text, String)
visualizeAstRejectsNonPositiveResolutionTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.svg"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main() { return 0; }' | "
        , htccCmd
        , " --visualize-ast --img-resolution 0x0 --out tmp.svg /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    svgExists <- doesFileExist "tmp.svg"
    svg <- if svgExists then T.readFile "tmp.svg" else pure T.empty
    let warnedAboutInvalidResolution =
            "warning: the specified resolution is invalid, so using default resolution." `T.isInfixOf` stderrOut
        hasSvgHeader = "<svg" `T.isInfixOf` svg
        usedDefaultResolution =
            "width=\"640.0000\"" `T.isInfixOf` svg
                && "height=\"480.0000\"" `T.isInfixOf` svg
        ok =
            T.null stdoutLeak
                && warnedAboutInvalidResolution
                && svgExists
                && hasSvgHeader
                && usedDefaultResolution
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "svgExists: " <> T.pack (show svgExists)
            , "hasSvgHeader: " <> T.pack (show hasSvgHeader)
            , "usedDefaultResolution: " <> T.pack (show usedDefaultResolution)
            ]
    return $ mkResult visualizeAstRejectsNonPositiveResolutionMsg ok details

visualizeAstRejectsDeclarationOnlyInputTest :: IO (Either T.Text T.Text, String)
visualizeAstRejectsDeclarationOnlyInputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-empty.c", "tmp-empty.svg"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-empty.svg"
            inputPath = "tmp-empty.c"
            expectedError = "There is nothing to describe"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        T.writeFile inputPath "int g;"
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && hasExpectedError
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstRejectsDeclarationOnlyInputMsg ok details

visualizeAstRejectsNonSvgOutputTest :: IO (Either T.Text T.Text, String)
visualizeAstRejectsNonSvgOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-invalid-output.c", "tmp-invalid-output.png"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-invalid-output.png"
            inputPath = "tmp-invalid-output.c"
            expectedError = "AST visualization output path must use the .svg extension"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        T.writeFile inputPath "int main(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && hasExpectedError
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstRejectsNonSvgOutputMsg ok details

visualizeAstAcceptsSymlinkAliasToSvgTargetTest :: IO (Either T.Text T.Text, String)
visualizeAstAcceptsSymlinkAliasToSvgTargetTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-visualize-output.c", "tmp-visualize-output-current", "tmp-visualize-output-target.svg"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-visualize-output-target.svg"
            alias = "tmp-visualize-output-current"
            inputPath = "tmp-visualize-output.c"
        clean [target, alias, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        execErrFin $ "ln -s '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        T.writeFile inputPath "int main(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack alias
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetContents <- T.readFile target
        linkStatus <- getSymbolicLinkStatus alias
        let succeeded = exitCode (const False) True result
            linkStillSymlink = isSymbolicLink linkStatus
            targetUpdated = "<svg" `T.isInfixOf` targetContents && targetContents /= "stale output"
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && linkStillSymlink
                    && targetUpdated
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "linkStillSymlink: " <> T.pack (show linkStillSymlink)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstAcceptsSymlinkAliasToSvgTargetMsg ok details

visualizeAstRejectsSymlinkedNonSvgOutputTest :: IO (Either T.Text T.Text, String)
visualizeAstRejectsSymlinkedNonSvgOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-invalid-output.c", "tmp-invalid-output-link.svg", "tmp-invalid-output-target.txt"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-invalid-output-target.txt"
            alias = "tmp-invalid-output-link.svg"
            inputPath = "tmp-invalid-output.c"
            expectedError = "AST visualization output path must use the .svg extension"
        clean [target, alias, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        execErrFin $ "ln -s '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        T.writeFile inputPath "int main(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack alias
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && hasExpectedError
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstRejectsSymlinkedNonSvgOutputMsg ok details

imgResolutionRequiresVisualizeAstTest :: IO (Either T.Text T.Text, String)
imgResolutionRequiresVisualizeAstTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-resolution.c", "tmp-resolution.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-resolution.s"
            inputPath = "tmp-resolution.c"
            expectedError = "--img-resolution requires --visualize-ast"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile inputPath "int main(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " --img-resolution 320x240 --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && not targetExists
                    && hasExpectedError
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult imgResolutionRequiresVisualizeAstMsg ok details

suppressWarnsCanonicalFlagTest :: IO (Either T.Text T.Text, String)
suppressWarnsCanonicalFlagTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.s"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main() { return foo(); }' | "
        , htccCmd
        , " --suppress-warns --out tmp.s /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrLeak <- T.readFile "tmp.err"
    asm <- T.readFile "tmp.s"
    let hasFooCall = "call foo" `T.isInfixOf` asm
        ok = T.null stdoutLeak && T.null stderrLeak && hasFooCall
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrLeak
            , "hasFooCall: " <> T.pack (show hasFooCall)
            ]
    return $ mkResult suppressWarnsCanonicalFlagMsg ok details

suppressWarnsLegacyFlagTest :: IO (Either T.Text T.Text, String)
suppressWarnsLegacyFlagTest = flip finally (clean ["tmp.err", "tmp.out", "tmp.s"]) $ do
    htccCmd <- htccCommand
    execErrFin $ mconcat
        [ "echo 'int main() { return foo(); }' | "
        , htccCmd
        , " --supress-warns --out tmp.s /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrLeak <- T.readFile "tmp.err"
    asm <- T.readFile "tmp.s"
    let hasFooCall = "call foo" `T.isInfixOf` asm
        ok = T.null stdoutLeak && T.null stderrLeak && hasFooCall
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrLeak
            , "hasFooCall: " <> T.pack (show hasFooCall)
            ]
    return $ mkResult suppressWarnsLegacyFlagMsg ok details

suppressWarnsRunAsmTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningDriverWrapper fakeWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            result <- exec "./tmp"
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && compilerSawExpectedArgs && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmMsg ok details

suppressWarnsRunAsmSuppressesStdoutWarningsTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmSuppressesStdoutWarningsTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeStdoutWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStdoutWarningDriverWrapper fakeStdoutWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStdoutWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            result <- exec "./tmp"
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && compilerSawExpectedArgs && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmSuppressesStdoutWarningsMsg ok details

suppressWarnsRunAsmPreservesBinaryOutputTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesBinaryOutputTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeBinaryWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeBinaryWarningDriverWrapper fakeBinaryWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeBinaryWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- B.readFile "tmp.out"
            stderrLeak <- B.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                hasBinaryStdout = B.elem 0xfe stdoutLeak
                hasBinaryStderr = B.elem 0xff stderrLeak
                hasWarningLeak = B.pack (map (fromIntegral . fromEnum) "warning: fake HTCC_ASSEMBLER warning")
                    `B.isInfixOf` stderrLeak
                ok = ranOk && hasBinaryStdout && hasBinaryStderr && not hasWarningLeak
                details = T.unlines
                    [ "stdoutBytes: " <> T.pack (show stdoutLeak)
                    , "stderrBytes: " <> T.pack (show stderrLeak)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesBinaryOutputMsg ok details

suppressWarnsRunAsmPreservesUnterminatedStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesUnterminatedStderrTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeUnterminatedStderrWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeUnterminatedStderrDriverWrapper fakeUnterminatedStderrWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeUnterminatedStderrWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- B.readFile "tmp.out"
            stderrLeak <- B.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = B.null stdoutLeak && stderrLeak == B.pack [88, 89, 90] && ranOk
                details = T.unlines
                    [ "stdoutBytes: " <> T.pack (show stdoutLeak)
                    , "stderrBytes: " <> T.pack (show stderrLeak)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesUnterminatedStderrMsg ok details

suppressWarnsRunAsmPreservesStdoutStderrInterleavingTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStdoutStderrInterleavingTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeInterleavedOutputWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeInterleavedOutputDriverWrapper fakeInterleavedOutputWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeInterleavedOutputWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines =
                    concat $
                        replicate
                            2
                            [ T.pack "stdout: wrapper start"
                            , T.pack "stderr: wrapper note"
                            , T.pack "stdout: wrapper end"
                            ]
                mergedLines = T.lines mergedOutput
                ok = mergedLines == expectedLines && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStdoutStderrInterleavingMsg ok details

suppressWarnsRunAsmPreservesSameReadStdoutInterleavingTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesSameReadStdoutInterleavingTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeSameReadStdoutInterleavingWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeSameReadStdoutInterleavingDriverWrapper
                fakeSameReadStdoutInterleavingWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeSameReadStdoutInterleavingWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines =
                    concat $
                        replicate
                            2
                            [ T.pack "stdout: first"
                            , T.pack "stderr: middle"
                            , T.pack "stdout: second"
                            ]
                mergedLines = T.lines mergedOutput
                ok = mergedLines == expectedLines && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesSameReadStdoutInterleavingMsg ok details

suppressWarnsRunAsmPreservesSameReadStderrChunksTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesSameReadStderrChunksTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeSameReadStderrChunksWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeSameReadStderrChunksDriverWrapper fakeSameReadStderrChunksWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeSameReadStderrChunksWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines =
                    concat $
                        replicate
                            2
                            [ T.pack "stderr: first"
                            , T.pack "stderr: second"
                            , T.pack "stdout: after"
                            ]
                mergedLines = T.lines mergedOutput
                ok = mergedLines == expectedLines && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesSameReadStderrChunksMsg ok details

suppressWarnsRunAsmScopesRetainedIndicesByStreamTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmScopesRetainedIndicesByStreamTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeStreamScopedWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStreamScopedWarningDriverWrapper fakeStreamScopedWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStreamScopedWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines =
                    [ T.pack "stdout: retained first"
                    , T.pack "stdout: retained second"
                    ]
                mergedLines = T.lines mergedOutput
                hasWarningLeak =
                    "warning: fake HTCC_ASSEMBLER warning" `T.isInfixOf` mergedOutput
                        || "1 warning generated." `T.isInfixOf` mergedOutput
                ok = mergedLines == expectedLines && not hasWarningLeak && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasWarningLeak: " <> T.pack (show hasWarningLeak)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmScopesRetainedIndicesByStreamMsg
                    ok
                    details

suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeSplitWarningInterleavedStdoutWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeSplitWarningInterleavedStdoutDriverWrapper
                fakeSplitWarningInterleavedStdoutWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeSplitWarningInterleavedStdoutWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines = replicate 2 $ T.pack "stdout: wrapper progress"
                mergedLines = T.lines mergedOutput
                hasWarningLeak =
                    "warning: fake HTCC_ASSEMBLER warning" `T.isInfixOf` mergedOutput
                ok = mergedLines == expectedLines && not hasWarningLeak && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasWarningLeak: " <> T.pack (show hasWarningLeak)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmDropsSplitWarningWithInterleavedStdoutMsg
                    ok
                    details

suppressWarnsRunAsmDropsSplitWarningPrefixTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsSplitWarningPrefixTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeSplitWarningPrefixWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeSplitWarningPrefixDriverWrapper
                fakeSplitWarningPrefixWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeSplitWarningPrefixWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines = replicate 2 $ T.pack "stdout: wrapper progress"
                mergedLines = T.lines mergedOutput
                hasWarningLeak =
                    "warning: fake HTCC_ASSEMBLER warning" `T.isInfixOf` mergedOutput
                        || "{standard input}:1:1: warn" `T.isInfixOf` mergedOutput
                ok = mergedLines == expectedLines && not hasWarningLeak && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasWarningLeak: " <> T.pack (show hasWarningLeak)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmDropsSplitWarningPrefixMsg
                    ok
                    details

suppressWarnsRunAsmDropsCrossStreamWarningPreambleTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsCrossStreamWarningPreambleTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeCrossStreamWarningPreambleWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeCrossStreamWarningPreambleDriverWrapper
                fakeCrossStreamWarningPreambleWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeCrossStreamWarningPreambleWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmDropsCrossStreamWarningPreambleMsg
                    ok
                    details

suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeStdoutPrefixBeforeStderrWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStdoutPrefixBeforeStderrDriverWrapper fakeStdoutPrefixBeforeStderrWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStdoutPrefixBeforeStderrWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- T.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedLines =
                    concat $
                        replicate
                            2
                            [ T.pack "stdout: prefixstderr: note"
                            , T.pack "stdout: suffix"
                            ]
                mergedLines = T.lines mergedOutput
                ok = mergedLines == expectedLines && targetExists
                details = T.unlines
                    [ "mergedOutput:"
                    , mergedOutput
                    , "mergedLines: " <> T.pack (show mergedLines)
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStdoutPrefixBeforeStderrMsg ok details

suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeStderrPrefixBeforeStdoutWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStderrPrefixBeforeStdoutDriverWrapper fakeStderrPrefixBeforeStdoutWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStderrPrefixBeforeStdoutWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2>&1"
                ]
            mergedOutput <- B.readFile "tmp.out"
            targetExists <- doesFileExist "tmp"
            let expectedChunk =
                    B.pack $
                        map
                            (fromIntegral . fromEnum)
                            "stderr: prefixstdout: note\nstderr: suffix\n"
                expectedOutput = B.concat $ replicate 2 expectedChunk
                ok = mergedOutput == expectedOutput && targetExists
                details = T.unlines
                    [ "mergedOutputBytes: " <> T.pack (show mergedOutput)
                    , "expectedOutputBytes: " <> T.pack (show expectedOutput)
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStderrPrefixBeforeStdoutMsg ok details

suppressWarnsRunAsmDropsWarningPreambleTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsWarningPreambleTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeWarningPreambleWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningPreambleDriverWrapper fakeWarningPreambleWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningPreambleWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsWarningPreambleMsg ok details

suppressWarnsRunAsmDropsAnsiWarningPreambleTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsAnsiWarningPreambleTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeAnsiWarningPreambleWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeAnsiWarningPreambleDriverWrapper fakeAnsiWarningPreambleWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeAnsiWarningPreambleWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let ok =
                    T.null stdoutLeak
                        && stderrLeak == "int x;\n"
                        && targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsAnsiWarningPreambleMsg ok details

suppressWarnsRunAsmDropsLeadingNoteTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsLeadingNoteTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeWarningLeadingNoteWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningLeadingNoteDriverWrapper fakeWarningLeadingNoteWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningLeadingNoteWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsLeadingNoteMsg ok details

suppressWarnsRunAsmDropsDriverContextTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsDriverContextTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeWarningContextWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningContextDriverWrapper fakeWarningContextWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningContextWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsDriverContextMsg ok details

suppressWarnsRunAsmDropsClangSnippetTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsClangSnippetTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeWarningSnippetWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningSnippetDriverWrapper fakeWarningSnippetWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningSnippetWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsClangSnippetMsg ok details

suppressWarnsRunAsmDropsMultiLineClangSnippetTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsMultiLineClangSnippetTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeMultiLineWarningSnippetWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeMultiLineWarningSnippetDriverWrapper fakeMultiLineWarningSnippetWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeMultiLineWarningSnippetWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsMultiLineClangSnippetMsg ok details

suppressWarnsRunAsmDropsSnippetOnlyContextTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsSnippetOnlyContextTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeSnippetOnlyWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeSnippetOnlyWarningDriverWrapper fakeSnippetOnlyWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeSnippetOnlyWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let ok =
                    T.null stdoutLeak
                        && stderrLeak == "int x;\n"
                        && targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsSnippetOnlyContextMsg ok details

suppressWarnsRunAsmDropsCrLfWarningBlockTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDropsCrLfWarningBlockTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeCrLfWarningWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeCrLfWarningDriverWrapper fakeCrLfWarningWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeCrLfWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmDropsCrLfWarningBlockMsg ok details

suppressWarnsRunAsmPreservesStandaloneLeadingNoteTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStandaloneLeadingNoteTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeStandaloneLeadingNoteWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStandaloneLeadingNoteDriverWrapper fakeStandaloneLeadingNoteWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStandaloneLeadingNoteWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && stderrLeak == "note: using fallback linker\n"
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStandaloneLeadingNoteMsg ok details

suppressWarnsRunAsmPreservesIndentedPostWarningStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesIndentedPostWarningStderrTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeIndentedPostWarningStderrWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeIndentedPostWarningStderrDriverWrapper fakeIndentedPostWarningStderrWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeIndentedPostWarningStderrWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && stderrLeak == "    cache hit: using wrapped assembler output\n"
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesIndentedPostWarningStderrMsg ok details

suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakePunctuatedPostWarningStderrWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writePunctuatedPostWarningStderrDriverWrapper fakePunctuatedPostWarningStderrWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakePunctuatedPostWarningStderrWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && stderrLeak
                            == "(cached result)\nstatus; retrying\ncache=hit;\nwrapper block {\nwrapper block }\n"
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmPreservesPunctuatedPostWarningStderrMsg
                    ok
                    details

suppressWarnsRunAsmPreservesDirectivePostWarningStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesDirectivePostWarningStderrTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeDirectivePostWarningStderrWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeDirectivePostWarningStderrDriverWrapper
                fakeDirectivePostWarningStderrWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeDirectivePostWarningStderrWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                expectedStderr = T.unlines
                    [ ".section keep"
                    , "# generated by fake HTCC_ASSEMBLER wrapper"
                    , "1 warning generated."
                    ]
                ok = T.null stdoutLeak && stderrLeak == expectedStderr && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmPreservesDirectivePostWarningStderrMsg
                    ok
                    details

suppressWarnsRunAsmPreservesStandalonePreSummaryNoteTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStandalonePreSummaryNoteTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeStandalonePreSummaryNoteWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStandalonePreSummaryNoteDriverWrapper fakeStandalonePreSummaryNoteWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeStandalonePreSummaryNoteWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && stderrLeak == "note: using fallback linker\n"
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStandalonePreSummaryNoteMsg ok details

suppressWarnsRunAsmPreservesStandalonePostWarningNoteTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStandalonePostWarningNoteTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakePostWarningNoteWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writePostWarningNoteDriverWrapper fakePostWarningNoteWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakePostWarningNoteWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && stderrLeak == "note: using fallback linker" && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStandalonePostWarningNoteMsg ok details

suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakePostWarningNoteWithoutSummaryWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writePostWarningNoteWithoutSummaryDriverWrapper fakePostWarningNoteWithoutSummaryWrapperPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakePostWarningNoteWithoutSummaryWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && stderrLeak == "note: using fallback linker"
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesStandalonePostWarningNoteWithoutSummaryMsg ok details

suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeErrorSnippetWarningTokenWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeErrorSnippetContainingWarningTokenDriverWrapper
                fakeErrorSnippetWarningTokenWrapperPath
                fakeAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeErrorSnippetWarningTokenWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let failed = exitCode (const True) False result
                hasError = "error: fake HTCC_ASSEMBLER failure" `T.isInfixOf` stderrLeak
                hasSnippet = "printf(\"warning:\");" `T.isInfixOf` stderrLeak
                hasCaret = "^~~~~~~~~~~~~~~~~~~" `T.isInfixOf` stderrLeak
                ok =
                    failed
                        && T.null stdoutLeak
                        && not targetExists
                        && hasError
                        && hasSnippet
                        && hasCaret
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasError: " <> T.pack (show hasError)
                    , "hasSnippet: " <> T.pack (show hasSnippet)
                    , "hasCaret: " <> T.pack (show hasCaret)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesErrorSnippetContainingWarningTokenMsg ok details

suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeWarningLabelErrorSnippetWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeWarningLabelErrorSnippetDriverWrapper
                fakeWarningLabelErrorSnippetWrapperPath
                fakeAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeWarningLabelErrorSnippetWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let failed = exitCode (const True) False result
                hasError = "error: fake HTCC_ASSEMBLER failure" `T.isInfixOf` stderrLeak
                hasSnippet = "warning: return 1;" `T.isInfixOf` stderrLeak
                hasCaret = "^~~~~~~~~~~~~~~~~" `T.isInfixOf` stderrLeak
                ok =
                    failed
                        && T.null stdoutLeak
                        && not targetExists
                        && hasError
                        && hasSnippet
                        && hasCaret
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasError: " <> T.pack (show hasError)
                    , "hasSnippet: " <> T.pack (show hasSnippet)
                    , "hasCaret: " <> T.pack (show hasCaret)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesWarningLabelErrorSnippetMsg ok details

suppressWarnsRunAsmPreservesFailingDiagnosticsTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesFailingDiagnosticsTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeFailingWarningWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeFailingWarningDriverWrapper fakeFailingWarningWrapperPath fakeAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeFailingWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let failed = exitCode (const True) False result
                hasWarning = "warning: fake HTCC_ASSEMBLER warning" `T.isInfixOf` stderrLeak
                hasPreamble = "In file included from fake-header.h:1:" `T.isInfixOf` stderrLeak
                hasAssemblerError = "error: fake HTCC_ASSEMBLER failure" `T.isInfixOf` stderrLeak
                hasFailure = "failed with ExitFailure 1" `T.isInfixOf` stderrLeak
                ok =
                    failed
                        && T.null stdoutLeak
                        && not targetExists
                        && not hasWarning
                        && hasPreamble
                        && hasAssemblerError
                        && hasFailure
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasWarning: " <> T.pack (show hasWarning)
                    , "hasPreamble: " <> T.pack (show hasPreamble)
                    , "hasAssemblerError: " <> T.pack (show hasAssemblerError)
                    , "hasFailure: " <> T.pack (show hasFailure)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesFailingDiagnosticsMsg ok details

suppressWarnsRunAsmPreservesErrorPreambleAfterWarningTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesErrorPreambleAfterWarningTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeErrorPreambleAfterWarningWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeErrorPreambleAfterWarningDriverWrapper
                fakeErrorPreambleAfterWarningWrapperPath
                fakeAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeErrorPreambleAfterWarningWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            let failed = exitCode (const True) False result
                hasWarning = "warning: fake HTCC_ASSEMBLER warning" `T.isInfixOf` stderrLeak
                hasFunctionPreamble = "fake-source.c: In function 'main':" `T.isInfixOf` stderrLeak
                hasAssemblerPreamble = "{standard input}: Assembler messages:" `T.isInfixOf` stderrLeak
                hasAssemblerError = "error: fake HTCC_ASSEMBLER failure" `T.isInfixOf` stderrLeak
                hasFailure = "failed with ExitFailure 1" `T.isInfixOf` stderrLeak
                ok =
                    failed
                        && T.null stdoutLeak
                        && not targetExists
                        && not hasWarning
                        && hasFunctionPreamble
                        && hasAssemblerPreamble
                        && hasAssemblerError
                        && hasFailure
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hasWarning: " <> T.pack (show hasWarning)
                    , "hasFunctionPreamble: " <> T.pack (show hasFunctionPreamble)
                    , "hasAssemblerPreamble: " <> T.pack (show hasAssemblerPreamble)
                    , "hasAssemblerError: " <> T.pack (show hasAssemblerError)
                    , "hasFailure: " <> T.pack (show hasFailure)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult suppressWarnsRunAsmPreservesErrorPreambleAfterWarningMsg ok details

suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeInheritedPipeHandlesWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeInheritedPipeHandlesDriverWrapper fakeInheritedPipeHandlesWrapperPath fakeAssemblerPath
            maybeResult <- timeout 4000000 $
                exec $ mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeInheritedPipeHandlesWrapperPath
                    , "' "
                    , htccCmd
                    , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            targetExists <- doesFileExist "tmp"
            let completed = maybe False (exitCode (const False) True) maybeResult
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ok =
                    completed
                        && T.null stdoutLeak
                        && T.null stderrLeak
                        && compilerSawExpectedArgs
                        && targetExists
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "targetExists: " <> T.pack (show targetExists)
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmDoesNotHangOnInheritedPipeHandlesMsg
                    ok
                    details

suppressWarnsRunAsmStreamsRetainedStdoutPromptTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmStreamsRetainedStdoutPromptTest =
    flip finally
        (clean
            [ "tmp"
            , fakeStreamingPromptWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStreamingPromptDriverWrapper fakeStreamingPromptWrapperPath fakeAssemblerPath
            let command = mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeStreamingPromptWrapperPath
                    , "' "
                    , htccCmd
                    , " --suppress-warns -r -o tmp /dev/stdin"
                    ]
            withCreateProcess
                ((proc "sh" ["-c", T.unpack command]) { std_out = CreatePipe, std_err = CreatePipe })
                (\_ maybeStdoutHandle maybeStderrHandle processHandle -> do
                    stdoutHandle <- maybe (ioError $ userError "missing stdout pipe for streaming prompt test") pure maybeStdoutHandle
                    stderrHandle <- maybe (ioError $ userError "missing stderr pipe for streaming prompt test") pure maybeStderrHandle
                    maybePromptLine <- timeout 900000 $ T.hGetLine stdoutHandle
                    processExitCode <- waitForProcess processHandle
                    remainingStdout <- T.hGetContents stdoutHandle
                    remainingStderr <- T.hGetContents stderrHandle
                    compilerArgs <- catchIOError
                        (T.lines <$> T.readFile fakeAssemblerLogPath)
                        (const $ pure [])
                    targetExists <- doesFileExist "tmp"
                    let compilerSawExpectedArgs =
                            all (`elem` compilerArgs)
                                [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                ]
                        ok =
                            maybePromptLine == Just "stdout: wrapper prompt"
                                && processExitCode == ExitSuccess
                                && T.null remainingStdout
                                && T.null remainingStderr
                                && compilerSawExpectedArgs
                                && targetExists
                        details = T.unlines
                            [ "promptLine: " <> fromMaybe "<timeout>" maybePromptLine
                            , "remainingStdout:"
                            , remainingStdout
                            , "remainingStderr:"
                            , remainingStderr
                            , "compilerArgs:"
                            , T.unlines compilerArgs
                            , "targetExists: " <> T.pack (show targetExists)
                            , "processExitCode: " <> T.pack (show processExitCode)
                            ]
                    pure $
                        mkResult
                            suppressWarnsRunAsmStreamsRetainedStdoutPromptMsg
                            ok
                            details
                )

suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineTest =
    flip finally
        (clean
            [ "tmp"
            , fakeStreamingPromptWithoutNewlineWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeStreamingPromptWithoutNewlineDriverWrapper
                fakeStreamingPromptWithoutNewlineWrapperPath
                fakeAssemblerPath
            let command = mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeStreamingPromptWithoutNewlineWrapperPath
                    , "' "
                    , htccCmd
                    , " --suppress-warns -r -o tmp /dev/stdin"
                    ]
                expectedPromptBytes = asciiBytesForProbe "stdout: wrapper prompt"
            withCreateProcess
                ((proc "sh" ["-c", T.unpack command]) { std_out = CreatePipe, std_err = CreatePipe })
                (\_ maybeStdoutHandle maybeStderrHandle processHandle -> do
                    stdoutHandle <- maybe (ioError $ userError "missing stdout pipe for unterminated streaming prompt test") pure maybeStdoutHandle
                    stderrHandle <- maybe (ioError $ userError "missing stderr pipe for unterminated streaming prompt test") pure maybeStderrHandle
                    maybePromptBytes <- timeout 900000 $ B.hGetSome stdoutHandle (B.length expectedPromptBytes)
                    processExitCode <- waitForProcess processHandle
                    remainingStdout <- B.hGetContents stdoutHandle
                    remainingStderr <- T.hGetContents stderrHandle
                    compilerArgs <- catchIOError
                        (T.lines <$> T.readFile fakeAssemblerLogPath)
                        (const $ pure [])
                    targetExists <- doesFileExist "tmp"
                    let compilerSawExpectedArgs =
                            all (`elem` compilerArgs)
                                [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                ]
                        ok =
                            maybePromptBytes == Just expectedPromptBytes
                                && processExitCode == ExitSuccess
                                && B.null remainingStdout
                                && T.null remainingStderr
                                && compilerSawExpectedArgs
                                && targetExists
                        details = T.unlines
                            [ "promptBytes: "
                                <> maybe "<timeout>" (T.pack . BC.unpack) maybePromptBytes
                            , "remainingStdoutBytes: " <> T.pack (show remainingStdout)
                            , "remainingStderr:"
                            , remainingStderr
                            , "compilerArgs:"
                            , T.unlines compilerArgs
                            , "targetExists: " <> T.pack (show targetExists)
                            , "processExitCode: " <> T.pack (show processExitCode)
                            ]
                    pure $
                        mkResult
                            suppressWarnsRunAsmStreamsRetainedStdoutPromptWithoutNewlineMsg
                            ok
                            details
                )

suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrTest =
    flip finally
        (clean
            [ "tmp"
            , fakeCrossStreamStreamingRetainedLineWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeCrossStreamStreamingRetainedLineDriverWrapper
                fakeCrossStreamStreamingRetainedLineWrapperPath
                fakeAssemblerPath
            let command = mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeCrossStreamStreamingRetainedLineWrapperPath
                    , "' "
                    , htccCmd
                    , " --suppress-warns -r -o tmp /dev/stdin"
                    ]
            withCreateProcess
                ((proc "sh" ["-c", T.unpack command]) { std_out = CreatePipe, std_err = CreatePipe })
                (\_ maybeStdoutHandle maybeStderrHandle processHandle -> do
                    stdoutHandle <- maybe (ioError $ userError "missing stdout pipe for cross-stream streaming test") pure maybeStdoutHandle
                    stderrHandle <- maybe (ioError $ userError "missing stderr pipe for cross-stream streaming test") pure maybeStderrHandle
                    maybeRetainedLine <- timeout 700000 $ T.hGetLine stdoutHandle
                    earlyExit <- timeout 100000 $ waitForProcess processHandle
                    processExitCode <- maybe (waitForProcess processHandle) pure earlyExit
                    remainingStdout <- T.hGetContents stdoutHandle
                    remainingStderr <- T.hGetContents stderrHandle
                    compilerArgs <- catchIOError
                        (T.lines <$> T.readFile fakeAssemblerLogPath)
                        (const $ pure [])
                    targetExists <- doesFileExist "tmp"
                    let compilerSawExpectedArgs =
                            all (`elem` compilerArgs)
                                [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                ]
                        ok =
                            maybeRetainedLine == Just "stdout: retained line"
                                && isNothing earlyExit
                                && processExitCode == ExitSuccess
                                && T.null remainingStdout
                                && remainingStderr == "stderr: wrapper prompt\n"
                                && compilerSawExpectedArgs
                                && targetExists
                        details = T.unlines
                            [ "retainedLine: " <> fromMaybe "<timeout>" maybeRetainedLine
                            , "earlyExit: " <> maybe "still-running" (T.pack . show) earlyExit
                            , "remainingStdout:"
                            , remainingStdout
                            , "remainingStderr:"
                            , remainingStderr
                            , "compilerArgs:"
                            , T.unlines compilerArgs
                            , "targetExists: " <> T.pack (show targetExists)
                            , "processExitCode: " <> T.pack (show processExitCode)
                            ]
                    pure $
                        mkResult
                            suppressWarnsRunAsmStreamsRetainedStdoutAcrossPendingStderrMsg
                            ok
                            details
                )

suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeProbeCrossStreamWarningPreambleWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeProbeCrossStreamWarningPreambleDriverWrapper
                fakeProbeCrossStreamWarningPreambleWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeProbeCrossStreamWarningPreambleWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmSuppressesProbeCrossStreamWarningPreambleMsg
                    ok
                    details

suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.err"
            , "tmp.out"
            , fakeProbePartialCrossStreamWarningPreambleWrapperPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeProbePartialCrossStreamWarningPreambleDriverWrapper
                fakeProbePartialCrossStreamWarningPreambleWrapperPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeProbePartialCrossStreamWarningPreambleWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            result <- exec "./tmp"
            let ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && T.null stderrLeak && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmSuppressesProbePartialCrossStreamWarningPreambleMsg
                    ok
                    details

suppressWarnsRunAsmProbeClosesStdinTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmProbeClosesStdinTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.c"
            , "tmp.err"
            , "tmp.out"
            , "tmp-probe-reads-stdin.fifo"
            , fakeProbeReadsStdinWrapperPath
            , fakeProbeReadsStdinWrapperLogPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeProbeReadsStdinWrapper
                fakeProbeReadsStdinWrapperPath
                fakeProbeReadsStdinWrapperLogPath
                fakeAssemblerPath
            T.writeFile "tmp.c" source
            maybeResult <- timeout 1500000 $
                exec $ mconcat
                    [ "rm -f tmp-probe-reads-stdin.fifo;"
                    , " mkfifo tmp-probe-reads-stdin.fifo;"
                    , " (sleep 2 > tmp-probe-reads-stdin.fifo) & writer=$!;"
                    , " HTCC_ASSEMBLER='./"
                    , T.pack fakeProbeReadsStdinWrapperPath
                    , "' "
                    , htccCmd
                    , " --suppress-warns -r -o tmp tmp.c"
                    , " < tmp-probe-reads-stdin.fifo > tmp.out 2> tmp.err;"
                    , " status=$?;"
                    , " kill $writer 2>/dev/null || true;"
                    , " wait $writer 2>/dev/null || true;"
                    , " rm -f tmp-probe-reads-stdin.fifo;"
                    , " exit $status"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            wrapperInvocations <- catchIOError
                (T.lines <$> T.readFile fakeProbeReadsStdinWrapperLogPath)
                (const $ pure [])
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            targetExists <- doesFileExist "tmp"
            let completed = maybe False (exitCode (const False) True) maybeResult
                sawProbeAttempt =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ok =
                    completed
                        && T.null stdoutLeak
                        && T.null stderrLeak
                        && sawProbeAttempt
                        && compilerSawExpectedArgs
                        && targetExists
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "sawProbeAttempt: " <> T.pack (show sawProbeAttempt)
                    , "compilerSawExpectedArgs: " <> T.pack (show compilerSawExpectedArgs)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $ mkResult suppressWarnsRunAsmProbeClosesStdinMsg ok details

suppressWarnsRunAsmPreservesStdinForRealInvocationsTest :: IO (Either T.Text T.Text, String)
suppressWarnsRunAsmPreservesStdinForRealInvocationsTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.c"
            , "tmp.err"
            , "tmp.out"
            , fakeRealInvocationReadsStdinWrapperPath
            , fakeRealInvocationReadsStdinWrapperLogPath
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeRealInvocationReadsStdinWrapper
                fakeRealInvocationReadsStdinWrapperPath
                fakeRealInvocationReadsStdinWrapperLogPath
                fakeAssemblerPath
            T.writeFile "tmp.c" source
            result <- exec $ mconcat
                [ "printf '%s\\n' 'wrapper-token' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeRealInvocationReadsStdinWrapperPath
                , "' "
                , htccCmd
                , " --suppress-warns -r -o tmp tmp.c > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrLeak <- T.readFile "tmp.err"
            wrapperLog <- catchIOError
                (T.lines <$> T.readFile fakeRealInvocationReadsStdinWrapperLogPath)
                (const $ pure [])
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            targetExists <- doesFileExist "tmp"
            runResult <- if targetExists then Just <$> exec "./tmp" else pure Nothing
            let succeeded = exitCode (const False) True result
                sawWrapperToken = "stdin:wrapper-token" `elem` wrapperLog
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                targetRuns = maybe False (exitCode (const False) True) runResult
                ok =
                    succeeded
                        && T.null stdoutLeak
                        && T.null stderrLeak
                        && sawWrapperToken
                        && compilerSawExpectedArgs
                        && targetExists
                        && targetRuns
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrLeak
                    , "wrapperLog:"
                    , T.unlines wrapperLog
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "sawWrapperToken: " <> T.pack (show sawWrapperToken)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "compileExitCode: " <> T.pack (show result)
                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                    ]
            return $
                mkResult
                    suppressWarnsRunAsmPreservesStdinForRealInvocationsMsg
                    ok
                    details

outputFileSingleInputStaticTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputStaticTest = flip finally (clean ["tmp.out", "tmp.s", "tmp.o", "tmp-single.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-single.c" $ T.unlines
        [ "static int helper;"
        , "int main() { helper = 1; return helper; }"
        ]
    execErrFin $ mconcat
        [ htccCmd
        , " -o tmp.s tmp-single.c > tmp.out"
        ]
    assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasOriginalStaticLabel = "\nhelper:" `T.isInfixOf` asm
        hasNamespacedStaticLabel = ".L.internal." `T.isInfixOf` asm
        ok = T.null stdoutLeak && hasOriginalStaticLabel && not hasNamespacedStaticLabel
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasOriginalStaticLabel: " <> T.pack (show hasOriginalStaticLabel)
            , "hasNamespacedStaticLabel: " <> T.pack (show hasNamespacedStaticLabel)
            ]
    return $ mkResult outputFileSingleInputStaticMsg ok details

outputFileSingleInputImplicitFunctionTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputImplicitFunctionTest = flip finally (clean ["tmp.out", "tmp.s", "tmp-single.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-single.c" $
        T.unlines
            [ "int main() { return foo(); }"
            ]
    execErrFin $ mconcat
        [ htccCmd
        , " -o tmp.s tmp-single.c > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasMainLabel = "\nmain:" `T.isInfixOf` asm
        hasFooCall =
            any (`T.isInfixOf` asm)
                [ "call foo"
                , "call \"foo\""
                ]
        ok = T.null stdoutLeak && hasMainLabel && hasFooCall
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasMainLabel: " <> T.pack (show hasMainLabel)
            , "hasFooCall: " <> T.pack (show hasFooCall)
            ]
    return $ mkResult outputFileSingleInputImplicitFunctionMsg ok details

outputFileSingleInputImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputImplicitFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        T.writeFile inputPath $ T.unlines
            [ "int foo;"
            , "int main(void) { return foo(); }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSingleInputImplicitFunctionConflictMsg ok details

outputFileSingleInputStaticImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputStaticImplicitFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        T.writeFile inputPath $ T.unlines
            [ "static int foo;"
            , "int main(void) { return foo(); }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSingleInputStaticImplicitFunctionConflictMsg ok details

outputFileSingleInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile inputPath $ T.unlines
            [ "int main(void) { return foo(); }"
            , "int foo(void) { return 1; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && hasExpectedWarning
                    && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSingleInputImplicitFunctionDefinitionWarningMsg ok details

outputFileSingleInputImplicitFunctionPrototypeWarningTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputImplicitFunctionPrototypeWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile inputPath $ T.unlines
            [ "int main(void) { return foo(); }"
            , "int foo(void);"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasMainLabel = "main:" `T.isInfixOf` asm
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && hasExpectedWarning
                    && hasMainLabel
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasMainLabel: " <> T.pack (show hasMainLabel)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSingleInputImplicitFunctionPrototypeWarningMsg ok details

outputFileSingleInputPrototypeRetypeTest :: IO (Either T.Text T.Text, String)
outputFileSingleInputPrototypeRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedError = "too many arguments to function call"
        T.writeFile inputPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return foo(1); }"
            , "int foo(void) { return 0; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSingleInputPrototypeRetypeMsg ok details

stdoutSingleInputImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
stdoutSingleInputImplicitFunctionConflictTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-single.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        T.writeFile inputPath $ T.unlines
            [ "int foo;"
            , "int main(void) { return foo(); }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack inputPath
            , " > "
            , T.pack target
            , " 2> tmp.err"
            ]
        asm <- T.readFile target
        stderrOut <- T.readFile "tmp.err"
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null asm && hasExpectedError
            details = T.unlines
                [ "asm:"
                , asm
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutSingleInputImplicitFunctionConflictMsg ok details

stdoutMultiInputStaticFunctionTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputStaticFunctionTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp.o", "tmp-foo.c", "tmp-bar.c"]) $ do
        htccCmd <- htccCommand
        T.writeFile "tmp-foo.c" $ T.unlines
            [ "static int helper(void) { return 1; }"
            , "int foo(void) { return helper(); }"
            ]
        T.writeFile "tmp-bar.c" $ T.unlines
            [ "static int helper(void) { return 2; }"
            , "int bar(void) { return helper(); }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " tmp-foo.c tmp-bar.c > tmp.s 2> tmp.err"
            ]
        assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile "tmp.s"
        let succeeded = exitCode (const False) True result
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , ".L.return.foo:"
                    , "bar:"
                    , ".L.return.bar:"
                    , ".L.internal.0.helper:"
                    , ".L.internal.1.helper:"
                    ]
            hasOriginalHelperLabel = "\nhelper:" `T.isInfixOf` asm
            ok = succeeded && T.null stderrOut && hasRequiredLabels && not hasOriginalHelperLabel
            details = T.unlines
                [ "stderr:"
                , stderrOut
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "hasOriginalHelperLabel: " <> T.pack (show hasOriginalHelperLabel)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputStaticFunctionMsg ok details

stdoutMultiInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath "int main(void) { return foo(1) != 1; }"
        T.writeFile defPath "int foo(int x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.s 2> tmp.err"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile "tmp.s"
        runResult <- exec "./tmp"
        let succeeded = exitCode (const False) True result
            warningSuppressed = not $ expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && warningSuppressed
                    && hasRequiredLabels
                    && exitCode (const False) True runResult
            details = T.unlines
                [ "stderr:"
                , stderrOut
                , "warningSuppressed: " <> T.pack (show warningSuppressed)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputImplicitFunctionDefinitionWarningMsg ok details

stdoutMultiInputSameInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputSameInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath $ T.unlines
            [ "int main(void) { return foo(); }"
            , "int foo(void) { return 1; }"
            ]
        T.writeFile otherPath "int helper(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.s 2> tmp.err"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile "tmp.s"
        runResult <- exec "./tmp"
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "helper:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && hasExpectedWarning
                    && hasRequiredLabels
                    && exitCode (const False) True runResult
            details = T.unlines
                [ "stderr:"
                , stderrOut
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputSameInputImplicitFunctionDefinitionWarningMsg ok details

stdoutMultiInputImplicitFunctionUnresolvedWarningTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputImplicitFunctionUnresolvedWarningTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c"]) $ do
        htccCmd <- htccCommand
        let callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
            unexpectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath "int main(void) { return foo(1) != 1; }"
        T.writeFile otherPath "int helper(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.s 2> tmp.err"
            ]
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile "tmp.s"
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = unexpectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "helper:"
                    , "main:"
                    ]
            ok = succeeded && hasExpectedWarning && hasRequiredLabels
            details = T.unlines
                [ "stderr:"
                , stderrOut
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputImplicitFunctionUnresolvedWarningMsg ok details

stdoutMultiInputParseFailurePreservesWarningsTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputParseFailurePreservesWarningsTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-first.c", "tmp-second.c", "tmp-bad.c"]) $ do
        htccCmd <- htccCommand
        let firstPath = "tmp-first.c"
            secondPath = "tmp-second.c"
            badPath = "tmp-bad.c"
            firstWarning = "warning: the function 'foo' is not declared."
            secondWarning = "warning: the function 'bar' is not declared."
        T.writeFile firstPath "int first(void) { return foo(1) != 1; }"
        T.writeFile secondPath "int second(void) { return bar(2) != 2; }"
        T.writeFile badPath "int broken( { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack firstPath
            , " "
            , T.pack secondPath
            , " "
            , T.pack badPath
            , " > tmp.s 2> tmp.err"
            ]
        asm <- T.readFile "tmp.s"
        stderrOut <- T.readFile "tmp.err"
        let failed = exitCode (const True) False result
            hasExpectedWarnings =
                all (`T.isInfixOf` stderrOut) [firstWarning, secondWarning]
            warningsInOrder = containsTextsInOrder [firstWarning, secondWarning] stderrOut
            mentionsBadInput = T.pack badPath `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null asm
                    && hasExpectedWarnings
                    && warningsInOrder
                    && mentionsBadInput
            details = T.unlines
                [ "asm:"
                , asm
                , "stderr:"
                , stderrOut
                , "hasExpectedWarnings: " <> T.pack (show hasExpectedWarnings)
                , "warningsInOrder: " <> T.pack (show warningsInOrder)
                , "mentionsBadInput: " <> T.pack (show mentionsBadInput)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputParseFailurePreservesWarningsMsg ok details

stdoutMultiInputPrototypeOnlyArityRetypeTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputPrototypeOnlyArityRetypeTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "too few arguments to function call"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return foo(); }"
            ]
        T.writeFile protoPath "int foo(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > "
            , T.pack target
            , " 2> tmp.err"
            ]
        asm <- T.readFile target
        stderrOut <- T.readFile "tmp.err"
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null asm && hasExpectedError
            details = T.unlines
                [ "asm:"
                , asm
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputPrototypeOnlyArityRetypeMsg ok details

stdoutMultiInputParameterIndirectFunctionPointerArityRetypeTest :: IO (Either T.Text T.Text, String)
stdoutMultiInputParameterIndirectFunctionPointerArityRetypeTest =
    flip finally (clean ["tmp.err", "tmp.s", "tmp-def.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            defPath = "tmp-def.c"
            protoPath = "tmp-proto.c"
            expectedError = "too many arguments to function call"
        T.writeFile defPath $ T.unlines
            [ "int g(int (*fp)()) { return fp(1, 2); }"
            , "int main(void) { return 0; }"
            ]
        T.writeFile protoPath "int g(int (*fp)(int));"
        result <- exec $ mconcat
            [ htccCmd
            , " "
            , T.pack defPath
            , " "
            , T.pack protoPath
            , " > "
            , T.pack target
            , " 2> tmp.err"
            ]
        asm <- T.readFile target
        stderrOut <- T.readFile "tmp.err"
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null asm && hasExpectedError
            details = T.unlines
                [ "asm:"
                , asm
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult stdoutMultiInputParameterIndirectFunctionPointerArityRetypeMsg ok details

outputFileMultiInputTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTest = flip finally (clean ["tmp.out", "tmp.s", "tmp.o", "tmp-foo.c", "tmp-bar.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-foo.c" "char* foo() { return \"foo\"; }"
    T.writeFile "tmp-bar.c" "char* bar() { return \"bar\"; }"
    execErrFin $ mconcat
        [ htccCmd
        , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
        ]
    assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasRequiredLabels =
            all (`T.isInfixOf` asm)
                [ "foo:"
                , ".L.return.foo:"
                , "bar:"
                , ".L.return.bar:"
                , ".L.data.0:"
                , ".L.data.1:"
                ]
        hasUniqueLiteralLabels =
            T.count ".L.data.0:" asm == 1
                && T.count ".L.data.1:" asm == 1
        ok = T.null stdoutLeak && hasRequiredLabels && hasUniqueLiteralLabels
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
            , "hasUniqueLiteralLabels: " <> T.pack (show hasUniqueLiteralLabels)
            ]
    return $ mkResult outputFileMultiInputMsg ok details

outputFileMultiInputFunctionDeclarationConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionDeclarationConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-global.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            globalPath = "tmp-global.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, declPath, globalPath]
        T.writeFile declPath $ T.unlines
            [ "int foo(void);"
            , "int main() { return foo(); }"
            ]
        T.writeFile globalPath "int foo;"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack globalPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionDeclarationConflictMsg ok details

outputFileMultiInputImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-global.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            globalPath = "tmp-global.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, callerPath, globalPath]
        T.writeFile callerPath "int main() { return foo(); }"
        T.writeFile globalPath "int foo;"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack globalPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionConflictMsg ok details

outputFileMultiInputConflictPreservesWarningsTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputConflictPreservesWarningsTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-dup-a.c", "tmp-dup-b.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            dupAPath = "tmp-dup-a.c"
            dupBPath = "tmp-dup-b.c"
            expectedWarning = "warning: the function 'bar' is not declared."
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, callerPath, dupAPath, dupBPath]
        T.writeFile callerPath "int main(void) { return bar(); }"
        T.writeFile dupAPath "int foo = 1;"
        T.writeFile dupBPath "int foo = 2;"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack dupAPath
            , " "
            , T.pack dupBPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedWarning
                    && hasExpectedError
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputConflictPreservesWarningsMsg ok details

outputFileMultiInputReadFailurePreservesWarningsTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputReadFailurePreservesWarningsTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-first.c", "tmp-second.c", "tmp-missing.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            firstPath = "tmp-first.c"
            secondPath = "tmp-second.c"
            missingPath = "tmp-missing.c"
            firstWarning = "warning: the function 'foo' is not declared."
            secondWarning = "warning: the function 'bar' is not declared."
        clean ["tmp.out", "tmp.err", target, firstPath, secondPath, missingPath]
        T.writeFile target "stale output"
        T.writeFile firstPath "int first(void) { return foo(1) != 1; }"
        T.writeFile secondPath "int second(void) { return bar(2) != 2; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack firstPath
            , " "
            , T.pack secondPath
            , " "
            , T.pack missingPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            hasExpectedWarnings =
                all (`T.isInfixOf` stderrOut) [firstWarning, secondWarning]
            warningsInOrder = containsTextsInOrder [firstWarning, secondWarning] stderrOut
            mentionsMissingInput = T.pack missingPath `T.isInfixOf` stderrOut
            hasMissingInputDiagnostic = "no such file" `T.isInfixOf` T.toLower stderrOut
            hidesCallStack = not $ "HasCallStack" `T.isInfixOf` stderrOut
            hidesWithFile = not $ "withFile" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && hasExpectedWarnings
                    && warningsInOrder
                    && mentionsMissingInput
                    && hasMissingInputDiagnostic
                    && hidesCallStack
                    && hidesWithFile
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "hasExpectedWarnings: " <> T.pack (show hasExpectedWarnings)
                , "warningsInOrder: " <> T.pack (show warningsInOrder)
                , "mentionsMissingInput: " <> T.pack (show mentionsMissingInput)
                , "hasMissingInputDiagnostic: " <> T.pack (show hasMissingInputDiagnostic)
                , "hidesCallStack: " <> T.pack (show hidesCallStack)
                , "hidesWithFile: " <> T.pack (show hidesWithFile)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputReadFailurePreservesWarningsMsg ok details

outputFileMultiInputImplicitFunctionTypeConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionTypeConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, callerPath, defPath]
        T.writeFile callerPath "int main() { return foo(); }"
        T.writeFile defPath "long foo(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionTypeConflictMsg ok details

outputFileMultiInputImplicitFunctionTypeConflictReverseOrderTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionTypeConflictReverseOrderTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, callerPath, defPath]
        T.writeFile defPath "long foo(void) { return 0; }"
        T.writeFile callerPath "int main() { return foo(); }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack defPath
            , " "
            , T.pack callerPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionTypeConflictReverseOrderMsg ok details

outputFileMultiInputFunctionTypeConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionTypeConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, declPath, defPath]
        T.writeFile declPath $ T.unlines
            [ "int foo(void);"
            , "int main() { return foo(); }"
            ]
        T.writeFile defPath "long foo(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionTypeConflictMsg ok details

outputFileMultiInputFunctionPointerRedeclarationConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionPointerRedeclarationConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external declarations in multi-input -o mode: p"
        clean ["tmp.out", "tmp.err", target, declPath, defPath]
        T.writeFile declPath $ T.unlines
            [ "int *p;"
            , "int main(void) { return p != 0; }"
            ]
        T.writeFile defPath "int (*p)(void);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionPointerRedeclarationConflictMsg ok details

outputFileMultiInputAdjustedFunctionParamTypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputAdjustedFunctionParamTypeTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
        clean ["tmp.out", target, declPath, defPath]
        T.writeFile declPath $ T.unlines
            [ "int take_array(int xs[]);"
            , "int take_cb(int cb());"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath $ T.unlines
            [ "int take_array(int *xs) { return 0; }"
            , "int take_cb(int (*cb)()) { return 0; }"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile target
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "take_array:"
                    , "take_cb:"
                    , "main:"
                    ]
            ok = T.null stdoutLeak && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                ]
        return $ mkResult outputFileMultiInputAdjustedFunctionParamTypeMsg ok details

outputFileMultiInputCompatiblePrototypeMergeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputCompatiblePrototypeMergeTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.s", "tmp-use.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            protoPath = "tmp-proto.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: f"
        clean ["tmp", "tmp.out", target, usePath, protoPath]
        T.writeFile usePath $ T.unlines
            [ "int (*f(void))[4];"
            , "int main(void) { return sizeof *f(); }"
            ]
        T.writeFile protoPath "int (*f(void))[];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputCompatiblePrototypeMergeMsg ok details

outputFileMultiInputLargeStructReturnMergeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputLargeStructReturnMergeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-def.c", "tmp-decl.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            defPath = "tmp-def.c"
            declPath = "tmp-decl.c"
            expectedError = "unsupported by-value function return type"
        clean ["tmp.out", "tmp.err", target, defPath, declPath]
        T.writeFile defPath "struct S f(void) { return; }"
        T.writeFile declPath $ T.unlines
            [ "struct S { int a; int b; int c; };"
            , "struct S f(void);"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack defPath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputLargeStructReturnMergeMsg ok details

outputFileMultiInputRepeatedPrototypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputRepeatedPrototypeTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
        T.writeFile declPath $ T.unlines
            [ "int foo(void);"
            , "int foo(void);"
            , "int main() { return foo() != 42; }"
            ]
        T.writeFile defPath "int foo(void) { return 42; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            fooLabelCount = T.count "\nfoo:" $ "\n" <> asm
            ok = T.null stdoutLeak && hasRequiredLabels && fooLabelCount == 1
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "fooLabelCount: " <> T.pack (show fooLabelCount)
                ]
        return $ mkResult outputFileMultiInputRepeatedPrototypeMsg ok details

outputFileMultiInputTaggedPrototypeScopeMergeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTaggedPrototypeScopeMergeTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-use.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            defPath = "tmp-def.c"
        clean ["tmp.out", target, usePath, defPath]
        T.writeFile usePath $ T.unlines
            [ "int g(void);"
            , "int f(struct S { int x; } a);"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int f(struct S { int x; } a) { return 0; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile target
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "f:"
                    , "main:"
                    ]
            ok = T.null stdoutLeak && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                ]
        return $ mkResult outputFileMultiInputTaggedPrototypeScopeMergeMsg ok details

outputFileMultiInputPrototypeOnlyArityRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputPrototypeOnlyArityRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "too few arguments to function call"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return foo(); }"
            ]
        T.writeFile protoPath "int foo(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputPrototypeOnlyArityRetypeMsg ok details

outputFileMultiInputSignedIntRedeclarationTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSignedIntRedeclarationTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-decl.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
        T.writeFile declPath $ T.unlines
            [ "int foo(void);"
            , "int x;"
            , "int main(void) { return foo() != 42 || x != 0; }"
            ]
        T.writeFile defPath $ T.unlines
            [ "signed foo(void) { return 42; }"
            , "signed x;"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        result <- exec "./tmp"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "x:"
                    , "main:"
                    ]
            fooLabelCount = T.count "\nfoo:" $ "\n" <> asm
            xLabelCount = T.count "\nx:" $ "\n" <> asm
            ok =
                T.null stdoutLeak
                    && exitCode (const False) True result
                    && hasRequiredLabels
                    && fooLabelCount == 1
                    && xLabelCount == 1
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "fooLabelCount: " <> T.pack (show fooLabelCount)
                , "xLabelCount: " <> T.pack (show xLabelCount)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputSignedIntRedeclarationMsg ok details

outputFileMultiInputOldStyleDeclarationTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputOldStyleDeclarationTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-decl.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
        T.writeFile declPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return foo(1) != 1; }"
            ]
        T.writeFile defPath "int foo(int x) { return x; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        result <- exec "./tmp"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok = T.null stdoutLeak && exitCode (const False) True result && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputOldStyleDeclarationMsg ok details

outputFileMultiInputOldStylePromotionConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputOldStylePromotionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, declPath, defPath]
        T.writeFile declPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int foo(char x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputOldStylePromotionConflictMsg ok details

outputFileMultiInputVoidPrototypeConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputVoidPrototypeConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: foo"
        clean ["tmp.out", "tmp.err", target, declPath, defPath]
        T.writeFile declPath $ T.unlines
            [ "int foo(void);"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int foo(int x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputVoidPrototypeConflictMsg ok details

outputFileMultiInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath "int main(void) { return foo(1) != 1; }"
        T.writeFile defPath "int foo(int x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        linkCmd <- assemblerCommand [target, "-o", "tmp"]
        execErrFin linkCmd
        runResult <- exec "./tmp"
        let succeeded = exitCode (const False) True result
            warningSuppressed = not $ expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && warningSuppressed
                    && hasRequiredLabels
                    && exitCode (const False) True runResult
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "warningSuppressed: " <> T.pack (show warningSuppressed)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionDefinitionWarningMsg ok details

outputFileMultiInputImplicitFunctionPrototypeWarningTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionPrototypeWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath "int main(void) { return foo(1) != 1; }"
        T.writeFile protoPath "int foo(int x);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            warningSuppressed = not $ expectedWarning `T.isInfixOf` stderrOut
            hasRequiredAsm =
                all (`T.isInfixOf` asm)
                    [ "main:"
                    , "call foo"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && warningSuppressed
                    && hasRequiredAsm
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "warningSuppressed: " <> T.pack (show warningSuppressed)
                , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionPrototypeWarningMsg ok details

outputFileMultiInputBlockScopeExternPrototypeWarningTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputBlockScopeExternPrototypeWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath $ T.unlines
            [ "int main(void) { return foo(1) != 1; }"
            , "int probe(void) { extern int foo(int); return 0; }"
            ]
        T.writeFile defPath "int foo(int x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        linkCmd <- assemblerCommand [target, "-o", "tmp"]
        execErrFin linkCmd
        runResult <- exec "./tmp"
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "probe:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && hasExpectedWarning
                    && hasRequiredLabels
                    && exitCode (const False) True runResult
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputBlockScopeExternPrototypeWarningMsg ok details

outputFileMultiInputImplicitFunctionDefinitionTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionDefinitionTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
        T.writeFile callerPath "int main(void) { return foo(1) != 1; }"
        T.writeFile defPath "int foo(int x) { return x; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        result <- exec "./tmp"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok = T.null stdoutLeak && exitCode (const False) True result && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionDefinitionMsg ok details

outputFileMultiInputBlockScopeExternPrototypeDefinitionTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputBlockScopeExternPrototypeDefinitionTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
        T.writeFile callerPath "int main(void) { extern char foo(void); return foo(); }"
        T.writeFile defPath "char foo(void) { return 0; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        result <- exec "./tmp"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            ok =
                T.null stdoutLeak
                    && T.null stderrOut
                    && exitCode (const False) True result
                    && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show result)
                ]
        return $
            mkResult
                outputFileMultiInputBlockScopeExternPrototypeDefinitionMsg
                ok
                details

outputFileMultiInputBlockScopeExternObjectConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputBlockScopeExternObjectConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external declarations in multi-input -o mode: x"
        T.writeFile usePath "int main(void) { extern char x; return x; }"
        T.writeFile defPath "int x = 256;"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $
            mkResult
                outputFileMultiInputBlockScopeExternObjectConflictMsg
                ok
                details

outputFileMultiInputBlockScopeExternObjectFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputBlockScopeExternObjectFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            expectedError = "multiple external definitions in multi-input -o mode: x"
        T.writeFile usePath "int main(void) { extern int x; return x; }"
        T.writeFile defPath "int x(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $
            mkResult
                outputFileMultiInputBlockScopeExternObjectFunctionConflictMsg
                ok
                details

outputFileMultiInputBlockScopeExternPrototypeVisibilityTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputBlockScopeExternPrototypeVisibilityTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int probe(void) { extern int foo(void); return 0; }"
            , "int main(void) { return foo(1); }"
            ]
        T.writeFile otherPath "int side(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            hasRequiredAsm =
                all (`T.isInfixOf` asm)
                    [ "main:"
                    , "call foo"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && hasRequiredAsm
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $
            mkResult
                outputFileMultiInputBlockScopeExternPrototypeVisibilityMsg
                ok
                details

outputFileMultiInputDeferredIncompletePointeeUseTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputDeferredIncompletePointeeUseTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: f"
        T.writeFile usePath $ T.unlines
            [ "int (*f(void))[];"
            , "int *g(void) { return *(f() + 1); }"
            , "int main(void) { return sizeof *f() != 16 || _Alignof *f() != 4 || g()[0] != 5; }"
            ]
        T.writeFile defPath $ T.unlines
            [ "int a[8] = { 0, 0, 0, 0, 5 };"
            , "int (*f(void))[4] { return &a; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputDeferredIncompletePointeeUseMsg ok details

outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-decl.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            declPath = "tmp-decl.c"
            expectedError = "invalid use of pointer to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int (*f(void))[];"
            , "int main(void) { int (*p)[] = f(); p += 1; return 0; }"
            ]
        T.writeFile declPath "int (*f(void))[];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputDeferredIncompletePointerAddSubAssignRejectMsg ok details

outputFileMultiInputDeferredIncompletePointerIncDecRejectTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputDeferredIncompletePointerIncDecRejectTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-decl.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            usePath = "tmp-use.c"
            declPath = "tmp-decl.c"
            expectedError = "invalid use of pointer to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int (*f(void))[];"
            , "int main(void) { int (*p)[] = f(); ++p; return 0; }"
            ]
        T.writeFile declPath "int (*f(void))[];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputDeferredIncompletePointerIncDecRejectMsg ok details

outputFileMultiInputImplicitFunctionArityRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionArityRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "too few arguments to function call"
        T.writeFile callerPath "int main(void) { return foo(); }"
        T.writeFile defPath "int foo(int x) { return x; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionArityRetypeMsg ok details

outputFileMultiInputImplicitFunctionObjectPointerRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionObjectPointerRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "invalid argument type to function call"
        T.writeFile callerPath "int main(void) { return foo(1); }"
        T.writeFile defPath "int foo(int *p) { return p != 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionObjectPointerRetypeMsg ok details

outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "invalid argument type to function call"
        T.writeFile callerPath $ T.unlines
            [ "int main(void) {"
            , "  int *x = 0;"
            , "  int **pp = &x;"
            , "  return foo(pp);"
            , "}"
            ]
        T.writeFile defPath "int foo(char **p) { return p != 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionObjectPointerMismatchRetypeMsg ok details

outputFileMultiInputIndirectFunctionPointerArityRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputIndirectFunctionPointerArityRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-decl.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            declPath = "tmp-decl.c"
            expectedError = "too few arguments to function call"
        T.writeFile callerPath $ T.unlines
            [ "int (*fp)();"
            , "int main(void) { return fp(); }"
            ]
        T.writeFile declPath "int (*fp)(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputIndirectFunctionPointerArityRetypeMsg ok details

outputFileMultiInputParameterSizeofRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputParameterSizeofRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-def.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            defPath = "tmp-def.c"
            protoPath = "tmp-proto.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: f"
        T.writeFile defPath $ T.unlines
            [ "int f(int (*p)[]) { return sizeof(*p); }"
            , "int main(void) { return 0; }"
            ]
        T.writeFile protoPath "int f(int (*p)[4]);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack defPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputParameterSizeofRetypeMsg ok details

outputFileMultiInputIndirectFunctionPointerVoidRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputIndirectFunctionPointerVoidRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-decl.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            declPath = "tmp-decl.c"
            expectedError = "too many arguments to function call"
        T.writeFile callerPath $ T.unlines
            [ "int (*fp)();"
            , "int main(void) { return fp(1); }"
            ]
        T.writeFile declPath "int (*fp)(void);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputIndirectFunctionPointerVoidRetypeMsg ok details

outputFileMultiInputImplicitFunctionVoidRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputImplicitFunctionVoidRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "too many arguments to function call"
        T.writeFile callerPath "int main(void) { return foo(1); }"
        T.writeFile defPath "int foo(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputImplicitFunctionVoidRetypeMsg ok details

outputFileMultiInputFunctionDesignatorAssignmentRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionDesignatorAssignmentRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "invalid operands to assignment"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int main(void) {"
            , "  int (*p)(void);"
            , "  p = foo;"
            , "  return 0;"
            , "}"
            ]
        T.writeFile protoPath "int foo(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionDesignatorAssignmentRetypeMsg ok details

outputFileMultiInputFunctionDesignatorReturnRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionDesignatorReturnRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "invalid return type"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int (*g(void))(void) { return foo; }"
            ]
        T.writeFile protoPath "int foo(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionDesignatorReturnRetypeMsg ok details

outputFileMultiInputObjectPointerReturnRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputObjectPointerReturnRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "invalid return type"
        T.writeFile callerPath $ T.unlines
            [ "int a[];"
            , "int (*f(void))[3] { return &a; }"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int a[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputObjectPointerReturnRetypeMsg ok details

outputFileMultiInputFunctionDesignatorInitializerRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionDesignatorInitializerRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "invalid initializer for scalar object"
        T.writeFile callerPath $ T.unlines
            [ "int foo();"
            , "int (*p)(void) = foo;"
            , "int main(void) { return 0; }"
            ]
        T.writeFile protoPath "int foo(int);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionDesignatorInitializerRetypeMsg ok details

outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-proto.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            protoPath = "tmp-proto.c"
            expectedError = "conflicting external function declarations in multi-input -o mode: take"
        T.writeFile callerPath $ T.unlines
            [ "int take(int (*p)[]);"
            , "int (*fp)(int (*arg)[]) = take;"
            , "int main(void) { return 0; }"
            ]
        T.writeFile protoPath "int take(int (*p)[4]);"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack protoPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputFunctionDesignatorInitializerParamRefinementRetypeMsg ok details

outputFileMultiInputObjectPointerAssignmentRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputObjectPointerAssignmentRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "invalid operands to assignment"
        T.writeFile callerPath $ T.unlines
            [ "int x[];"
            , "int main(void) {"
            , "  int (*p)[5];"
            , "  p = &x;"
            , "  return 0;"
            , "}"
            ]
        T.writeFile defPath "int x[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputObjectPointerAssignmentRetypeMsg ok details

outputFileMultiInputObjectPointerInitializerRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputObjectPointerInitializerRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
            expectedError = "invalid initializer for scalar object"
        T.writeFile callerPath $ T.unlines
            [ "int x[];"
            , "int (*p)[5] = &x;"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int x[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputObjectPointerInitializerRetypeMsg ok details

outputFileMultiInputPointerPointeeArrayConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputPointerPointeeArrayConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
            expectedError = "conflicting external declarations in multi-input -o mode: p"
        T.writeFile declPath $ T.unlines
            [ "int (*p)[3];"
            , "int main(void) { return 0; }"
            ]
        T.writeFile defPath "int (*p)[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputPointerPointeeArrayConflictMsg ok details

outputFileMultiInputAggregateFunctionDesignatorInitializerTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputAggregateFunctionDesignatorInitializerTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            defPath = "tmp-def.c"
        T.writeFile callerPath $ T.unlines
            [ "int foo(void);"
            , "int (*fps[1])(void) = { foo };"
            , "int main(void) { return fps[0](); }"
            ]
        T.writeFile defPath "int foo(void) { return 0; }"
        execErrFin $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile target
        result <- exec "./tmp"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "fps:"
                    , "main:"
                    ]
            ok = T.null stdoutLeak && T.null stderrOut && exitCode (const False) True result && hasRequiredLabels
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputAggregateFunctionDesignatorInitializerMsg ok details

outputFileMultiInputSameInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSameInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile callerPath $ T.unlines
            [ "int main(void) { return foo(); }"
            , "int foo(void) { return 1; }"
            ]
        T.writeFile otherPath "int helper(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        linkCmd <- assemblerCommand [target, "-o", "tmp"]
        execErrFin linkCmd
        runResult <- exec "./tmp"
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "helper:"
                    , "main:"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && hasExpectedWarning
                    && hasRequiredLabels
                    && exitCode (const False) True runResult
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputSameInputImplicitFunctionDefinitionWarningMsg ok details

outputFileMultiInputSameInputImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSameInputImplicitFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c", "tmp.o"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        T.writeFile callerPath $ T.unlines
            [ "int foo;"
            , "int main(void) { return foo(); }"
            ]
        T.writeFile otherPath "int helper(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
        let ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputSameInputImplicitFunctionConflictMsg ok details

outputFileMultiInputSameInputFunctionDeclarationConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSameInputFunctionDeclarationConflictTest =
    flip finally (clean ["tmp-forward.out", "tmp-forward.err", "tmp-forward.s", "tmp-forward.c", "tmp-forward.o", "tmp-reverse.out", "tmp-reverse.err", "tmp-reverse.s", "tmp-reverse.c", "tmp-reverse.o", "tmp-other.c"]) $ do
        htccCmd <- htccCommand
        let otherPath = "tmp-other.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
            runCase target declPath stdoutPath stderrPath objPath declarations = do
                clean [target, declPath, stdoutPath, stderrPath, objPath]
                T.writeFile declPath $ T.unlines declarations
                result <- exec $ mconcat
                    [ htccCmd
                    , " -o "
                    , T.pack target
                    , " "
                    , T.pack declPath
                    , " "
                    , T.pack otherPath
                    , " > "
                    , T.pack stdoutPath
                    , " 2> "
                    , T.pack stderrPath
                    ]
                stdoutLeak <- T.readFile stdoutPath
                stderrOut <- T.readFile stderrPath
                targetExists <- doesFileExist target
                let failed = exitCode (const True) False result
                    hasExpectedError = expectedError `T.isInfixOf` stderrOut
                let ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
                    details = T.unlines
                        [ "stdout:"
                        , stdoutLeak
                        , "stderr:"
                        , stderrOut
                        , "targetExists: " <> T.pack (show targetExists)
                        , "hasExpectedError: " <> T.pack (show hasExpectedError)
                        , "exitCode: " <> T.pack (show result)
                        ]
                pure (ok, details)
        T.writeFile otherPath "int helper(void) { return 0; }"
        (forwardOk, forwardDetails) <-
            runCase
                "tmp-forward.s"
                "tmp-forward.c"
                "tmp-forward.out"
                "tmp-forward.err"
                "tmp-forward.o"
                [ "int foo(void);"
                , "int foo;"
                , "int main(void) { return 0; }"
                ]
        (reverseOk, reverseDetails) <-
            runCase
                "tmp-reverse.s"
                "tmp-reverse.c"
                "tmp-reverse.out"
                "tmp-reverse.err"
                "tmp-reverse.o"
                [ "int foo;"
                , "int foo(void);"
                , "int main(void) { return 0; }"
                ]
        let ok = forwardOk && reverseOk
            details = T.unlines
                [ "forward:"
                , forwardDetails
                , "reverse:"
                , reverseDetails
                ]
        return $ mkResult outputFileMultiInputSameInputFunctionDeclarationConflictMsg ok details

outputFileMultiInputSameInputStaticImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSameInputStaticImplicitFunctionConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-caller.c", "tmp-other.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            callerPath = "tmp-caller.c"
            otherPath = "tmp-other.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        T.writeFile callerPath $ T.unlines
            [ "static int foo;"
            , "int main(void) { return foo(); }"
            ]
        T.writeFile otherPath "int helper(void) { return 0; }"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack callerPath
            , " "
            , T.pack otherPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputSameInputStaticImplicitFunctionConflictMsg ok details

outputFileMultiInputSameInputInternalLinkageConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputSameInputInternalLinkageConflictTest =
    flip finally
        (clean
            [ "tmp-static-static.out"
            , "tmp-static-static.err"
            , "tmp-static-static.s"
            , "tmp-static-static.c"
            , "tmp-static-extern-forward.out"
            , "tmp-static-extern-forward.err"
            , "tmp-static-extern-forward.s"
            , "tmp-static-extern-forward.c"
            , "tmp-static-extern-reverse.out"
            , "tmp-static-extern-reverse.err"
            , "tmp-static-extern-reverse.s"
            , "tmp-static-extern-reverse.c"
            , "tmp-other.c"
            ]
        ) $ do
        htccCmd <- htccCommand
        let otherPath = "tmp-other.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
            runCase target sourcePath stdoutPath stderrPath declarations = do
                clean [target, sourcePath, stdoutPath, stderrPath]
                T.writeFile sourcePath $ T.unlines declarations
                result <- exec $ mconcat
                    [ htccCmd
                    , " -o "
                    , T.pack target
                    , " "
                    , T.pack sourcePath
                    , " "
                    , T.pack otherPath
                    , " > "
                    , T.pack stdoutPath
                    , " 2> "
                    , T.pack stderrPath
                    ]
                stdoutLeak <- T.readFile stdoutPath
                stderrOut <- T.readFile stderrPath
                targetExists <- doesFileExist target
                let failed = exitCode (const True) False result
                    hasExpectedError = expectedError `T.isInfixOf` stderrOut
                    ok = failed && T.null stdoutLeak && not targetExists && hasExpectedError
                    details = T.unlines
                        [ "stdout:"
                        , stdoutLeak
                        , "stderr:"
                        , stderrOut
                        , "targetExists: " <> T.pack (show targetExists)
                        , "hasExpectedError: " <> T.pack (show hasExpectedError)
                        , "exitCode: " <> T.pack (show result)
                        ]
                pure (ok, details)
        T.writeFile otherPath "int helper(void) { return 0; }"
        (staticStaticOk, staticStaticDetails) <-
            runCase
                "tmp-static-static.s"
                "tmp-static-static.c"
                "tmp-static-static.out"
                "tmp-static-static.err"
                [ "static int foo;"
                , "static int foo(void) { return 1; }"
                , "int main(void) { return 0; }"
                ]
        (staticExternForwardOk, staticExternForwardDetails) <-
            runCase
                "tmp-static-extern-forward.s"
                "tmp-static-extern-forward.c"
                "tmp-static-extern-forward.out"
                "tmp-static-extern-forward.err"
                [ "static int foo(void) { return 1; }"
                , "int foo;"
                , "int main(void) { return foo; }"
                ]
        (staticExternReverseOk, staticExternReverseDetails) <-
            runCase
                "tmp-static-extern-reverse.s"
                "tmp-static-extern-reverse.c"
                "tmp-static-extern-reverse.out"
                "tmp-static-extern-reverse.err"
                [ "int foo;"
                , "static int foo(void) { return 1; }"
                , "int main(void) { return foo; }"
                ]
        let ok = staticStaticOk && staticExternForwardOk && staticExternReverseOk
            details = T.unlines
                [ "static-static:"
                , staticStaticDetails
                , "static-extern-forward:"
                , staticExternForwardDetails
                , "static-extern-reverse:"
                , staticExternReverseDetails
                ]
        return $ mkResult outputFileMultiInputSameInputInternalLinkageConflictMsg ok details

outputFileMultiInputTentativeGlobalTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeGlobalTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp.o", "tmp-foo.c", "tmp-bar.c"]) $ do
        htccCmd <- htccCommand
        T.writeFile "tmp-foo.c" $ T.unlines
            [ "int x;"
            , "int* foo() { return &x; }"
            ]
        T.writeFile "tmp-bar.c" $ T.unlines
            [ "int x;"
            , "int bar() { x = 42; return x; }"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
            ]
        assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "bar:"
                    ]
            tentativeLabelCount = T.count "\nx:" $ "\n" <> asm
            ok = T.null stdoutLeak && hasRequiredLabels && tentativeLabelCount == 1
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "tentativeLabelCount: " <> T.pack (show tentativeLabelCount)
                ]
        return $ mkResult outputFileMultiInputTentativeGlobalMsg ok details

outputFileMultiInputExternGlobalDeclarationMergeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputExternGlobalDeclarationMergeTest =
    flip finally
        (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-decl.c", "tmp-def.c", "tmp"]) $ do
            htccCmd <- htccCommand
            let declPath = "tmp-decl.c"
                defPath = "tmp-def.c"

            T.writeFile declPath $ T.unlines
                [ "extern int x;"
                , "int main(void) { return x != 1; }"
                ]
            T.writeFile defPath "int x = 1;"
            execErrFin $ mconcat
                [ htccCmd
                , " -o tmp.s "
                , T.pack declPath
                , " "
                , T.pack defPath
                , " > tmp.out 2> tmp.err"
                ]
            linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
            execErrFin linkCmd
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            initResult <- exec "./tmp"
            asmAfterInit <- T.readFile "tmp.s"

            T.writeFile declPath $ T.unlines
                [ "extern int y;"
                , "int main(void) { return y; }"
                ]
            T.writeFile defPath "int y;"
            execErrFin $ mconcat
                [ htccCmd
                , " -o tmp.s "
                , T.pack declPath
                , " "
                , T.pack defPath
                , " > tmp.out 2> tmp.err"
                ]
            linkCmd' <- assemblerCommand ["tmp.s", "-o", "tmp"]
            execErrFin linkCmd'
            stdoutLeak' <- T.readFile "tmp.out"
            stderrOut' <- T.readFile "tmp.err"
            tentativeResult <- exec "./tmp"
            asmAfterTentative <- T.readFile "tmp.s"

            let initOk =
                    T.null stdoutLeak
                        && T.null stderrOut
                        && exitCode (const False) True initResult
                        && "x:" `T.isInfixOf` asmAfterInit
                tentativeOk =
                    T.null stdoutLeak'
                        && T.null stderrOut'
                        && exitCode (const False) True tentativeResult
                        && "y:" `T.isInfixOf` asmAfterTentative
                ok = initOk && tentativeOk
                details = T.unlines
                    [ "initialized-case stdout:"
                    , stdoutLeak
                    , "initialized-case stderr:"
                    , stderrOut
                    , "initialized-case asm has x: " <> T.pack (show $ "x:" `T.isInfixOf` asmAfterInit)
                    , "initialized-case runExitCode: " <> T.pack (show initResult)
                    , "tentative-case stdout:"
                    , stdoutLeak'
                    , "tentative-case stderr:"
                    , stderrOut'
                    , "tentative-case asm has y: " <> T.pack (show $ "y:" `T.isInfixOf` asmAfterTentative)
                    , "tentative-case runExitCode: " <> T.pack (show tentativeResult)
                    ]
            return $
                mkResult
                    outputFileMultiInputExternGlobalDeclarationMergeMsg
                    ok
                    details

outputFileMultiInputTentativeArrayTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-decl.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let declPath = "tmp-decl.c"
            defPath = "tmp-def.c"
        T.writeFile declPath $ T.unlines
            [ "int x[];"
            , "int foo(void) { return 0; }"
            ]
        T.writeFile defPath $ T.unlines
            [ "int x[4];"
            , "int main() { return 0; }"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack declPath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            tentativeLabelCount = T.count "\nx:" $ "\n" <> asm
            hasCompletedArraySize = "x:\n\t.zero 16" `T.isInfixOf` asm
            ok =
                T.null stdoutLeak
                    && hasRequiredLabels
                    && tentativeLabelCount == 1
                    && hasCompletedArraySize
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "tentativeLabelCount: " <> T.pack (show tentativeLabelCount)
                , "hasCompletedArraySize: " <> T.pack (show hasCompletedArraySize)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayMsg ok details

outputFileMultiInputTentativeArrayDecayRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayDecayRetypeTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-use.c", "tmp-def.c"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            fSection asm = snd $ T.breakOn "\nf:\n" ("\n" <> asm)
        T.writeFile usePath $ T.unlines
            [ "int x[];"
            , "int *f(void) { return x; }"
            ]
        T.writeFile defPath "int x[2];"
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        let functionAsm = fSection asm
            hasRetypedDecay =
                all (`T.isInfixOf` functionAsm)
                    [ "f:"
                    , "push offset x"
                    , "pop rax"
                    , "jmp .L.return.f"
                    ]
            loadsScalarElement = "movsxd rax, dword ptr [rax]" `T.isInfixOf` functionAsm
            ok = T.null stdoutLeak && hasRetypedDecay && not loadsScalarElement
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "functionAsm:"
                , functionAsm
                , "hasRetypedDecay: " <> T.pack (show hasRetypedDecay)
                , "loadsScalarElement: " <> T.pack (show loadsScalarElement)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayDecayRetypeMsg ok details

externalBoolLowByteNormalizationTest :: IO (Int, String)
externalBoolLowByteNormalizationTest =
    flip finally (clean ["tmp", "tmp.s", "tmp-bool-ext.o", "tmp-bool-ext.s", "tmp-bool-main.c"]) $ do
        let description = "external _Bool calls normalize only the low byte of rax"
        htccCmd <- htccCommand
        T.writeFile "tmp-bool-main.c" $ T.unlines
            [ "_Bool test_bool_low_byte_zero(void);"
            , "int main(void) {"
            , "  _Bool (*fp)(void);"
            , "  fp = test_bool_low_byte_zero;"
            , "  return test_bool_low_byte_zero() != 0 || fp() != 0;"
            , "}"
            ]
        T.writeFile "tmp-bool-ext.s" $ T.unlines
            [ ".intel_syntax noprefix"
            , ".global test_bool_low_byte_zero"
            , "test_bool_low_byte_zero:"
            , "  mov rax, 256"
            , "  ret"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " tmp-bool-main.c > tmp.s"
            ]
        assemblerCommand ["-x", "assembler", "-c", "-o", "tmp-bool-ext.o", "tmp-bool-ext.s"] >>= execErrFin
        assemblerCommand ["tmp-bool-ext.o", "tmp.s", "-o", "tmp"] >>= execErrFin
        exitCode (, description) (0, description) <$> exec "./tmp"

externalBoolParameterLowByteNormalizationTest :: IO (Int, String)
externalBoolParameterLowByteNormalizationTest =
    flip finally (clean ["tmp", "tmp.s", "tmp-bool-param-ext.o", "tmp-bool-param-ext.s", "tmp-bool-param-main.c"]) $ do
        let description = "external callers normalize only the low byte of incoming _Bool parameters"
        htccCmd <- htccCommand
        T.writeFile "tmp-bool-param-main.c" $ T.unlines
            [ "int takes_bool_reg(_Bool x) {"
            , "  return x;"
            , "}"
            , "int takes_bool_stack(long a, long b, long c, long d, long e, long f, _Bool g) {"
            , "  return g;"
            , "}"
            ]
        T.writeFile "tmp-bool-param-ext.s" $ T.unlines
            [ ".intel_syntax noprefix"
            , ".global main"
            , "main:"
            , "  push rbp"
            , "  mov rbp, rsp"
            , "  sub rsp, 16"
            , "  mov rdi, 256"
            , "  call takes_bool_reg"
            , "  mov [rbp-8], rax"
            , "  sub rsp, 8"
            , "  push 256"
            , "  mov r9, 6"
            , "  mov r8, 5"
            , "  mov rcx, 4"
            , "  mov rdx, 3"
            , "  mov rsi, 2"
            , "  mov rdi, 1"
            , "  call takes_bool_stack"
            , "  add rsp, 16"
            , "  mov rdx, [rbp-8]"
            , "  or rax, rdx"
            , "  leave"
            , "  ret"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " tmp-bool-param-main.c > tmp.s"
            ]
        assemblerCommand ["-x", "assembler", "-c", "-o", "tmp-bool-param-ext.o", "tmp-bool-param-ext.s"] >>= execErrFin
        assemblerCommand ["tmp-bool-param-ext.o", "tmp.s", "-o", "tmp"] >>= execErrFin
        exitCode (, description) (0, description) <$> exec "./tmp"

externalIntegralReturnNormalizationTest :: IO (Int, String)
externalIntegralReturnNormalizationTest =
    flip finally (clean ["tmp", "tmp.s", "tmp-int-ret-ext.o", "tmp-int-ret-ext.s", "tmp-int-ret-main.c"]) $ do
        let description = "external direct and indirect calls truncate signed char/short/int return values"
        htccCmd <- htccCommand
        T.writeFile "tmp-int-ret-main.c" $ T.unlines
            [ "char ret_char(void);"
            , "short ret_short(void);"
            , "int ret_int(void);"
            , "int main(void) {"
            , "  char (*char_fp)(void);"
            , "  short (*short_fp)(void);"
            , "  int (*int_fp)(void);"
            , "  char_fp = ret_char;"
            , "  short_fp = ret_short;"
            , "  int_fp = ret_int;"
            , "  return ret_char() != -1"
            , "      || char_fp() != -1"
            , "      || ret_short() != -1"
            , "      || short_fp() != -1"
            , "      || ret_int() != -1"
            , "      || int_fp() != -1;"
            , "}"
            ]
        T.writeFile "tmp-int-ret-ext.s" $ T.unlines
            [ ".intel_syntax noprefix"
            , ".global ret_char"
            , "ret_char:"
            , "  mov eax, 0x123456ff"
            , "  ret"
            , ".global ret_short"
            , "ret_short:"
            , "  mov eax, 0x1234ffff"
            , "  ret"
            , ".global ret_int"
            , "ret_int:"
            , "  mov eax, -1"
            , "  ret"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " tmp-int-ret-main.c > tmp.s"
            ]
        assemblerCommand ["-x", "assembler", "-c", "-o", "tmp-int-ret-ext.o", "tmp-int-ret-ext.s"] >>= execErrFin
        assemblerCommand ["tmp-int-ret-ext.o", "tmp.s", "-o", "tmp"] >>= execErrFin
        exitCode (, description) (0, description) <$> exec "./tmp"

outputFileMultiInputTentativeIncompleteArrayTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeIncompleteArrayTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.s", "tmp-foo.c", "tmp-bar.c"]) $ do
        htccCmd <- htccCommand
        T.writeFile "tmp-foo.c" $ T.unlines
            [ "int x[];"
            , "int *foo(void) { return x; }"
            ]
        T.writeFile "tmp-bar.c" $ T.unlines
            [ "int x[];"
            , "int *foo(void);"
            , "int main(void) { x[0] = 1; return foo() != x || x[0] != 1; }"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        runResult <- exec "./tmp"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ "foo:"
                    , "main:"
                    ]
            tentativeLabelCount = T.count "\nx:" $ "\n" <> asm
            hasMaterializedArraySize = "x:\n\t.zero 4" `T.isInfixOf` asm
            exitStatus = exitCode id 0 runResult
            ok =
                T.null stdoutLeak
                    && hasRequiredLabels
                    && tentativeLabelCount == 1
                    && hasMaterializedArraySize
                    && exitStatus == 0
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "tentativeLabelCount: " <> T.pack (show tentativeLabelCount)
                , "hasMaterializedArraySize: " <> T.pack (show hasMaterializedArraySize)
                , "exitStatus: " <> T.pack (show exitStatus)
                ]
        return $ mkResult outputFileMultiInputTentativeIncompleteArrayMsg ok details

outputFileMultiInputTentativeNestedIncompleteArrayTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeNestedIncompleteArrayTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-decl.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            declPath = "tmp-decl.c"
            target = "tmp.s"
            expectedError = "invalid use of pointer to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int x[][4];"
            , "int main(void) { return ((char*)(&x + 1)) - ((char*)&x); }"
            ]
        T.writeFile declPath "int x[][4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack declPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeNestedIncompleteArrayMsg ok details

outputFileMultiInputTentativeNestedArrayExtentInferenceTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeNestedArrayExtentInferenceTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            target = "tmp.s"
            expectedError = "invalid use of pointer to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int x[][4];"
            , "int main(void) { return ((char*)(&x + 1)) - ((char*)&x); }"
            ]
        T.writeFile defPath "int x[2][4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeNestedArrayExtentInferenceMsg ok details

outputFileMultiInputTentativeArrayRankConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayRankConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-foo.c", "tmp-bar.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            fooPath = "tmp-foo.c"
            barPath = "tmp-bar.c"
            expectedError = "conflicting external declarations in multi-input -o mode: x"
        T.writeFile fooPath $ T.unlines
            [ "int x[];"
            , "int foo(void) { return 0; }"
            ]
        T.writeFile barPath $ T.unlines
            [ "int x[2][4];"
            , "int main(void) { return 0; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack fooPath
            , " "
            , T.pack barPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayRankConflictMsg ok details

outputFileMultiInputTentativeArrayInnerExtentConflictTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayInnerExtentConflictTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-foo.c", "tmp-bar.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            fooPath = "tmp-foo.c"
            barPath = "tmp-bar.c"
            expectedError = "conflicting external declarations in multi-input -o mode: x"
        T.writeFile fooPath $ T.unlines
            [ "int x[][3];"
            , "int foo(void) { return 0; }"
            ]
        T.writeFile barPath $ T.unlines
            [ "int x[][4];"
            , "int main(void) { return 0; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack fooPath
            , " "
            , T.pack barPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayInnerExtentConflictMsg ok details

outputFileMultiInputTentativeArrayUseSiteTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayUseSiteTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            target = "tmp.s"
            expectedError = "invalid application of 'sizeof' to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int x[];"
            , "int main(void) { return sizeof x / sizeof x[0]; }"
            ]
        T.writeFile defPath "int x[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayUseSiteMsg ok details

outputFileMultiInputTentativeArrayAddressUseSiteTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayAddressUseSiteTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            target = "tmp.s"
            expectedError = "invalid use of pointer to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int x[];"
            , "int main(void) { return ((char*)(&x + 1)) - ((char*)&x); }"
            ]
        T.writeFile defPath "int x[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayAddressUseSiteMsg ok details

outputFileMultiInputTentativeArrayInitializerRetypeTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputTentativeArrayInitializerRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-use.c", "tmp-def.c", "tmp"]) $ do
        htccCmd <- htccCommand
        let usePath = "tmp-use.c"
            defPath = "tmp-def.c"
            target = "tmp.s"
            expectedError = "invalid application of 'sizeof' to incomplete type"
        T.writeFile usePath $ T.unlines
            [ "int x[];"
            , "int y = sizeof x;"
            , "char *p = (char*)(&x + 1);"
            , "int main(void) { return y == 4 && p == ((char*)&x) + 4; }"
            ]
        T.writeFile defPath "int x[4];"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack usePath
            , " "
            , T.pack defPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            hasNoPointerArithmeticError = not ("invalid use of pointer to incomplete type" `T.isInfixOf` stderrOut)
            ok = failed && T.null stdoutLeak && hasExpectedError && hasNoPointerArithmeticError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "hasNoPointerArithmeticError: " <> T.pack (show hasNoPointerArithmeticError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileMultiInputTentativeArrayInitializerRetypeMsg ok details

outputFileMultiInputStaticTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputStaticTest = flip finally (clean ["tmp.out", "tmp.s", "tmp.o", "tmp-foo.c", "tmp-bar.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-foo.c" $ T.unlines
        [ "static int helper;"
        , "int foo() { helper = 1; return helper; }"
        ]
    T.writeFile "tmp-bar.c" $ T.unlines
        [ "static int helper;"
        , "int bar() { helper = 2; return helper; }"
        ]
    execErrFin $ mconcat
        [ htccCmd
        , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
        ]
    assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasRequiredLabels =
            all (`T.isInfixOf` asm)
                [ "foo:"
                , ".L.return.foo:"
                , "bar:"
                , ".L.return.bar:"
                ]
        hasOriginalStaticLabel = "\nhelper:" `T.isInfixOf` asm
        ok = T.null stdoutLeak && hasRequiredLabels && not hasOriginalStaticLabel
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
            , "hasOriginalStaticLabel: " <> T.pack (show hasOriginalStaticLabel)
            ]
    return $ mkResult outputFileMultiInputStaticMsg ok details

outputFileMultiInputStaticFunctionTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputStaticFunctionTest = flip finally (clean ["tmp.out", "tmp.s", "tmp.o", "tmp-foo.c", "tmp-bar.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-foo.c" $ T.unlines
        [ "static int helper(void) { return 1; }"
        , "int foo(void) { return helper(); }"
        ]
    T.writeFile "tmp-bar.c" $ T.unlines
        [ "static int helper(void) { return 2; }"
        , "int bar(void) { return helper(); }"
        ]
    execErrFin $ mconcat
        [ htccCmd
        , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
        ]
    assemblerCommand ["-x", "assembler", "-c", "-o", "tmp.o", "tmp.s"] >>= execErrFin
    stdoutLeak <- T.readFile "tmp.out"
    asm <- T.readFile "tmp.s"
    let hasRequiredLabels =
            all (`T.isInfixOf` asm)
                [ "foo:"
                , ".L.return.foo:"
                , "bar:"
                , ".L.return.bar:"
                , ".L.internal.0.helper:"
                , ".L.internal.1.helper:"
                ]
        hasOriginalHelperLabel = "\nhelper:" `T.isInfixOf` asm
        ok = T.null stdoutLeak && hasRequiredLabels && not hasOriginalHelperLabel
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
            , "hasOriginalHelperLabel: " <> T.pack (show hasOriginalHelperLabel)
            ]
    return $ mkResult outputFileMultiInputStaticFunctionMsg ok details

outputFileMultiInputStaticFunctionPointerTest :: IO (Either T.Text T.Text, String)
outputFileMultiInputStaticFunctionPointerTest =
    flip finally (clean ["tmp.out", "tmp.s", "tmp-foo.c", "tmp-bar.c", "tmp"]) $ do
        htccCmd <- htccCommand
        T.writeFile "tmp-foo.c" $ T.unlines
            [ "static int helper(void) { return 1; }"
            , "int foo(void) { int (*fp)(void); fp = helper; return helper != 0 && fp != 0; }"
            ]
        T.writeFile "tmp-bar.c" $ T.unlines
            [ "int foo(void);"
            , "static int helper(void) { return 2; }"
            , "int bar(void) { int (*fp)(void); fp = helper; return helper != 0 && fp != 0; }"
            , "int main(void) { return foo() + bar(); }"
            ]
        execErrFin $ mconcat
            [ htccCmd
            , " -o tmp.s tmp-foo.c tmp-bar.c > tmp.out"
            ]
        linkCmd <- assemblerCommand ["tmp.s", "-o", "tmp"]
        execErrFin linkCmd
        stdoutLeak <- T.readFile "tmp.out"
        asm <- T.readFile "tmp.s"
        let hasRequiredLabels =
                all (`T.isInfixOf` asm)
                    [ ".L.internal.0.helper:"
                    , ".L.internal.1.helper:"
                    ]
        runResult <- exec "./tmp"
        let exitStatus = exitCode id 0 runResult
            ok = T.null stdoutLeak && hasRequiredLabels && exitStatus == 2
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "hasRequiredLabels: " <> T.pack (show hasRequiredLabels)
                , "exitStatus: " <> T.pack (show exitStatus)
                ]
        return $ mkResult outputFileMultiInputStaticFunctionPointerMsg ok details

outputFilePreservesExistingModeTest :: IO (Either T.Text T.Text, String)
outputFilePreservesExistingModeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-mode-input.c", "tmp-mode.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-mode.s"
            inputPath = "tmp-mode-input.c"
            originalMode = foldr1 unionFileModes
                [ ownerReadMode
                , groupReadMode
                , otherReadMode
                ]
        clean ["tmp.out", "tmp.err", inputPath, target]
        T.writeFile inputPath source
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile target
        replacedMode <- fileMode <$> getFileStatus target
        let replacedPermissions = permissionBits replacedMode
        let succeeded = exitCode (const False) True result
            hasAsm = all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && replacedPermissions == originalMode
                    && hasAsm
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "modePreserved: " <> T.pack (show (replacedPermissions == originalMode))
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "hasAsm: " <> T.pack (show hasAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFilePreservesExistingModeMsg ok details

outputFileClearsSpecialBitsTest :: IO (Either T.Text T.Text, String)
outputFileClearsSpecialBitsTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-special-input.c", "tmp-special.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-special.s"
            inputPath = "tmp-special-input.c"
            originalMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                , specialFileModeMask
                ]
        clean ["tmp.out", "tmp.err", inputPath, target]
        T.writeFile inputPath source
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile target
        replacedMode <- fileMode <$> getFileStatus target
        let specialBitsCleared = intersectFileModes replacedMode specialFileModeMask == 0
            hasAsm = all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            succeeded = exitCode (const False) True result
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && specialBitsCleared
                    && hasAsm
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "specialBitsCleared: " <> T.pack (show specialBitsCleared)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "hasAsm: " <> T.pack (show hasAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileClearsSpecialBitsMsg ok details

outputFileFollowsSymlinkTargetTest :: IO (Either T.Text T.Text, String)
outputFileFollowsSymlinkTargetTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-link-output.s", "tmp-link-input.c", "tmp-link-target.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-link-target.s"
            linkPath = "tmp-link-output.s"
            inputPath = "tmp-link-input.c"
        clean ["tmp.out", "tmp.err", linkPath, inputPath, target]
        T.writeFile inputPath source
        T.writeFile target "stale output"
        execErrFin $ "ln -s '" <> T.pack target <> "' '" <> T.pack linkPath <> "'"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack linkPath
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetContents <- T.readFile target
        linkStatus <- getSymbolicLinkStatus linkPath
        let succeeded = exitCode (const False) True result
            linkStillSymlink = isSymbolicLink linkStatus
            targetUpdated = all (`T.isInfixOf` targetContents)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && linkStillSymlink
                    && targetUpdated
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "linkStillSymlink: " <> T.pack (show linkStillSymlink)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileFollowsSymlinkTargetMsg ok details

outputFileSpecialPathDevNullTest :: IO (Either T.Text T.Text, String)
outputFileSpecialPathDevNullTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-devnull-input.c"]) $ do
        htccCmd <- htccCommand
        let inputPath = "tmp-devnull-input.c"
        clean ["tmp.out", "tmp.err", inputPath]
        T.writeFile inputPath source
        result <- exec $ mconcat
            [ htccCmd
            , " -o /dev/null "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        let succeeded = exitCode (const False) True result
            ok = succeeded && T.null stdoutLeak && T.null stderrOut
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileSpecialPathDevNullMsg ok details

outputFileSamePathTest :: IO (Either T.Text T.Text, String)
outputFileSamePathTest = flip finally (clean ["tmp.out", "tmp.err", "tmp-same.c"]) $ do
    htccCmd <- htccCommand
    T.writeFile "tmp-same.c" source
    result <- exec $ mconcat
        [ htccCmd
        , " -o tmp-same.c tmp-same.c > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    contents <- T.readFile "tmp-same.c"
    let failed = exitCode (const True) False result
        hasAliasError = "-o output path must not overwrite an input file" `T.isInfixOf` stderrOut
        ok = failed && T.null stdoutLeak && contents == source && hasAliasError
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "inputUnchanged: " <> T.pack (show (contents == source))
            , "hasAliasError: " <> T.pack (show hasAliasError)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult outputFileSamePathMsg ok details

outputFileMissingInputMatchingOutputPathTest :: IO (Either T.Text T.Text, String)
outputFileMissingInputMatchingOutputPathTest = flip finally (clean ["tmp.out", "tmp.err", "tmp-missing.c"]) $ do
    htccCmd <- htccCommand
    result <- exec $ mconcat
        [ htccCmd
        , " -o tmp-missing.c tmp-missing.c > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    targetExists <- doesFileExist "tmp-missing.c"
    let failed = exitCode (const True) False result
        hasAliasError = "-o output path must not overwrite an input file" `T.isInfixOf` stderrOut
        mentionsInputPath = "tmp-missing.c" `T.isInfixOf` stderrOut
        ok =
            failed
                && T.null stdoutLeak
                && not targetExists
                && mentionsInputPath
                && not hasAliasError
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "targetExists: " <> T.pack (show targetExists)
            , "mentionsInputPath: " <> T.pack (show mentionsInputPath)
            , "hasAliasError: " <> T.pack (show hasAliasError)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult outputFileMissingInputMatchingOutputPathMsg ok details

visualizeAstDefaultOutputSamePathTest :: IO (Either T.Text T.Text, String)
visualizeAstDefaultOutputSamePathTest = do
    targetDir <- createFreshTempDir "tmp-visualize-ast-default-output"
    flip finally (clean [targetDir]) $ do
        htccCmd <- absoluteHtccCommand
        let inputPath = targetDir </> "out.svg"
            stdoutPath = targetDir </> "tmp.out"
            stderrPath = targetDir </> "tmp.err"
        T.writeFile inputPath source
        result <- exec $ mconcat
            [ "cd "
            , shellQuote $ T.pack targetDir
            , " && "
            , htccCmd
            , " --visualize-ast out.svg > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile stdoutPath
        stderrOut <- T.readFile stderrPath
        contents <- T.readFile inputPath
        let failed = exitCode (const True) False result
            hasAliasError = "--visualize-ast output path must not overwrite an input file: ./out.svg" `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && contents == source && hasAliasError
            details = T.unlines
                [ "targetDir: " <> T.pack targetDir
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "inputUnchanged: " <> T.pack (show (contents == source))
                , "hasAliasError: " <> T.pack (show hasAliasError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstDefaultOutputSamePathMsg ok details

visualizeAstDefaultOutputMissingInputTest :: IO (Either T.Text T.Text, String)
visualizeAstDefaultOutputMissingInputTest = do
    targetDir <- createFreshTempDir "tmp-visualize-ast-default-output-missing"
    flip finally (clean [targetDir]) $ do
        htccCmd <- absoluteHtccCommand
        let inputPath = targetDir </> "out.svg"
            stdoutPath = targetDir </> "tmp.out"
            stderrPath = targetDir </> "tmp.err"
            aliasError = "--visualize-ast output path must not overwrite an input file: ./out.svg"
        result <- exec $ mconcat
            [ "cd "
            , shellQuote $ T.pack targetDir
            , " && "
            , htccCmd
            , " --visualize-ast out.svg > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile stdoutPath
        stderrOut <- T.readFile stderrPath
        inputExists <- doesFileExist inputPath
        let failed = exitCode (const True) False result
            hasAliasError = aliasError `T.isInfixOf` stderrOut
            mentionsInputPath = "out.svg" `T.isInfixOf` stderrOut
            hasMissingInputDiagnostic = "no such file" `T.isInfixOf` T.toLower stderrOut
            hidesCallStack = not $ "HasCallStack" `T.isInfixOf` stderrOut
            hidesWithFile = not $ "withFile" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && not inputExists
                    && mentionsInputPath
                    && hasMissingInputDiagnostic
                    && hidesCallStack
                    && hidesWithFile
                    && not hasAliasError
            details = T.unlines
                [ "targetDir: " <> T.pack targetDir
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "inputExists: " <> T.pack (show inputExists)
                , "mentionsInputPath: " <> T.pack (show mentionsInputPath)
                , "hasMissingInputDiagnostic: " <> T.pack (show hasMissingInputDiagnostic)
                , "hidesCallStack: " <> T.pack (show hidesCallStack)
                , "hidesWithFile: " <> T.pack (show hidesWithFile)
                , "hasAliasError: " <> T.pack (show hasAliasError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstDefaultOutputMissingInputMsg ok details

visualizeAstSingleInputPrototypeRetypeTest :: IO (Either T.Text T.Text, String)
visualizeAstSingleInputPrototypeRetypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.svg", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.svg"
            inputPath = "tmp-single.c"
            expectedError = "too many arguments to function call"
        T.writeFile inputPath $ T.unlines
            [ "int foo();"
            , "int main(void) { return foo(1); }"
            , "int foo(void) { return 0; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstSingleInputPrototypeRetypeMsg ok details

visualizeAstUsesMergedTentativeArrayTypeTest :: IO (Either T.Text T.Text, String)
visualizeAstUsesMergedTentativeArrayTypeTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.svg", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.svg"
            inputPath = "tmp-single.c"
        T.writeFile inputPath $ T.unlines
            [ "int x[];"
            , "int *f(void) { return x; }"
            , "int x[4];"
            , "int main(void) { return 0; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        svg <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            hasMergedType = "auto int[4] x" `T.isInfixOf` svg
            hasStaleType = "auto int[] x" `T.isInfixOf` svg
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && hasMergedType
                    && not hasStaleType
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasMergedType: " <> T.pack (show hasMergedType)
                , "hasStaleType: " <> T.pack (show hasStaleType)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstUsesMergedTentativeArrayTypeMsg ok details

visualizeAstSingleInputImplicitFunctionDefinitionWarningTest :: IO (Either T.Text T.Text, String)
visualizeAstSingleInputImplicitFunctionDefinitionWarningTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.svg", "tmp-single.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.svg"
            inputPath = "tmp-single.c"
            expectedWarning = "warning: the function 'foo' is not declared."
        T.writeFile inputPath $ T.unlines
            [ "int main(void) { return foo(); }"
            , "int foo(void) { return 1; }"
            ]
        result <- exec $ mconcat
            [ htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        svg <- if targetExists then T.readFile target else pure ""
        let succeeded = exitCode (const False) True result
            hasExpectedWarning = expectedWarning `T.isInfixOf` stderrOut
            hasSvg = "<svg" `T.isInfixOf` svg
            ok =
                succeeded
                    && T.null stdoutLeak
                    && targetExists
                    && hasExpectedWarning
                    && hasSvg
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedWarning: " <> T.pack (show hasExpectedWarning)
                , "hasSvg: " <> T.pack (show hasSvg)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstSingleInputImplicitFunctionDefinitionWarningMsg ok details

outputFileHardLinkAliasTest :: IO (Either T.Text T.Text, String)
outputFileHardLinkAliasTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-source.c", "tmp-source-link.c"]) $ do
        htccCmd <- htccCommand
        T.writeFile "tmp-source.c" source
        execErrFin "ln tmp-source.c tmp-source-link.c"
        result <- exec $ mconcat
            [ htccCmd
            , " -o tmp-source-link.c tmp-source.c > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        inputContents <- T.readFile "tmp-source.c"
        outputContents <- T.readFile "tmp-source-link.c"
        let failed = exitCode (const True) False result
            hasAliasError = "-o output path must not overwrite an input file" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && inputContents == source
                    && outputContents == source
                    && hasAliasError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "inputUnchanged: " <> T.pack (show (inputContents == source))
                , "outputUnchanged: " <> T.pack (show (outputContents == source))
                , "hasAliasError: " <> T.pack (show hasAliasError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileHardLinkAliasMsg ok details

outputFileParseFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileParseFailurePreservesExistingOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-parse-error.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-parse-error.c"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        T.writeFile inputPath parseFailureSource
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        inputContents <- T.readFile inputPath
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            mentionsInput = T.pack inputPath `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && inputContents == parseFailureSource
                    && mentionsInput
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "inputUnchanged: " <> T.pack (show (inputContents == parseFailureSource))
                , "mentionsInput: " <> T.pack (show mentionsInput)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileParseFailurePreservesExistingOutputMsg ok details

outputFileReadFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileReadFailurePreservesExistingOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp.s", "tmp-missing.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp.s"
            inputPath = "tmp-missing.c"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        result <- exec $ mconcat
            [ htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        inputExists <- doesFileExist inputPath
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            mentionsInput = T.pack inputPath `T.isInfixOf` stderrOut
            hasMissingInputDiagnostic = "no such file" `T.isInfixOf` T.toLower stderrOut
            hidesCallStack = not $ "HasCallStack" `T.isInfixOf` stderrOut
            hidesWithFile = not $ "withFile" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && not inputExists
                    && mentionsInput
                    && hasMissingInputDiagnostic
                    && hidesCallStack
                    && hidesWithFile
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "inputExists: " <> T.pack (show inputExists)
                , "mentionsInput: " <> T.pack (show mentionsInput)
                , "hasMissingInputDiagnostic: " <> T.pack (show hasMissingInputDiagnostic)
                , "hidesCallStack: " <> T.pack (show hidesCallStack)
                , "hidesWithFile: " <> T.pack (show hidesWithFile)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileReadFailurePreservesExistingOutputMsg ok details

outputFileOpenFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileOpenFailurePreservesExistingOutputTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.s"
        clean [targetDir, "tmp.out", "tmp.err"]
        createDirectoryIfMissing False targetDir
        T.writeFile target "stale output"
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , htccCmd
            , " -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            hasPermissionError = "permission" `T.isInfixOf` T.toLower stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
                    && hasPermissionError
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "hasPermissionError: " <> T.pack (show hasPermissionError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileOpenFailurePreservesExistingOutputMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err"]

outputFileHardLinkedRenameReplacementPreservesAliasTest :: IO (Either T.Text T.Text, String)
outputFileHardLinkedRenameReplacementPreservesAliasTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-hard-link-output.s", "tmp-hard-link-output-link.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-hard-link-output.s"
            alias = "tmp-hard-link-output-link.s"
            staleOutput = "stale output\n"
        clean [target, alias, "tmp.out", "tmp.err"]
        T.writeFile target staleOutput
        execErrFin $ "ln '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , htccCmd
            , " -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        aliasExists <- doesFileExist alias
        targetContents <- if targetExists then T.readFile target else pure ""
        aliasContents <- if aliasExists then T.readFile alias else pure ""
        let succeeded = exitCode (const False) True result
            targetUpdated =
                targetContents /= staleOutput
                    && all (`T.isInfixOf` targetContents)
                        [ ".intel_syntax noprefix"
                        , ".global main"
                        , ".L.return.main:"
                        ]
            aliasPreserved = aliasContents == staleOutput
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && aliasExists
                    && targetUpdated
                    && aliasPreserved
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "aliasExists: " <> T.pack (show aliasExists)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "aliasPreserved: " <> T.pack (show aliasPreserved)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileHardLinkedRenameReplacementPreservesAliasMsg ok details

outputFileReadOnlyParentWritableTargetTest :: IO (Either T.Text T.Text, String)
outputFileReadOnlyParentWritableTargetTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.s"
            targetMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                ]
        clean [targetDir, "tmp.out", "tmp.err"]
        createDirectoryIfMissing False targetDir
        T.writeFile target "stale output"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , htccCmd
            , " -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        asm <- T.readFile target
        let succeeded = exitCode (const False) True result
            hasAsm = all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && hasAsm
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasAsm: " <> T.pack (show hasAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileReadOnlyParentWritableTargetMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err"]

outputFileReadOnlyParentWriteOnlyTargetTest :: IO (Either T.Text T.Text, String)
outputFileReadOnlyParentWriteOnlyTargetTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-write-only.s"
            targetMode = ownerWriteMode
        clean [targetDir, "tmp.out", "tmp.err"]
        createDirectoryIfMissing False targetDir
        T.writeFile target "stale output"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , htccCmd
            , " -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        execErrFin $ "chmod 755 '" <> T.pack targetDir <> "'"
        when targetExists $
            setFileMode target $ targetMode `unionFileModes` ownerReadMode
        asm <- if targetExists then T.readFile target else pure ""
        let replacedPermissions = permissionBits replacedMode
        let succeeded = exitCode (const False) True result
            hasAsm = all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && replacedPermissions == targetMode
                    && hasAsm
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "hasAsm: " <> T.pack (show hasAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileReadOnlyParentWriteOnlyTargetMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err"]

outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.s"
            alias = targetDir </> "tmp-read-only-link.s"
            expectedError = "hard-linked output"
            staleOutput = "stale output"
        clean [targetDir, "tmp.out", "tmp.err"]
        createDirectoryIfMissing False targetDir
        T.writeFile target staleOutput
        execErrFin $ "ln '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , htccCmd
            , " -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        aliasExists <- doesFileExist alias
        execErrFin $ "chmod 755 '" <> T.pack targetDir <> "'"
        targetContents <- if targetExists then T.readFile target else pure ""
        aliasContents <- if aliasExists then T.readFile alias else pure ""
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            preservedTarget = targetContents == staleOutput
            preservedAlias = aliasContents == staleOutput
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && targetExists
                    && aliasExists
                    && preservedTarget
                    && preservedAlias
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "aliasExists: " <> T.pack (show aliasExists)
                , "preservedTarget: " <> T.pack (show preservedTarget)
                , "preservedAlias: " <> T.pack (show preservedAlias)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileReadOnlyParentHardLinkAliasPreservesExistingOutputMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err"]

outputFileReadOnlyParentWriteFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileReadOnlyParentWriteFailurePreservesExistingOutputTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.s"
            inputPath = "tmp-large.c"
            targetMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                ]
        clean [targetDir, inputPath, "tmp.out", "tmp.err"]
        createDirectoryIfMissing False targetDir
        T.writeFile target "stale output"
        setFileMode target targetMode
        T.writeFile inputPath writeFailureSource
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "sh -c \"ulimit -f 1; "
            , htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err\""
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileReadOnlyParentWriteFailurePreservesExistingOutputMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp-large.c", "tmp.out", "tmp.err"]

outputFileWriteFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
outputFileWriteFailurePreservesExistingOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-limit.s", "tmp-large.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-limit.s"
            inputPath = "tmp-large.c"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        T.writeFile inputPath writeFailureSource
        result <- exec $ mconcat
            [ "sh -c \"ulimit -f 1; "
            , htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err\""
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileWriteFailurePreservesExistingOutputMsg ok details

visualizeAstWriteFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
visualizeAstWriteFailurePreservesExistingOutputTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-limit.svg", "tmp-large.c"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-limit.svg"
            inputPath = "tmp-large.c"
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile target "stale output"
        T.writeFile inputPath writeFailureSource
        result <- exec $ mconcat
            [ "sh -c \"ulimit -f 1; "
            , htccCmd
            , " --visualize-ast --out "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err\""
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        let failed = exitCode (const True) False result
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && targetContents == "stale output"
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult visualizeAstWriteFailurePreservesExistingOutputMsg ok details

outputFileFreshOutputRestrictiveUmaskTest :: IO (Either T.Text T.Text, String)
outputFileFreshOutputRestrictiveUmaskTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-umask-input.c", "tmp-umask.s"]) $ do
        htccCmd <- htccCommand
        let target = "tmp-umask.s"
            inputPath = "tmp-umask-input.c"
            expectedMode = foldr1 unionFileModes
                [ ownerReadMode
                , groupReadMode
                , otherReadMode
                ]
        clean [target, inputPath, "tmp.out", "tmp.err"]
        T.writeFile inputPath source
        result <- exec $ mconcat
            [ "sh -c \"umask 0222; "
            , htccCmd
            , " -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err\""
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        asm <- if targetExists then T.readFile target else pure ""
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        let replacedPermissions = permissionBits replacedMode
        let succeeded = exitCode (const False) True result
            hasAsm = all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".global main"
                , ".L.return.main:"
                ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && replacedPermissions == expectedMode
                    && hasAsm
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "expectedMode: " <> T.pack (show expectedMode)
                , "hasAsm: " <> T.pack (show hasAsm)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult outputFileFreshOutputRestrictiveUmaskMsg ok details

runAsmTest :: IO (Either T.Text T.Text, String)
runAsmTest = flip finally (clean ["tmp", "tmp.out", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
    htccCmd <- htccCommand
    writeFakeAssembler fakeAssemblerPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER=./tmp-assembler.sh "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
    asm <- T.readFile fakeAssemblerAsmPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult runAsmMsg ok details

runAsmDoesNotInjectValidationMarkerIntoFinalAsmTest :: IO (Either T.Text T.Text, String)
runAsmDoesNotInjectValidationMarkerIntoFinalAsmTest =
    if not hostCanExecuteGeneratedElf
        then pure (Right runAsmDoesNotInjectValidationMarkerIntoFinalAsmMsg, "")
        else
            flip finally
                (clean ["tmp-stable-marker-bin", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath])
                $ do
                    htccCmd <- htccCommand
                    writeFakeAssembler fakeAssemblerPath
                    let target = "tmp-stable-marker-bin"
                        runOnce = do
                            execErrFin $ mconcat
                                [ "echo '"
                                , source
                                , "' | "
                                , "HTCC_ASSEMBLER=./tmp-assembler.sh "
                                , htccCmd
                                , " -r -o "
                                , T.pack target
                                , " /dev/stdin > tmp.out 2> tmp.err"
                                ]
                            asm <- T.readFile fakeAssemblerAsmPath
                            stdoutLeak <- T.readFile "tmp.out"
                            stderrLeak <- T.readFile "tmp.err"
                            pure (asm, stdoutLeak, stderrLeak)
                    (firstAsm, firstStdout, firstStderr) <- runOnce
                    (secondAsm, secondStdout, secondStderr) <- runOnce
                    targetExists <- doesFileExist target
                    runResult <-
                        if targetExists
                            then Just <$> exec ("./" <> T.pack target)
                            else pure Nothing
                    let validationMarker = "htcc-output-marker:runnable-linked-output"
                        markerCount = T.count validationMarker
                        stableAsm = firstAsm == secondAsm
                        markerAbsent =
                            markerCount firstAsm == 0
                                && markerCount secondAsm == 0
                        leaksTempNames =
                            any
                                (\needle ->
                                    needle `T.isInfixOf` firstAsm || needle `T.isInfixOf` secondAsm
                                )
                                ["htcc-.s", "htcc-.o", ".htcc-"]
                        ranOk = maybe False (exitCode (const False) True) runResult
                        ok =
                            T.null firstStdout
                                && T.null firstStderr
                                && T.null secondStdout
                                && T.null secondStderr
                                && stableAsm
                                && markerAbsent
                                && not leaksTempNames
                                && ranOk
                        details = T.unlines
                            [ "firstStdout:"
                            , firstStdout
                            , "firstStderr:"
                            , firstStderr
                            , "secondStdout:"
                            , secondStdout
                            , "secondStderr:"
                            , secondStderr
                            , "stableAsm: " <> T.pack (show stableAsm)
                            , "markerCount(first): " <> T.pack (show $ markerCount firstAsm)
                            , "markerCount(second): " <> T.pack (show $ markerCount secondAsm)
                            , "markerAbsent: " <> T.pack (show markerAbsent)
                            , "leaksTempNames: " <> T.pack (show leaksTempNames)
                            , "targetExists: " <> T.pack (show targetExists)
                            , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                            , "firstAsm:"
                            , firstAsm
                            , "secondAsm:"
                            , secondAsm
                            ]
                    return $ mkResult runAsmDoesNotInjectValidationMarkerIntoFinalAsmMsg ok details

runAsmAcceptsUserDefinedValidationMarkerSymbolTest :: IO (Either T.Text T.Text, String)
runAsmAcceptsUserDefinedValidationMarkerSymbolTest =
    if not hostCanExecuteGeneratedElf
        then pure (Right runAsmAcceptsUserDefinedValidationMarkerSymbolMsg, "")
        else do
            maybeGcc <- findExecutable "gcc"
            case maybeGcc of
                Nothing ->
                    pure (Right runAsmAcceptsUserDefinedValidationMarkerSymbolMsg, "")
                Just _ ->
                    flip finally
                        (clean ["tmp-user-marker", "tmp-user-marker.c", "tmp.out", "tmp.err"])
                        $ do
                            htccCmd <- htccCommand
                            let target = "tmp-user-marker"
                                inputPath = "tmp-user-marker.c"
                            T.writeFile inputPath $
                                T.unlines
                                    [ "long htcc_runnable_output_marker = 7;"
                                    , "int main(void) { return htcc_runnable_output_marker != 7; }"
                                    ]
                            result <- exec $ mconcat
                                [ "HTCC_ASSEMBLER=gcc "
                                , htccCmd
                                , " -r -o "
                                , T.pack target
                                , " "
                                , T.pack inputPath
                                , " > tmp.out 2> tmp.err"
                                ]
                            stdoutLeak <- T.readFile "tmp.out"
                            stderrOut <- T.readFile "tmp.err"
                            targetExists <- doesFileExist target
                            runResult <-
                                if targetExists
                                    then Just <$> exec ("./" <> T.pack target)
                                    else pure Nothing
                            let succeeded = exitCode (const False) True result
                                ranOk = maybe False (exitCode (const False) True) runResult
                                multipleDefinition =
                                    "multiple definition" `T.isInfixOf` stderrOut
                                ok =
                                    succeeded
                                        && T.null stdoutLeak
                                        && T.null stderrOut
                                        && targetExists
                                        && ranOk
                                details = T.unlines
                                    [ "stdout:"
                                    , stdoutLeak
                                    , "stderr:"
                                    , stderrOut
                                    , "targetExists: " <> T.pack (show targetExists)
                                    , "multipleDefinition: " <> T.pack (show multipleDefinition)
                                    , "compileExitCode: " <> T.pack (show result)
                                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                                    ]
                            pure $
                                mkResult
                                    runAsmAcceptsUserDefinedValidationMarkerSymbolMsg
                                    ok
                                    details

runAsmAcceptsGcSectionsLinkerTest :: IO (Either T.Text T.Text, String)
runAsmAcceptsGcSectionsLinkerTest =
    if os == "mingw32"
        then pure (Right runAsmAcceptsGcSectionsLinkerMsg, "")
        else
            flip finally
                (clean
                    [ "tmp-gc-sections"
                    , "tmp.out"
                    , "tmp.err"
                    , fakeGcSectionsLinkDriverPath
                    , fakeGcSectionsLinkDriverLogPath
                    ]
                )
                $ do
                    htccCmd <- htccCommand
                    writeGcSectionsLinkDriver
                        fakeGcSectionsLinkDriverPath
                        fakeGcSectionsLinkDriverLogPath
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HTCC_ASSEMBLER=./"
                        , T.pack fakeGcSectionsLinkDriverPath
                        , " "
                        , htccCmd
                        , " -r -o tmp-gc-sections /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    targetExists <- doesFileExist "tmp-gc-sections"
                    driverInvocations <-
                        catchIOError
                            (T.lines <$> T.readFile fakeGcSectionsLinkDriverLogPath)
                            (const $ pure [])
                    let succeeded = exitCode (const False) True result
                        rejectedForMissingMarker =
                            "non-runnable final output for -r" `T.isInfixOf` stderrOut
                        sawProbeLink =
                            any
                                (\line ->
                                    "-no-pie -o" `T.isInfixOf` line
                                        && "htcc-probe-" `T.isInfixOf` line
                                )
                                driverInvocations
                        sawFinalLink =
                            any
                                (\line ->
                                    "-no-pie -o" `T.isInfixOf` line
                                        && "tmp-gc-sections.htcc-" `T.isInfixOf` line
                                )
                                driverInvocations
                        ok =
                            succeeded
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && targetExists
                                && sawProbeLink
                                && sawFinalLink
                                && not rejectedForMissingMarker
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "driverInvocations:"
                            , T.unlines driverInvocations
                            , "targetExists: " <> T.pack (show targetExists)
                            , "rejectedForMissingMarker: " <> T.pack (show rejectedForMissingMarker)
                            , "sawProbeLink: " <> T.pack (show sawProbeLink)
                            , "sawFinalLink: " <> T.pack (show sawFinalLink)
                            , "compileExitCode: " <> T.pack (show result)
                            ]
                    pure $ mkResult runAsmAcceptsGcSectionsLinkerMsg ok details

runAsmSingleInputImplicitFunctionConflictTest :: IO (Either T.Text T.Text, String)
runAsmSingleInputImplicitFunctionConflictTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", "tmp-single.c", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp"
            inputPath = "tmp-single.c"
            expectedError = "multiple external definitions in multi-input -o mode: foo"
        writeFakeAssembler fakeAssemblerPath
        T.writeFile inputPath $ T.unlines
            [ "int foo;"
            , "int main(void) { return foo(); }"
            ]
        result <- exec $ mconcat
            [ "HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        driverLogExists <- doesFileExist fakeAssemblerLogPath
        driverInvocations <- if driverLogExists then T.lines <$> T.readFile fakeAssemblerLogPath else pure []
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            ok = failed && T.null stdoutLeak && hasExpectedError && not targetExists && null driverInvocations
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmSingleInputImplicitFunctionConflictMsg ok details

runAsmSpecialPathDevNullTest :: IO (Either T.Text T.Text, String)
runAsmSpecialPathDevNullTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-devnull-run.c", fakeDriverPath]) $ do
        htccCmd <- htccCommand
        let inputPath = "tmp-devnull-run.c"
        clean ["tmp.out", "tmp.err", inputPath, fakeDriverPath]
        writeSpecialOutputDriver fakeDriverPath
        T.writeFile inputPath source
        result <- exec $ mconcat
            [ "HTCC_ASSEMBLER=./"
            , T.pack fakeDriverPath
            , " "
            , htccCmd
            , " -r -o /dev/null "
            , T.pack inputPath
            , " > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        let succeeded = exitCode (const False) True result
            ok = succeeded && T.null stdoutLeak && T.null stderrOut
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmSpecialPathDevNullMsg ok details

runAsmSpecialPathDevNullSkipsPostLinkValidationWaitTest :: IO (Either T.Text T.Text, String)
runAsmSpecialPathDevNullSkipsPostLinkValidationWaitTest =
    flip finally
        ( clean
            [ "tmp.out"
            , "tmp.err"
            , "tmp-devnull-delayed-run.c"
            , fakeDriverPath
            , fakeDriverLogPath
            , fakeDelayedChildFinalLinkOnlyWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            let inputPath = "tmp-devnull-delayed-run.c"
            clean
                [ "tmp.out"
                , "tmp.err"
                , inputPath
                , fakeDriverPath
                , fakeDriverLogPath
                , fakeDelayedChildFinalLinkOnlyWrapperPath
                ]
            writeLoggingDriver fakeDriverPath fakeDriverLogPath
            writeDelayedChildFinalLinkOnlyWrapper
                fakeDelayedChildFinalLinkOnlyWrapperPath
                fakeDriverPath
            T.writeFile inputPath source
            maybeResult <- timeout 4000000 $
                exec $ mconcat
                    [ "HTCC_ASSEMBLER='./"
                    , T.pack fakeDelayedChildFinalLinkOnlyWrapperPath
                    , "' "
                    , htccCmd
                    , " -r -o /dev/null "
                    , T.pack inputPath
                    , " > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            driverInvocations <- catchIOError
                (T.lines <$> T.readFile fakeDriverLogPath)
                (const $ pure [])
            let completed = maybe False (exitCode (const False) True) maybeResult
                waitedForDelayedChild = not (null driverInvocations)
                ok = completed && T.null stdoutLeak && T.null stderrOut && waitedForDelayedChild
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "waitedForDelayedChild: " <> T.pack (show waitedForDelayedChild)
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $
                mkResult
                    runAsmSpecialPathDevNullSkipsPostLinkValidationWaitMsg
                    ok
                    details

runAsmPreservesExecutableBitsTest :: IO (Either T.Text T.Text, String)
runAsmPreservesExecutableBitsTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp"
            originalMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                ]
            executeModeMask = foldr1 unionFileModes
                [ ownerExecuteMode
                , groupExecuteMode
                , otherExecuteMode
                ]
            expectedMode = originalMode `unionFileModes` ownerExecuteMode
        writeFakeAssembler fakeAssemblerPath
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        replacedMode <- fileMode <$> getFileStatus target
        let replacedPermissions = permissionBits replacedMode
        setFileMode target $ replacedMode `unionFileModes` ownerReadMode `unionFileModes` ownerExecuteMode
        programResult <- exec "./tmp"
        let replacedExecuteMode = intersectFileModes replacedMode executeModeMask
            ranOk = exitCode (const False) True programResult
            succeeded = exitCode (const False) True result
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && replacedPermissions == expectedMode
                    && replacedExecuteMode == ownerExecuteMode
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "replacedExecuteMode: " <> T.pack (show replacedExecuteMode)
                , "expectedMode: " <> T.pack (show expectedMode)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> T.pack (show programResult)
                ]
        return $ mkResult runAsmPreservesExecutableBitsMsg ok details

runAsmPreservesExistingExecuteMaskTest :: IO (Either T.Text T.Text, String)
runAsmPreservesExistingExecuteMaskTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp"
            originalMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , ownerExecuteMode
                ]
            executeModeMask = foldr1 unionFileModes
                [ ownerExecuteMode
                , groupExecuteMode
                , otherExecuteMode
                ]
        writeFakeAssembler fakeAssemblerPath
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        replacedMode <- fileMode <$> getFileStatus target
        let replacedPermissions = permissionBits replacedMode
        setFileMode target $ replacedMode `unionFileModes` ownerReadMode `unionFileModes` ownerExecuteMode
        programResult <- exec "./tmp"
        let replacedExecuteMode = intersectFileModes replacedMode executeModeMask
            succeeded = exitCode (const False) True result
            ranOk = exitCode (const False) True programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && replacedPermissions == originalMode
                    && replacedExecuteMode == ownerExecuteMode
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "replacedExecuteMode: " <> T.pack (show replacedExecuteMode)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> T.pack (show programResult)
                ]
        return $ mkResult runAsmPreservesExistingExecuteMaskMsg ok details

runAsmRestoresOwnerExecuteBitTest :: IO (Either T.Text T.Text, String)
runAsmRestoresOwnerExecuteBitTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp"
            originalMode = foldr1 unionFileModes
                [ groupReadMode
                , groupExecuteMode
                , otherReadMode
                , otherExecuteMode
                ]
            executeModeMask = foldr1 unionFileModes
                [ ownerExecuteMode
                , groupExecuteMode
                , otherExecuteMode
                ]
            expectedMode = originalMode `unionFileModes` ownerExecuteMode
            expectedExecuteMode = executeModeMask
        writeFakeAssembler fakeAssemblerPath
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        replacedMode <- fileMode <$> getFileStatus target
        let replacedPermissions = permissionBits replacedMode
        setFileMode target $ replacedMode `unionFileModes` ownerReadMode `unionFileModes` ownerExecuteMode
        programResult <- exec "./tmp"
        let replacedExecuteMode = intersectFileModes replacedMode executeModeMask
            succeeded = exitCode (const False) True result
            ranOk = exitCode (const False) True programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && replacedPermissions == expectedMode
                    && replacedExecuteMode == expectedExecuteMode
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "replacedExecuteMode: " <> T.pack (show replacedExecuteMode)
                , "expectedMode: " <> T.pack (show expectedMode)
                , "expectedExecuteMode: " <> T.pack (show expectedExecuteMode)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> T.pack (show programResult)
                ]
        return $ mkResult runAsmRestoresOwnerExecuteBitMsg ok details

runAsmFreshOutputInPlaceLinkDriverTest :: IO (Either T.Text T.Text, String)
runAsmFreshOutputInPlaceLinkDriverTest =
    flip finally (clean ["tmp-in-place-bin", "tmp.out", "tmp.err", fakeInPlaceLinkDriverPath, fakeInPlaceLinkDriverLogPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp-in-place-bin"
            executeModeMask = foldr1 unionFileModes
                [ ownerExecuteMode
                , groupExecuteMode
                , otherExecuteMode
                ]
        writeInPlaceLinkDriver fakeInPlaceLinkDriverPath fakeInPlaceLinkDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeInPlaceLinkDriverPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        driverInvocations <- T.lines <$> T.readFile fakeInPlaceLinkDriverLogPath
        programResult <- if targetExists then Just <$> exec (T.pack $ "./" <> target) else pure Nothing
        let hasExecuteBits = intersectFileModes replacedMode executeModeMask /= 0
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            succeeded = exitCode (const False) True result
            ranOk = maybe False (exitCode (const False) True) programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && hasExecuteBits
                    && sawLinkProbe
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExecuteBits: " <> T.pack (show hasExecuteBits)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> maybe "not-run" (T.pack . show) programResult
                ]
        return $ mkResult runAsmFreshOutputInPlaceLinkDriverMsg ok details

runAsmProbePreservesPrecreatedOutputTest :: IO (Either T.Text T.Text, String)
runAsmProbePreservesPrecreatedOutputTest =
    flip finally (clean ["tmp-probe-bin", "tmp.out", "tmp.err", fakeProbePrecreatedOutputDriverPath, fakeProbePrecreatedOutputDriverLogPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp-probe-bin"
        writeProbePrecreatedOutputDriver fakeProbePrecreatedOutputDriverPath fakeProbePrecreatedOutputDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeProbePrecreatedOutputDriverPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        driverInvocations <- T.lines <$> T.readFile fakeProbePrecreatedOutputDriverLogPath
        programResult <- if targetExists then Just <$> exec (T.pack $ "./" <> target) else pure Nothing
        let sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            succeeded = exitCode (const False) True result
            ranOk = maybe False (exitCode (const False) True) programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && sawLinkProbe
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "targetExists: " <> T.pack (show targetExists)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> maybe "not-run" (T.pack . show) programResult
                ]
        return $ mkResult runAsmProbePreservesPrecreatedOutputMsg ok details

runAsmClearsSpecialBitsTest :: IO (Either T.Text T.Text, String)
runAsmClearsSpecialBitsTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp"
            originalMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                , specialFileModeMask
                ]
        writeFakeAssembler fakeAssemblerPath
        T.writeFile target "stale output"
        setFileMode target originalMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        replacedMode <- fileMode <$> getFileStatus target
        programResult <- exec "./tmp"
        let specialBitsCleared = intersectFileModes replacedMode specialFileModeMask == 0
            succeeded = exitCode (const False) True result
            ranOk = exitCode (const False) True programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && specialBitsCleared
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "specialBitsCleared: " <> T.pack (show specialBitsCleared)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> T.pack (show programResult)
                ]
        return $ mkResult runAsmClearsSpecialBitsMsg ok details

runAsmLinkUsesResolvedDriverTest :: IO (Either T.Text T.Text, String)
runAsmLinkUsesResolvedDriverTest =
    flip finally (clean ["tmp", "tmp.out", fakeDriverPath, fakeDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeLoggingDriver fakeDriverPath fakeDriverLogPath
        execErrFin $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        driverInvocations <- T.lines <$> T.readFile fakeDriverLogPath
        result <- exec "./tmp"
        let sawAssembleInvocation =
                any
                    (\line -> "assemble:" `T.isPrefixOf` line && "-x assembler -c" `T.isInfixOf` line)
                    driverInvocations
            sawLinkInvocation =
                any
                    (\line -> "link:" `T.isPrefixOf` line && "-no-pie -o " `T.isInfixOf` line)
                    driverInvocations
            ranOk = exitCode (const False) True result
            ok = T.null stdoutLeak && sawAssembleInvocation && sawLinkInvocation && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmLinkUsesResolvedDriverMsg ok details

runAsmBareLocalAssemblerPathTest :: IO (Either T.Text T.Text, String)
runAsmBareLocalAssemblerPathTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerAsmPath
            , fakePathBinDir
            , fakePathAssemblerLogPath
            , fakeLocalAssemblerLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writeFakeAssemblerWithLogs fakePathAssemblerLogPath fakeAssemblerAsmPath fakePathAssemblerPath
            writeFailingCompiler fakeAssemblerPath fakeLocalAssemblerLogPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=./"
                , T.pack fakePathBinDir
                , ":$PATH "
                , "HTCC_ASSEMBLER=tmp-assembler.sh "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            compilerArgs <- T.lines <$> T.readFile fakePathAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            localAssemblerWasInvoked <- doesFileExist fakeLocalAssemblerLogPath
            result <- exec "./tmp"
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = exitCode (const False) True result
                ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk && not localAssemblerWasInvoked
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "localAssemblerWasInvoked: " <> T.pack (show localAssemblerWasInvoked)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmBareLocalAssemblerPathMsg ok details

runAsmQuotedCompilerTest :: IO (Either T.Text T.Text, String)
runAsmQuotedCompilerTest = flip finally (clean ["tmp", "tmp.out", fakeAssemblerQuotedPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
    htccCmd <- htccCommand
    writeFakeAssembler fakeAssemblerQuotedPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER='./"
        , T.pack fakeAssemblerQuotedPath
        , "' "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
    asm <- T.readFile fakeAssemblerAsmPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult runAsmQuotedCompilerMsg ok details

runAsmWrappedAssemblerTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerTest = flip finally (clean ["tmp", "tmp.out", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
    htccCmd <- htccCommand
    writeFakeAssembler fakeAssemblerPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER='./tmp-assembler.sh "
        , fakeAssemblerWrapperArg
        , "' "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
    asm <- T.readFile fakeAssemblerAsmPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ fakeAssemblerWrapperArg
                , "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult runAsmWrappedAssemblerMsg ok details

runAsmWrappedAssemblerFirstWordDriverTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerFirstWordDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeDriverPath
            , fakeDriverLogPath
            , fakeCombinedDriverPath
            , fakeCombinedDriverLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeForwardingDriverWrapper fakeAssemblerPath
            writeLoggingDriver fakeDriverPath fakeDriverLogPath
            writeFailingCompiler fakeCombinedDriverPath fakeCombinedDriverLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=.:$PATH "
                , "HTCC_ASSEMBLER='"
                , T.pack fakeAssemblerPath
                , " "
                , T.pack fakeDriverPath
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            driverInvocations <- do
                logExists <- doesFileExist fakeDriverLogPath
                if logExists
                    then T.lines <$> T.readFile fakeDriverLogPath
                    else pure []
            combinedDriverWasInvoked <- doesFileExist fakeCombinedDriverLogPath
            outputExists <- doesFileExist "tmp"
            ranOk <- if outputExists
                then exitCode (const False) True <$> exec "./tmp"
                else pure False
            let sawAssembleInvocation = any ("assemble:" `T.isPrefixOf`) driverInvocations
                sawLinkInvocation = any ("link:" `T.isPrefixOf`) driverInvocations
                ok =
                    exitCode (const False) True result
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && sawAssembleInvocation
                        && sawLinkInvocation
                        && not combinedDriverWasInvoked
                        && outputExists
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "combinedDriverWasInvoked: " <> T.pack (show combinedDriverWasInvoked)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmWrappedAssemblerFirstWordDriverMsg ok details

runAsmWrappedAssemblerProbeFallbackTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerProbeFallbackTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeProbeWrapperPath
            , fakeProbeWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeProbeRejectingWrapper fakeProbeWrapperPath fakeProbeWrapperLogPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeProbeWrapperPath
                , " "
                , fakeAssemblerWrapperArg
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            wrapperInvocations <- T.lines <$> T.readFile fakeProbeWrapperLogPath
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            result <- exec "./tmp"
            let sawProbeAttempt =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ fakeAssemblerWrapperArg
                        , "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && sawProbeAttempt
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmWrappedAssemblerProbeFallbackMsg ok details

runAsmWrappedAssemblerHostMetadataFallbackTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerHostMetadataFallbackTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeHostMetadataWrapperPath
            , fakeHostMetadataWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeHostMetadataWrapper fakeHostMetadataWrapperPath fakeHostMetadataWrapperLogPath fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeHostMetadataWrapperPath
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            wrapperInvocations <- T.lines <$> T.readFile fakeHostMetadataWrapperLogPath
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            result <- exec "./tmp"
            let sawHostMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && T.null stderrOut
                        && sawHostMetadataProbe
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "runExitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmWrappedAssemblerHostMetadataFallbackMsg ok details

runAsmWrappedAssemblerDelayedChildHostMetadataTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerDelayedChildHostMetadataTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeDelayedChildHostMetadataWrapperPath
            , fakeDelayedChildHostMetadataWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeDelayedChildHostMetadataWrapper
                fakeDelayedChildHostMetadataWrapperPath
                fakeDelayedChildHostMetadataWrapperLogPath
                fakeAssemblerPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='./"
                , T.pack fakeDelayedChildHostMetadataWrapperPath
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            wrapperInvocations <- T.lines <$> T.readFile fakeDelayedChildHostMetadataWrapperLogPath
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            runResult <- if hostCanExecuteGeneratedElf then Just <$> exec "./tmp" else pure Nothing
            let sawHostMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = maybe True (exitCode (const False) True) runResult
                ok =
                    T.null stdoutLeak
                        && T.null stderrOut
                        && sawHostMetadataProbe
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                    ]
            return $ mkResult runAsmWrappedAssemblerDelayedChildHostMetadataMsg ok details

runAsmWrappedAssemblerDelayedChildFinalOutputTest :: IO (Either T.Text T.Text, String)
runAsmWrappedAssemblerDelayedChildFinalOutputTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeDelayedChildFinalOutputWrapperPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeDelayedChildFinalOutputWrapper
                fakeDelayedChildFinalOutputWrapperPath
                fakeAssemblerPath
            maybeResult <- timeout 4000000 $
                exec $ mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeDelayedChildFinalOutputWrapperPath
                    , "' "
                    , htccCmd
                    , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            asm <- catchIOError
                (T.readFile fakeAssemblerAsmPath)
                (const $ pure "")
            targetExists <- doesFileExist "tmp"
            runResult <- if hostCanExecuteGeneratedElf && targetExists then Just <$> exec "./tmp" else pure Nothing
            let completed = maybe False (exitCode (const False) True) maybeResult
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = maybe True (exitCode (const False) True) runResult
                ok =
                    completed
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && targetExists
                        && ranOk
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $ mkResult runAsmWrappedAssemblerDelayedChildFinalOutputMsg ok details

runAsmDoesNotWaitOnBackgroundProcessGroupHelpersTest :: IO (Either T.Text T.Text, String)
runAsmDoesNotWaitOnBackgroundProcessGroupHelpersTest =
    flip finally
        (cleanupBackgroundProcessGroupHelperPids fakeBackgroundProcessGroupHelperPidPath
            *> clean
                [ "tmp"
                , "tmp.out"
                , "tmp.err"
                , fakeAssemblerPath
                , fakeAssemblerLogPath
                , fakeAssemblerAsmPath
                , fakeBackgroundProcessGroupHelperWrapperPath
                , fakeBackgroundProcessGroupHelperPidPath
                ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeBackgroundProcessGroupHelperWrapper
                fakeBackgroundProcessGroupHelperWrapperPath
                fakeBackgroundProcessGroupHelperPidPath
                fakeAssemblerPath
            maybeResult <- timeout 2500000 $
                exec $ mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeBackgroundProcessGroupHelperWrapperPath
                    , "' "
                    , htccCmd
                    , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            helperPids <- catchIOError
                (filter (not . T.null) . map T.strip . T.lines <$> T.readFile fakeBackgroundProcessGroupHelperPidPath)
                (const $ pure [])
            asm <- catchIOError
                (T.readFile fakeAssemblerAsmPath)
                (const $ pure "")
            targetExists <- doesFileExist "tmp"
            runResult <- if hostCanExecuteGeneratedElf && targetExists then Just <$> exec "./tmp" else pure Nothing
            let completed = maybe False (exitCode (const False) True) maybeResult
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                spawnedHelpers = length helperPids >= 3
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = maybe True (exitCode (const False) True) runResult
                ok =
                    completed
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && spawnedHelpers
                        && hasRequiredAsm
                        && targetExists
                        && ranOk
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "helperPids:"
                    , T.unlines helperPids
                    , "spawnedHelpers: " <> T.pack (show spawnedHelpers)
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $ mkResult runAsmDoesNotWaitOnBackgroundProcessGroupHelpersMsg ok details

runAsmProbeDoesNotHangOnInheritedPipeHandlesTest :: IO (Either T.Text T.Text, String)
runAsmProbeDoesNotHangOnInheritedPipeHandlesTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeProbeInheritedPipeHandlesWrapperPath
            , fakeProbeInheritedPipeHandlesWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeProbeInheritedPipeHandlesDriverWrapper
                fakeProbeInheritedPipeHandlesWrapperPath
                fakeProbeInheritedPipeHandlesWrapperLogPath
                fakeAssemblerPath
            maybeResult <- timeout 4000000 $
                exec $ mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeProbeInheritedPipeHandlesWrapperPath
                    , "' "
                    , htccCmd
                    , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            wrapperInvocations <- catchIOError
                (T.lines <$> T.readFile fakeProbeInheritedPipeHandlesWrapperLogPath)
                (const $ pure [])
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            targetExists <- doesFileExist "tmp"
            let completed = maybe False (exitCode (const False) True) maybeResult
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                sawAssemblyProbe =
                    any ("htcc-probe-" `T.isInfixOf`) wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ok =
                    completed
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && compilerSawExpectedArgs
                        && targetExists
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "compilerSawExpectedArgs: " <> T.pack (show compilerSawExpectedArgs)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    ]
            return $ mkResult runAsmProbeDoesNotHangOnInheritedPipeHandlesMsg ok details

runAsmProbeDoesNotHangOnEarlyClosedStdoutTest :: IO (Either T.Text T.Text, String)
runAsmProbeDoesNotHangOnEarlyClosedStdoutTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            , fakeEarlyClosedStdoutProbeWrapperPath
            , fakeEarlyClosedStdoutProbeWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            writeEarlyClosedStdoutProbeDriverWrapper
                fakeEarlyClosedStdoutProbeWrapperPath
                fakeEarlyClosedStdoutProbeWrapperLogPath
                fakeAssemblerPath
            maybeResult <- timeout 5000000 $
                exec $ mconcat
                    [ "echo '"
                    , source
                    , "' | "
                    , "HTCC_ASSEMBLER='./"
                    , T.pack fakeEarlyClosedStdoutProbeWrapperPath
                    , "' "
                    , htccCmd
                    , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                    ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            wrapperInvocations <- catchIOError
                (T.lines <$> T.readFile fakeEarlyClosedStdoutProbeWrapperLogPath)
                (const $ pure [])
            compilerArgs <- catchIOError
                (T.lines <$> T.readFile fakeAssemblerLogPath)
                (const $ pure [])
            targetExists <- doesFileExist "tmp"
            runResult <- if targetExists then Just <$> exec "./tmp" else pure Nothing
            let completed = maybe False (exitCode (const False) True) maybeResult
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperInvocations
                sawAssemblyProbe = any ("htcc-probe-" `T.isInfixOf`) wrapperInvocations
                compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                preservedFlood = "probe-stderr-flood" `T.isInfixOf` stderrOut
                ranOk = maybe False (exitCode (const False) True) runResult
                ok =
                    completed
                        && T.null stdoutLeak
                        && preservedFlood
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && compilerSawExpectedArgs
                        && targetExists
                        && ranOk
                details = T.unlines
                    [ "timedOut: " <> T.pack (show $ isNothing maybeResult)
                    , "stdout:"
                    , stdoutLeak
                    , "stderrHasFlood: " <> T.pack (show preservedFlood)
                    , "stderrBytes: " <> T.pack (show $ T.length stderrOut)
                    , "wrapperInvocations:"
                    , T.unlines wrapperInvocations
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "compilerSawExpectedArgs: " <> T.pack (show compilerSawExpectedArgs)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "compileExitCode: " <> maybe "timeout" (T.pack . show) maybeResult
                    , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                    ]
            pure $ mkResult runAsmProbeDoesNotHangOnEarlyClosedStdoutMsg ok details

runAsmTildeExpandedAssemblerPathTest :: IO (Either T.Text T.Text, String)
runAsmTildeExpandedAssemblerPathTest =
    if os == "mingw32"
        then pure (Right runAsmTildeExpandedAssemblerPathMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , "tmp-home"
                    , fakeAssemblerLogPath
                    , fakeAssemblerAsmPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    let fakeHomeDir = "tmp-home"
                        fakeHomeAssemblerPath = fakeHomeDir </> fakeAssemblerPath
                    createDirectoryIfMissing True fakeHomeDir
                    writeFakeAssemblerWithLogs
                        fakeAssemblerLogPath
                        fakeAssemblerAsmPath
                        fakeHomeAssemblerPath
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HOME=$(pwd)/"
                        , T.pack fakeHomeDir
                        , " "
                        , "HTCC_ASSEMBLER='~/"
                        , T.pack fakeAssemblerPath
                        , "' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
                    asm <- T.readFile fakeAssemblerAsmPath
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let compilerSawExpectedArgs =
                            all (`elem` compilerArgs)
                                [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                ]
                        hasRequiredAsm =
                            all (`T.isInfixOf` asm)
                                [ ".intel_syntax noprefix"
                                , ".L.return.main:"
                                , ".L.label.main.done:"
                                ]
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && compilerSawExpectedArgs
                                && hasRequiredAsm
                                && outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "compilerArgs:"
                            , T.unlines compilerArgs
                            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmTildeExpandedAssemblerPathMsg ok details

runAsmLeadingEnvAssignmentTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeAssemblerPath
            , fakeAssemblerAsmPath
            , fakePathBinDir
            , fakePathAssemblerLogPath
            , fakeLocalAssemblerLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writeFakeAssemblerWithLogs fakePathAssemblerLogPath fakeAssemblerAsmPath fakePathAssemblerPath
            writeFailingCompiler fakeAssemblerPath fakeLocalAssemblerLogPath
            execErrFin $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=.:$PATH "
                , "HTCC_ASSEMBLER='PATH=./"
                , T.pack fakePathBinDir
                , " tmp-assembler.sh' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            compilerArgs <- T.lines <$> T.readFile fakePathAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            localAssemblerWasInvoked <- doesFileExist fakeLocalAssemblerLogPath
            result <- exec "./tmp"
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                envAssignmentPassedAsArg =
                    any ("PATH=./" `T.isPrefixOf`) compilerArgs
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ranOk = exitCode (const False) True result
                ok =
                    T.null stdoutLeak
                        && compilerSawExpectedArgs
                        && not envAssignmentPassedAsArg
                        && not localAssemblerWasInvoked
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "envAssignmentPassedAsArg: " <> T.pack (show envAssignmentPassedAsArg)
                    , "localAssemblerWasInvoked: " <> T.pack (show localAssemblerWasInvoked)
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmLeadingEnvAssignmentMsg ok details

runAsmLeadingEnvAssignmentExpandsTildePathTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentExpandsTildePathTest =
    if os == "mingw32"
        then pure (Right runAsmLeadingEnvAssignmentExpandsTildePathMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , "tmp-home"
                    , fakePathAssemblerLogPath
                    , fakeAssemblerAsmPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    let fakeHomeDir = "tmp-home"
                        fakeHomeBinDir = fakeHomeDir </> "bin"
                        fakeHomeAssemblerPath = fakeHomeBinDir </> fakeAssemblerPath
                    createDirectoryIfMissing True fakeHomeBinDir
                    writeFakeAssemblerWithLogs
                        fakePathAssemblerLogPath
                        fakeAssemblerAsmPath
                        fakeHomeAssemblerPath
                    catPath <- maybe (ioError $ userError "missing cat executable for test") pure
                        =<< findExecutable "cat"
                    writeExecutableProxy (fakeHomeBinDir </> "cat") catPath
                    chmodPath <- maybe (ioError $ userError "missing chmod executable for test") pure
                        =<< findExecutable "chmod"
                    writeExecutableProxy (fakeHomeBinDir </> "chmod") chmodPath
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HOME=$(pwd)/"
                        , T.pack fakeHomeDir
                        , " "
                        , "HTCC_ASSEMBLER='PATH=~/bin tmp-assembler.sh' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    compilerArgs <- T.lines <$> T.readFile fakePathAssemblerLogPath
                    asm <- T.readFile fakeAssemblerAsmPath
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let compilerSawExpectedArgs =
                            all (`elem` compilerArgs)
                                [ "-x"
                                , "assembler"
                                , "-c"
                                , "-o"
                                ]
                        envAssignmentPassedAsArg =
                            any ("PATH=~/" `T.isPrefixOf`) compilerArgs
                                || any ("PATH=" `T.isPrefixOf`) compilerArgs
                        hasRequiredAsm =
                            all (`T.isInfixOf` asm)
                                [ ".intel_syntax noprefix"
                                , ".L.return.main:"
                                , ".L.label.main.done:"
                                ]
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && compilerSawExpectedArgs
                                && not envAssignmentPassedAsArg
                                && hasRequiredAsm
                                && outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "compilerArgs:"
                            , T.unlines compilerArgs
                            , "envAssignmentPassedAsArg: " <> T.pack (show envAssignmentPassedAsArg)
                            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmLeadingEnvAssignmentExpandsTildePathMsg ok details

runAsmExpandedAssignmentWordFailsTest :: IO (Either T.Text T.Text, String)
runAsmExpandedAssignmentWordFailsTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerAsmPath
            , fakePathBinDir
            , fakePathAssemblerLogPath
            , fakeLocalAssemblerLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writeFakeAssemblerWithLogs fakePathAssemblerLogPath fakeAssemblerAsmPath fakePathAssemblerPath
            writeFailingCompiler fakeAssemblerPath fakeLocalAssemblerLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=.:$PATH "
                , "WRAPPED_ASSEMBLER='PATH=./"
                , T.pack fakePathBinDir
                , " tmp-assembler.sh' "
                , "HTCC_ASSEMBLER='$WRAPPED_ASSEMBLER' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            pathAssemblerWasInvoked <- doesFileExist fakePathAssemblerLogPath
            localAssemblerWasInvoked <- doesFileExist fakeLocalAssemblerLogPath
            outputExists <- doesFileExist "tmp"
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed to determine an x86_64-ELF target from HTCC_ASSEMBLER"
                        `T.isInfixOf` stderrOut
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && not pathAssemblerWasInvoked
                        && not localAssemblerWasInvoked
                        && not outputExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "pathAssemblerWasInvoked: " <> T.pack (show pathAssemblerWasInvoked)
                    , "localAssemblerWasInvoked: " <> T.pack (show localAssemblerWasInvoked)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmExpandedAssignmentWordFailsMsg ok details

runAsmQuotedAssignmentLikeExecutableTest :: IO (Either T.Text T.Text, String)
runAsmQuotedAssignmentLikeExecutableTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "PATH=tmp-assembler.sh", fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
            htccCmd <- htccCommand
            let quotedAssemblerPath = "PATH=tmp-assembler.sh"
            writeFakeAssembler quotedAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='\""
                , T.pack quotedAssemblerPath
                , "\"' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            outputExists <- doesFileExist "tmp"
            ranOk <- if outputExists
                then exitCode (const False) True <$> exec "./tmp"
                else pure False
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ok =
                    exitCode (const False) True result
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && outputExists
                        && (not hostCanExecuteGeneratedElf || ranOk)
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                    , "ranOk: " <> T.pack (show ranOk)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmQuotedAssignmentLikeExecutableMsg ok details

runAsmEscapedAssignmentLikeExecutableTest :: IO (Either T.Text T.Text, String)
runAsmEscapedAssignmentLikeExecutableTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "FOO=bar", fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
            htccCmd <- htccCommand
            let escapedAssemblerPath = "FOO=bar"
            writeFakeAssembler escapedAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='FOO\\=bar' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            outputExists <- doesFileExist "tmp"
            ranOk <- if outputExists
                then exitCode (const False) True <$> exec "./tmp"
                else pure False
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ok =
                    exitCode (const False) True result
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && hasRequiredAsm
                        && outputExists
                        && (not hostCanExecuteGeneratedElf || ranOk)
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                    , "ranOk: " <> T.pack (show ranOk)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmEscapedAssignmentLikeExecutableMsg ok details

runAsmLeadingEnvAssignmentWithoutEnvPathTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentWithoutEnvPathTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakePathBinDir
            , fakePathAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writeFakeAssemblerWithLogs fakePathAssemblerLogPath fakeAssemblerAsmPath fakePathAssemblerPath
            catPath <- maybe (ioError $ userError "missing cat executable for test") pure
                =<< findExecutable "cat"
            writeExecutableProxy (fakePathBinDir </> "cat") catPath
            chmodPath <- maybe (ioError $ userError "missing chmod executable for test") pure
                =<< findExecutable "chmod"
            writeExecutableProxy (fakePathBinDir </> "chmod") chmodPath
            when (htccCmd == "htcc") $ do
                compilerPath <- maybe (ioError $ userError "missing htcc executable for test") pure
                    =<< findExecutable "htcc"
                writeExecutableProxy (fakePathBinDir </> "htcc") compilerPath
            when (htccCmd == "stack exec htcc --") $ do
                stackPath <- maybe (ioError $ userError "missing stack executable for test") pure
                    =<< findExecutable "stack"
                writeExecutableProxy (fakePathBinDir </> "stack") stackPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=./"
                , T.pack fakePathBinDir
                , " "
                , "HTCC_ASSEMBLER='PATH=./"
                , T.pack fakePathBinDir
                , " tmp-assembler.sh' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- T.lines <$> T.readFile fakePathAssemblerLogPath
            asm <- T.readFile fakeAssemblerAsmPath
            outputExists <- doesFileExist "tmp"
            ranOk <- if outputExists
                then exitCode (const False) True <$> exec "./tmp"
                else pure False
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                envAssignmentPassedAsArg =
                    any ("PATH=./" `T.isPrefixOf`) compilerArgs
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ok =
                    T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && not envAssignmentPassedAsArg
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "envAssignmentPassedAsArg: " <> T.pack (show envAssignmentPassedAsArg)
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmLeadingEnvAssignmentWithoutEnvPathMsg ok details

runAsmLeadingEnvAssignmentPreservesPathOverrideTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentPreservesPathOverrideTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakePathBinDir
            , fakePathWrapperLogPath
            , fakeHostPathHelperPath
            , fakeHostPathHelperLogPath
            , fakeHostPathHelperAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writePathLoggingWrapper fakePathAssemblerPath fakePathWrapperLogPath fakeHostPathHelperPath
            writeFakeAssemblerWithLogs fakeHostPathHelperLogPath fakeHostPathHelperAsmPath fakeHostPathHelperPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "PATH=.:$PATH "
                , "HTCC_ASSEMBLER='PATH=./"
                , T.pack fakePathBinDir
                , " "
                , T.pack fakeAssemblerPath
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            wrapperPathValue <- do
                wrapperLogExists <- doesFileExist fakePathWrapperLogPath
                if wrapperLogExists
                    then T.strip <$> T.readFile fakePathWrapperLogPath
                    else pure ""
            hostHelperWasInvoked <- doesFileExist fakeHostPathHelperLogPath
            outputExists <- doesFileExist "tmp"
            let failedAsExpected = exitCode (const True) False result
                hasExpectedError =
                    "failed to determine an x86_64-ELF target from HTCC_ASSEMBLER"
                        `T.isInfixOf` stderrOut
                preservedExactPath = wrapperPathValue == T.pack ("./" <> fakePathBinDir)
                ok =
                    T.null stdoutLeak
                        && failedAsExpected
                        && hasExpectedError
                        && preservedExactPath
                        && not hostHelperWasInvoked
                        && not outputExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "wrapperPath:"
                    , wrapperPathValue
                    , "preservedExactPath: " <> T.pack (show preservedExactPath)
                    , "hostHelperWasInvoked: " <> T.pack (show hostHelperWasInvoked)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmLeadingEnvAssignmentPreservesPathOverrideMsg ok details

runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangTest =
    if os == "mingw32"
        then pure (Right runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , fakeAssemblerPath
                    , fakeAssemblerLogPath
                    , fakeAssemblerAsmPath
                    , fakeEnvLoggingWrapperPath
                    , fakeEnvLoggingWrapperLogPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    writeFakeAssembler fakeAssemblerPath
                    writeEnvLoggingForwardingWrapper
                        fakeEnvLoggingWrapperPath
                        fakeEnvLoggingWrapperLogPath
                        ["MSG_PERCENT", "MSG_BANG"]
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HTCC_ASSEMBLER='MSG_PERCENT=%foo% MSG_BANG=!bar! ./"
                        , T.pack fakeEnvLoggingWrapperPath
                        , " "
                        , T.pack fakeAssemblerPath
                        , "' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    envLog <- do
                        logExists <- doesFileExist fakeEnvLoggingWrapperLogPath
                        if logExists
                            then T.lines <$> T.readFile fakeEnvLoggingWrapperLogPath
                            else pure []
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let linkedOk =
                            outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        preservedPercent = "MSG_PERCENT=%foo%" `elem` envLog
                        preservedBang = "MSG_BANG=!bar!" `elem` envLog
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && preservedPercent
                                && preservedBang
                                && linkedOk
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "envLog:"
                            , T.unlines envLog
                            , "preservedPercent: " <> T.pack (show preservedPercent)
                            , "preservedBang: " <> T.pack (show preservedBang)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmLeadingEnvAssignmentPreservesLiteralPercentAndBangMsg ok details

runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionTest =
    if os == "mingw32"
        then pure (Right runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , fakeAssemblerPath
                    , fakeAssemblerLogPath
                    , fakeAssemblerAsmPath
                    , fakeEnvLoggingWrapperPath
                    , fakeEnvLoggingWrapperLogPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    writeFakeAssembler fakeAssemblerPath
                    writeEnvLoggingForwardingWrapper
                        fakeEnvLoggingWrapperPath
                        fakeEnvLoggingWrapperLogPath
                        ["MSG"]
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HTCC_TEST_LITERAL='$HOME' "
                        , "HTCC_ASSEMBLER='MSG=$HTCC_TEST_LITERAL ./"
                        , T.pack fakeEnvLoggingWrapperPath
                        , " "
                        , T.pack fakeAssemblerPath
                        , "' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    envLog <- do
                        logExists <- doesFileExist fakeEnvLoggingWrapperLogPath
                        if logExists
                            then T.lines <$> T.readFile fakeEnvLoggingWrapperLogPath
                            else pure []
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let linkedOk =
                            outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        preservedLiteral = "MSG=$HOME" `elem` envLog
                        recursivelyExpanded =
                            any (\line -> "MSG=" `T.isPrefixOf` line && line /= "MSG=$HOME") envLog
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && preservedLiteral
                                && not recursivelyExpanded
                                && linkedOk
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "envLog:"
                            , T.unlines envLog
                            , "preservedLiteral: " <> T.pack (show preservedLiteral)
                            , "recursivelyExpanded: " <> T.pack (show recursivelyExpanded)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmLeadingEnvAssignmentPreservesSinglePassPosixExpansionMsg ok details

runAsmLeadingEnvAssignmentPreservesInheritedPwdTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentPreservesInheritedPwdTest =
    if os == "mingw32"
        then pure (Right runAsmLeadingEnvAssignmentPreservesInheritedPwdMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , "tmp-pwd-override"
                    , fakeAssemblerPath
                    , fakeAssemblerLogPath
                    , fakeAssemblerAsmPath
                    , fakeEnvLoggingWrapperPath
                    , fakeEnvLoggingWrapperLogPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    workingDir <- getCurrentDirectory
                    let logicalPwdDir = "tmp-pwd-override"
                        logicalPwd = workingDir </> logicalPwdDir
                        expectedMsgEntry = "MSG=" <> T.pack logicalPwd
                        expectedPwdEntry = "PWD=" <> T.pack logicalPwd
                    createDirectoryIfMissing False logicalPwdDir
                    writeFakeAssembler fakeAssemblerPath
                    writeEnvLoggingForwardingWrapper
                        fakeEnvLoggingWrapperPath
                        fakeEnvLoggingWrapperLogPath
                        ["MSG", "PWD"]
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "PWD="
                        , shellQuote (T.pack logicalPwd)
                        , " "
                        , "HTCC_ASSEMBLER='MSG=$PWD ./"
                        , T.pack fakeEnvLoggingWrapperPath
                        , " "
                        , T.pack fakeAssemblerPath
                        , "' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    envLog <- do
                        logExists <- doesFileExist fakeEnvLoggingWrapperLogPath
                        if logExists
                            then T.lines <$> T.readFile fakeEnvLoggingWrapperLogPath
                            else pure []
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let linkedOk =
                            outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        preservedExpandedPwd = expectedMsgEntry `elem` envLog
                        preservedChildPwd = expectedPwdEntry `elem` envLog
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && preservedExpandedPwd
                                && preservedChildPwd
                                && linkedOk
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "envLog:"
                            , T.unlines envLog
                            , "expectedMsgEntry: " <> expectedMsgEntry
                            , "expectedPwdEntry: " <> expectedPwdEntry
                            , "preservedExpandedPwd: " <> T.pack (show preservedExpandedPwd)
                            , "preservedChildPwd: " <> T.pack (show preservedChildPwd)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmLeadingEnvAssignmentPreservesInheritedPwdMsg ok details

runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralTest :: IO (Either T.Text T.Text, String)
runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralTest =
    if os == "mingw32"
        then pure (Right runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , fakeAssemblerPath
                    , fakeAssemblerLogPath
                    , fakeAssemblerAsmPath
                    , fakeEnvLoggingWrapperPath
                    , fakeEnvLoggingWrapperLogPath
                    ]
                ) $ do
                    htccCmd <- htccCommand
                    writeFakeAssembler fakeAssemblerPath
                    writeEnvLoggingForwardingWrapper
                        fakeEnvLoggingWrapperPath
                        fakeEnvLoggingWrapperLogPath
                        ["MSG"]
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HTCC_ASSEMBLER='MSG=\\$PWD ./"
                        , T.pack fakeEnvLoggingWrapperPath
                        , " "
                        , T.pack fakeAssemblerPath
                        , "' "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    envLog <- do
                        logExists <- doesFileExist fakeEnvLoggingWrapperLogPath
                        if logExists
                            then T.lines <$> T.readFile fakeEnvLoggingWrapperLogPath
                            else pure []
                    outputExists <- doesFileExist "tmp"
                    ranOk <- if outputExists
                        then exitCode (const False) True <$> exec "./tmp"
                        else pure False
                    let linkedOk =
                            outputExists
                                && (not hostCanExecuteGeneratedElf || ranOk)
                        preservedLiteral = "MSG=$PWD" `elem` envLog
                        expandedWorkingDirectory =
                            any (\line -> "MSG=" `T.isPrefixOf` line && line /= "MSG=$PWD") envLog
                        ok =
                            exitCode (const False) True result
                                && T.null stdoutLeak
                                && T.null stderrOut
                                && preservedLiteral
                                && not expandedWorkingDirectory
                                && linkedOk
                        details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "envLog:"
                            , T.unlines envLog
                            , "preservedLiteral: " <> T.pack (show preservedLiteral)
                            , "expandedWorkingDirectory: " <> T.pack (show expandedWorkingDirectory)
                            , "outputExists: " <> T.pack (show outputExists)
                            , "hostCanExecuteGeneratedElf: " <> T.pack (show hostCanExecuteGeneratedElf)
                            , "ranOk: " <> T.pack (show ranOk)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmLeadingEnvAssignmentPreservesEscapedPosixLiteralMsg ok details

runAsmEnvPathOverrideEmptyEntryTest :: IO (Either T.Text T.Text, String)
runAsmEnvPathOverrideEmptyEntryTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakeAssemblerLogPath
            , fakeAssemblerAsmPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssembler fakeAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='PATH= "
                , T.pack fakeAssemblerPath
                , "' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            compilerArgs <- do
                logExists <- doesFileExist fakeAssemblerLogPath
                if logExists
                    then T.lines <$> T.readFile fakeAssemblerLogPath
                    else pure []
            asm <- do
                asmExists <- doesFileExist fakeAssemblerAsmPath
                if asmExists
                    then T.readFile fakeAssemblerAsmPath
                    else pure ""
            outputExists <- doesFileExist "tmp"
            ranOk <- if outputExists
                then exitCode (const False) True <$> exec "./tmp"
                else pure False
            let compilerSawExpectedArgs =
                    all (`elem` compilerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                envAssignmentPassedAsArg =
                    any ("PATH=" `T.isPrefixOf`) compilerArgs
                hasRequiredAsm =
                    all (`T.isInfixOf` asm)
                        [ ".intel_syntax noprefix"
                        , ".L.return.main:"
                        , ".L.label.main.done:"
                        ]
                ok =
                    T.null stdoutLeak
                        && T.null stderrOut
                        && compilerSawExpectedArgs
                        && not envAssignmentPassedAsArg
                        && hasRequiredAsm
                        && ranOk
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "compilerArgs:"
                    , T.unlines compilerArgs
                    , "envAssignmentPassedAsArg: " <> T.pack (show envAssignmentPassedAsArg)
                    , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmEnvPathOverrideEmptyEntryMsg ok details

runAsmEnvPathOverrideNoLocalFallbackTest :: IO (Either T.Text T.Text, String)
runAsmEnvPathOverrideNoLocalFallbackTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeAssemblerPath
            , fakePathBinDir
            , fakeLocalAssemblerLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            createDirectoryIfMissing False fakePathBinDir
            writeFailingCompiler fakeAssemblerPath fakeLocalAssemblerLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER='PATH=./"
                , T.pack fakePathBinDir
                , " tmp-assembler.sh' "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            localAssemblerWasInvoked <- doesFileExist fakeLocalAssemblerLogPath
            outputExists <- doesFileExist "tmp"
            let failedAsExpected = exitCode (const True) False result
                hasExpectedError =
                    "failed to determine an x86_64-ELF target from HTCC_ASSEMBLER"
                        `T.isInfixOf` stderrOut
                ok =
                    T.null stdoutLeak
                        && failedAsExpected
                        && hasExpectedError
                        && not localAssemblerWasInvoked
                        && not outputExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "localAssemblerWasInvoked: " <> T.pack (show localAssemblerWasInvoked)
                    , "outputExists: " <> T.pack (show outputExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmEnvPathOverrideNoLocalFallbackMsg ok details

runAsmQuotedBackslashArgTest :: IO (Either T.Text T.Text, String)
runAsmQuotedBackslashArgTest = flip finally (clean ["tmp", "tmp.out", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
    htccCmd <- htccCommand
    writeFakeAssembler fakeAssemblerPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER='./tmp-assembler.sh \""
        , fakeAssemblerBackslashArg
        , "\"' "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
    asm <- T.readFile fakeAssemblerAsmPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ fakeAssemblerBackslashArg
                , "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult runAsmQuotedBackslashArgMsg ok details

runAsmIgnoresCcTest :: IO (Either T.Text T.Text, String)
runAsmIgnoresCcTest = flip finally (clean ["tmp", "tmp.out", fakeGccPath, fakeBadCcPath, fakeBadCcLogPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
    htccCmd <- htccCommand
    writeFakeAssembler fakeGccPath
    writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "PATH=.:$PATH "
        , "CC='./tmp-bad-cc.sh "
        , fakeAssemblerWrapperArg
        , "' "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakeAssemblerLogPath
    asm <- T.readFile fakeAssemblerAsmPath
    ccWasInvoked <- doesFileExist fakeBadCcLogPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk && not ccWasInvoked
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "ccWasInvoked: " <> T.pack (show ccWasInvoked)
            , "exitCode: " <> T.pack (show result)
            ]
    return $ mkResult runAsmIgnoresCcMsg ok details

runAsmGccPrefersPathTest :: IO (Either T.Text T.Text, String)
runAsmGccPrefersPathTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , fakeGccPath
            , fakePathBinDir
            , fakePathGccLogPath
            , fakePathGccAsmPath
            , fakeLocalGccLogPath
            ]
        ) $ do
            (ok, details) <- runAsmGccPrefersPathScenario
            return $ mkResult runAsmGccPrefersPathMsg ok details

runAsmGccPrefersPathScenario :: IO (Bool, T.Text)
runAsmGccPrefersPathScenario = do
    htccCmd <- htccCommand
    clean ["tmp", "tmp.out", fakeGccPath, fakePathBinDir, fakePathGccLogPath, fakePathGccAsmPath, fakeLocalGccLogPath]
    createDirectoryIfMissing False fakePathBinDir
    writeFakeAssemblerWithLogs fakePathGccLogPath fakePathGccAsmPath fakePathGccPath
    writeFailingCompiler fakeGccPath fakeLocalGccLogPath
    execErrFin $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "PATH=./"
        , T.pack fakePathBinDir
        , ":$PATH "
        , htccCmd
        , " -r -o tmp /dev/stdin > tmp.out"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    compilerArgs <- T.lines <$> T.readFile fakePathGccLogPath
    asm <- T.readFile fakePathGccAsmPath
    localGccWasInvoked <- doesFileExist fakeLocalGccLogPath
    result <- exec "./tmp"
    let compilerSawExpectedArgs =
            all (`elem` compilerArgs)
                [ "-x"
                , "assembler"
                , "-c"
                , "-o"
                ]
        hasRequiredAsm =
            all (`T.isInfixOf` asm)
                [ ".intel_syntax noprefix"
                , ".L.return.main:"
                , ".L.label.main.done:"
                ]
        ranOk = exitCode (const False) True result
        ok = T.null stdoutLeak && compilerSawExpectedArgs && hasRequiredAsm && ranOk && not localGccWasInvoked
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
            , "localGccWasInvoked: " <> T.pack (show localGccWasInvoked)
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmFailurePreservesExistingOutputTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "a.out", fakeBadCcPath, fakeBadCcLogPath]) $ do
            htccCmd <- htccCommand
            writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
            defaultResult <- runAsmFailurePreservesExistingOutputScenario htccCmd "" "a.out"
            explicitResult <- runAsmFailurePreservesExistingOutputScenario htccCmd " -o tmp" "tmp"
            let ok = resultOk defaultResult && resultOk explicitResult
                details = T.unlines
                    [ "[default]"
                    , resultDetails defaultResult
                    , "[explicit]"
                    , resultDetails explicitResult
                    ]
            return $ mkResult runAsmFailurePreservesExistingOutputMsg ok details
    where
        resultOk (x, _) = x
        resultDetails (_, x) = x

runAsmFailurePreservesExistingOutputScenario :: T.Text -> T.Text -> FilePath -> IO (Bool, T.Text)
runAsmFailurePreservesExistingOutputScenario htccCmd outputArgs target = do
    clean [target, "tmp.out", "tmp.err", fakeBadCcLogPath]
    T.writeFile target "stale output"
    result <- exec $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER=./"
        , T.pack fakeBadCcPath
        , " "
        , htccCmd
        , " -r"
        , outputArgs
        , " /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    targetExists <- doesFileExist target
    targetContents <- if targetExists then T.readFile target else pure ""
    compilerArgs <- do
        hasLog <- doesFileExist fakeBadCcLogPath
        if hasLog
            then T.lines <$> T.readFile fakeBadCcLogPath
            else pure []
    let failed = exitCode (const True) False result
        compilerInvoked = not $ null compilerArgs
        compilerSawAssembleFlags = all (`elem` compilerArgs) ["-x", "assembler", "-c", "-o"]
        ok =
            failed
                && T.null stdoutLeak
                && targetExists
                && targetContents == "stale output"
                && compilerInvoked
                && compilerSawAssembleFlags
        details = T.unlines
            [ "target: " <> T.pack target
            , "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "compilerInvoked: " <> T.pack (show compilerInvoked)
            , "compilerSawAssembleFlags: " <> T.pack (show compilerSawAssembleFlags)
            , "targetExists: " <> T.pack (show targetExists)
            , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmParseFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmParseFailurePreservesExistingOutputTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "a.out", "tmp-parse-error.c", fakeBadCcPath, fakeBadCcLogPath]) $ do
            htccCmd <- htccCommand
            writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
            defaultResult <- runAsmParseFailurePreservesExistingOutputScenario htccCmd "" "a.out"
            explicitResult <- runAsmParseFailurePreservesExistingOutputScenario htccCmd " -o tmp" "tmp"
            let ok = resultOk defaultResult && resultOk explicitResult
                details = T.unlines
                    [ "[default]"
                    , resultDetails defaultResult
                    , "[explicit]"
                    , resultDetails explicitResult
                    ]
            return $ mkResult runAsmParseFailurePreservesExistingOutputMsg ok details
    where
        resultOk (x, _) = x
        resultDetails (_, x) = x

runAsmParseFailurePreservesExistingOutputScenario :: T.Text -> T.Text -> FilePath -> IO (Bool, T.Text)
runAsmParseFailurePreservesExistingOutputScenario htccCmd outputArgs target = do
    let inputPath = "tmp-parse-error.c"
    clean [target, inputPath, "tmp.out", "tmp.err", fakeBadCcLogPath]
    T.writeFile target "stale output"
    T.writeFile inputPath parseFailureSource
    result <- exec $ mconcat
        [ "HTCC_ASSEMBLER=./"
        , T.pack fakeBadCcPath
        , " "
        , htccCmd
        , " -r"
        , outputArgs
        , " "
        , T.pack inputPath
        , " > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    inputContents <- T.readFile inputPath
    targetExists <- doesFileExist target
    targetContents <- if targetExists then T.readFile target else pure ""
    compilerInvoked <- doesFileExist fakeBadCcLogPath
    let failed = exitCode (const True) False result
        mentionsInput = T.pack inputPath `T.isInfixOf` stderrOut
        ok =
            failed
                && T.null stdoutLeak
                && targetExists
                && targetContents == "stale output"
                && inputContents == parseFailureSource
                && not compilerInvoked
                && mentionsInput
        details = T.unlines
            [ "target: " <> T.pack target
            , "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "targetExists: " <> T.pack (show targetExists)
            , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
            , "inputUnchanged: " <> T.pack (show (inputContents == parseFailureSource))
            , "compilerInvoked: " <> T.pack (show compilerInvoked)
            , "mentionsInput: " <> T.pack (show mentionsInput)
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmReadFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmReadFailurePreservesExistingOutputTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "a.out", "tmp-missing.c", fakeBadCcPath, fakeBadCcLogPath]) $ do
            htccCmd <- htccCommand
            writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
            defaultResult <- runAsmReadFailurePreservesExistingOutputScenario htccCmd "" "a.out"
            explicitResult <- runAsmReadFailurePreservesExistingOutputScenario htccCmd " -o tmp" "tmp"
            let ok = resultOk defaultResult && resultOk explicitResult
                details = T.unlines
                    [ "[default]"
                    , resultDetails defaultResult
                    , "[explicit]"
                    , resultDetails explicitResult
                    ]
            return $ mkResult runAsmReadFailurePreservesExistingOutputMsg ok details
    where
        resultOk (x, _) = x
        resultDetails (_, x) = x

runAsmReadFailurePreservesExistingOutputScenario :: T.Text -> T.Text -> FilePath -> IO (Bool, T.Text)
runAsmReadFailurePreservesExistingOutputScenario htccCmd outputArgs target = do
    let inputPath = "tmp-missing.c"
    clean [target, inputPath, "tmp.out", "tmp.err", fakeBadCcLogPath]
    T.writeFile target "stale output"
    result <- exec $ mconcat
        [ "HTCC_ASSEMBLER=./"
        , T.pack fakeBadCcPath
        , " "
        , htccCmd
        , " -r"
        , outputArgs
        , " "
        , T.pack inputPath
        , " > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    inputExists <- doesFileExist inputPath
    targetExists <- doesFileExist target
    targetContents <- if targetExists then T.readFile target else pure ""
    compilerInvoked <- doesFileExist fakeBadCcLogPath
    let failed = exitCode (const True) False result
        mentionsInput = T.pack inputPath `T.isInfixOf` stderrOut
        ok =
            failed
                && T.null stdoutLeak
                && targetExists
                && targetContents == "stale output"
                && not inputExists
                && not compilerInvoked
                && mentionsInput
        details = T.unlines
            [ "target: " <> T.pack target
            , "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "targetExists: " <> T.pack (show targetExists)
            , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
            , "inputExists: " <> T.pack (show inputExists)
            , "compilerInvoked: " <> T.pack (show compilerInvoked)
            , "mentionsInput: " <> T.pack (show mentionsInput)
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmHardLinkedRenameReplacementPreservesAliasTest :: IO (Either T.Text T.Text, String)
runAsmHardLinkedRenameReplacementPreservesAliasTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-hard-link-output.out", "tmp-hard-link-output-link.out", fakeAssemblerPath, fakeAssemblerLogPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp-hard-link-output.out"
            alias = "tmp-hard-link-output-link.out"
            staleTarget = "#!/bin/sh\nexit 99\n"
        clean [target, alias, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]
        writeLoggingDriver fakeAssemblerPath fakeAssemblerLogPath
        T.writeFile target staleTarget
        execErrFin $ "ln '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        setFileMode target $ ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeAssemblerPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        aliasExists <- doesFileExist alias
        driverInvocations <- do
            hasLog <- doesFileExist fakeAssemblerLogPath
            if hasLog
                then T.lines <$> T.readFile fakeAssemblerLogPath
                else pure []
        targetContents <- if targetExists then T.readFile target else pure ""
        aliasContents <- if aliasExists then T.readFile alias else pure ""
        targetRunResult <- if targetExists then Just <$> exec ("./" <> T.pack target) else pure Nothing
        aliasRunResult <- if aliasExists then Just <$> exec ("./" <> T.pack alias) else pure Nothing
        let succeeded = exitCode (const False) True result
            targetUpdated = targetContents /= staleTarget
            aliasPreserved = aliasContents == staleTarget
            targetRuns = maybe False (exitCode (== 0) False) targetRunResult
            aliasStillRuns = maybe False (exitCode (== 99) False) aliasRunResult
            driverInvoked = not $ null driverInvocations
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && aliasExists
                    && targetUpdated
                    && aliasPreserved
                    && targetRuns
                    && aliasStillRuns
                    && driverInvoked
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "targetExists: " <> T.pack (show targetExists)
                , "aliasExists: " <> T.pack (show aliasExists)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "aliasPreserved: " <> T.pack (show aliasPreserved)
                , "targetRunExitCode: " <> maybe "not-run" (T.pack . show) targetRunResult
                , "aliasRunExitCode: " <> maybe "not-run" (T.pack . show) aliasRunResult
                , "driverInvoked: " <> T.pack (show driverInvoked)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmHardLinkedRenameReplacementPreservesAliasMsg ok details

runAsmReadOnlyParentWritableTargetTest :: IO (Either T.Text T.Text, String)
runAsmReadOnlyParentWritableTargetTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.out"
            targetMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , groupReadMode
                , otherReadMode
                ]
        clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]
        writeLoggingDriver fakeAssemblerPath fakeAssemblerLogPath
        createDirectoryIfMissing False targetDir
        T.writeFile target "#!/bin/sh\nexit 99\n"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeAssemblerPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetContents <- T.readFile target
        runResult <- exec $ "./" <> T.pack target
        let succeeded = exitCode (const False) True result
            ranOk = exitCode (const False) True runResult
            targetUpdated =
                all (`T.isInfixOf` targetContents)
                    [ "#!/bin/sh"
                    , "exit 0"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetUpdated
                    && ranOk
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "runExitCode: " <> T.pack (show runResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmReadOnlyParentWritableTargetMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]

runAsmReadOnlyParentWriteOnlyTargetTest :: IO (Either T.Text T.Text, String)
runAsmReadOnlyParentWriteOnlyTargetTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-write-only.out"
            targetMode = ownerWriteMode
            expectedMode = ownerWriteMode `unionFileModes` ownerExecuteMode
        clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]
        writeLoggingDriver fakeAssemblerPath fakeAssemblerLogPath
        createDirectoryIfMissing False targetDir
        T.writeFile target "#!/bin/sh\nexit 99\n"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeAssemblerPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        execErrFin $ "chmod 755 '" <> T.pack targetDir <> "'"
        when targetExists $
            setFileMode target $ replacedMode `unionFileModes` ownerReadMode `unionFileModes` ownerExecuteMode
        targetContents <- if targetExists then T.readFile target else pure ""
        runResult <- if targetExists then Just <$> exec ("./" <> T.pack target) else pure Nothing
        let replacedPermissions = permissionBits replacedMode
        let succeeded = exitCode (const False) True result
            ranOk = maybe False (exitCode (const False) True) runResult
            targetUpdated =
                all (`T.isInfixOf` targetContents)
                    [ "#!/bin/sh"
                    , "exit 0"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && replacedPermissions == expectedMode
                    && targetUpdated
                    && ranOk
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmReadOnlyParentWriteOnlyTargetMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]

runAsmReadOnlyParentExecutableOnlyTargetTest :: IO (Either T.Text T.Text, String)
runAsmReadOnlyParentExecutableOnlyTargetTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-exec-only.out"
            targetMode = 0o555
        clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]
        writeLoggingDriver fakeAssemblerPath fakeAssemblerLogPath
        createDirectoryIfMissing False targetDir
        T.writeFile target "#!/bin/sh\nexit 99\n"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeAssemblerPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        targetContents <- if targetExists then T.readFile target else pure ""
        runResult <- if targetExists then Just <$> exec ("./" <> T.pack target) else pure Nothing
        let replacedPermissions = permissionBits replacedMode
        let succeeded = exitCode (const False) True result
            ranOk = maybe False (exitCode (const False) True) runResult
            targetUpdated =
                all (`T.isInfixOf` targetContents)
                    [ "#!/bin/sh"
                    , "exit 0"
                    ]
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && replacedPermissions == targetMode
                    && targetUpdated
                    && ranOk
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "targetUpdated: " <> T.pack (show targetUpdated)
                , "runExitCode: " <> maybe "not-run" (T.pack . show) runResult
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmReadOnlyParentExecutableOnlyTargetMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]

runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.out"
            alias = targetDir </> "tmp-read-only-link.out"
            expectedError = "hard-linked output"
            staleTarget = "#!/bin/sh\nexit 99\n"
        clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]
        writeLoggingDriver fakeAssemblerPath fakeAssemblerLogPath
        createDirectoryIfMissing False targetDir
        T.writeFile target staleTarget
        execErrFin $ "ln '" <> T.pack target <> "' '" <> T.pack alias <> "'"
        setFileMode target $ ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeAssemblerPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        aliasExists <- doesFileExist alias
        driverInvocations <- do
            hasLog <- doesFileExist fakeAssemblerLogPath
            if hasLog
                then T.lines <$> T.readFile fakeAssemblerLogPath
                else pure []
        execErrFin $ "chmod 755 '" <> T.pack targetDir <> "'"
        targetContents <- if targetExists then T.readFile target else pure ""
        aliasContents <- if aliasExists then T.readFile alias else pure ""
        targetRunResult <- if targetExists then Just <$> exec ("./" <> T.pack target) else pure Nothing
        aliasRunResult <- if aliasExists then Just <$> exec ("./" <> T.pack alias) else pure Nothing
        let failed = exitCode (const True) False result
            hasExpectedError = expectedError `T.isInfixOf` stderrOut
            preservedTarget = targetContents == staleTarget
            preservedAlias = aliasContents == staleTarget
            targetStillRuns = maybe False (exitCode (== 99) False) targetRunResult
            aliasStillRuns = maybe False (exitCode (== 99) False) aliasRunResult
            driverInvoked = not $ null driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && targetExists
                    && aliasExists
                    && preservedTarget
                    && preservedAlias
                    && targetStillRuns
                    && aliasStillRuns
                    && driverInvoked
            details = T.unlines
                [ "target: " <> T.pack target
                , "alias: " <> T.pack alias
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "targetExists: " <> T.pack (show targetExists)
                , "aliasExists: " <> T.pack (show aliasExists)
                , "preservedTarget: " <> T.pack (show preservedTarget)
                , "preservedAlias: " <> T.pack (show preservedAlias)
                , "targetRunExitCode: " <> maybe "not-run" (T.pack . show) targetRunResult
                , "aliasRunExitCode: " <> maybe "not-run" (T.pack . show) aliasRunResult
                , "driverInvoked: " <> T.pack (show driverInvoked)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmReadOnlyParentHardLinkAliasPreservesExistingOutputMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath]

runAsmReadOnlyParentLinkFailurePreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmReadOnlyParentLinkFailurePreservesExistingOutputTest =
    flip finally cleanupReadOnlyOutputDir $ do
        htccCmd <- htccCommand
        let targetDir = "tmp-read-only-dir"
            target = targetDir </> "tmp-read-only.out"
            targetMode = foldr1 unionFileModes
                [ ownerReadMode
                , ownerWriteMode
                , ownerExecuteMode
                , groupReadMode
                , otherReadMode
                ]
        clean [targetDir, "tmp.out", "tmp.err", fakeFailingLinkDriverPath, fakeFailingLinkDriverLogPath]
        writeFailingLinkDriver fakeFailingLinkDriverPath fakeFailingLinkDriverLogPath
        createDirectoryIfMissing False targetDir
        T.writeFile target "#!/bin/sh\nexit 99\n"
        setFileMode target targetMode
        execErrFin $ "chmod 555 '" <> T.pack targetDir <> "'"
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | HTCC_ASSEMBLER=./"
            , T.pack fakeFailingLinkDriverPath
            , " "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        targetContents <- if targetExists then T.readFile target else pure ""
        programResult <- if targetExists then Just <$> exec ("./" <> T.pack target) else pure Nothing
        driverInvocations <- do
            hasLog <- doesFileExist fakeFailingLinkDriverLogPath
            if hasLog
                then T.lines <$> T.readFile fakeFailingLinkDriverLogPath
                else pure []
        let failed = exitCode (const True) False result
            attemptedStagedLink =
                any (\line -> "tmp-read-only.out" `T.isInfixOf` line && ".htcc-" `T.isInfixOf` line) driverInvocations
            preservedTarget = targetContents == "#!/bin/sh\nexit 99\n"
            staleTargetRan = maybe False (exitCode (== 99) False) programResult
            ok =
                failed
                    && T.null stdoutLeak
                    && targetExists
                    && preservedTarget
                    && staleTargetRan
                    && attemptedStagedLink
            details = T.unlines
                [ "target: " <> T.pack target
                , "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "targetExists: " <> T.pack (show targetExists)
                , "preservedTarget: " <> T.pack (show preservedTarget)
                , "staleTargetRan: " <> T.pack (show staleTargetRan)
                , "attemptedStagedLink: " <> T.pack (show attemptedStagedLink)
                , "runExitCode: " <> T.pack (show programResult)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmReadOnlyParentLinkFailurePreservesExistingOutputMsg ok details
    where
        cleanupReadOnlyOutputDir = do
            let targetDir = "tmp-read-only-dir"
            _ <- exec $ "chmod 755 '" <> T.pack targetDir <> "' > /dev/null 2>&1"
            clean [targetDir, "tmp.out", "tmp.err", fakeFailingLinkDriverPath, fakeFailingLinkDriverLogPath]

runAsmFailurePreservesInputOutputAliasTest :: IO (Either T.Text T.Text, String)
runAsmFailurePreservesInputOutputAliasTest =
    flip finally
        (clean ["tmp.out", "tmp.err", "a.out", "tmp-source.c", fakeBadCcPath, fakeBadCcLogPath]) $ do
            htccCmd <- htccCommand
            writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
            defaultResult <- runAsmFailurePreservesInputOutputAliasScenario htccCmd "" "a.out"
            explicitResult <- runAsmFailurePreservesInputOutputAliasScenario htccCmd " -o tmp-source.c" "tmp-source.c"
            let ok = resultOk defaultResult && resultOk explicitResult
                details = T.unlines
                    [ "[default]"
                    , resultDetails defaultResult
                    , "[explicit]"
                    , resultDetails explicitResult
                    ]
            return $ mkResult runAsmFailurePreservesInputOutputAliasMsg ok details
    where
        resultOk (x, _) = x
        resultDetails (_, x) = x

runAsmFailurePreservesInputOutputAliasScenario :: T.Text -> T.Text -> FilePath -> IO (Bool, T.Text)
runAsmFailurePreservesInputOutputAliasScenario htccCmd outputArgs inputPath = do
    clean [inputPath, "tmp.out", "tmp.err", fakeBadCcLogPath]
    T.writeFile inputPath source
    result <- exec $ mconcat
        [ "HTCC_ASSEMBLER=./"
        , T.pack fakeBadCcPath
        , " "
        , htccCmd
        , " -r"
        , outputArgs
        , " "
        , T.pack inputPath
        , " > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    inputExists <- doesFileExist inputPath
    inputContents <- if inputExists then T.readFile inputPath else pure ""
    compilerArgs <- do
        hasLog <- doesFileExist fakeBadCcLogPath
        if hasLog
            then T.lines <$> T.readFile fakeBadCcLogPath
            else pure []
    let failed = exitCode (const True) False result
        compilerInvoked = not $ null compilerArgs
        hasAliasError = "-r output path must not overwrite an input file" `T.isInfixOf` stderrOut
        ok = failed && T.null stdoutLeak && inputExists && inputContents == source && not compilerInvoked && hasAliasError
        details = T.unlines
            [ "inputPath: " <> T.pack inputPath
            , "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "inputExists: " <> T.pack (show inputExists)
            , "inputUnchanged: " <> T.pack (show (inputContents == source))
            , "compilerInvoked: " <> T.pack (show compilerInvoked)
            , "hasAliasError: " <> T.pack (show hasAliasError)
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmMissingInputMatchingOutputPathTest :: IO (Either T.Text T.Text, String)
runAsmMissingInputMatchingOutputPathTest =
    flip finally (clean ["tmp.out", "tmp.err", "tmp-missing"]) $ do
        htccCmd <- htccCommand
        result <- exec $ mconcat
            [ htccCmd
            , " -r -o tmp-missing tmp-missing > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp-missing"
        let failed = exitCode (const True) False result
            hasAliasError = "-r output path must not overwrite an input file" `T.isInfixOf` stderrOut
            mentionsInputPath = "tmp-missing" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && not targetExists
                    && mentionsInputPath
                    && not hasAliasError
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "mentionsInputPath: " <> T.pack (show mentionsInputPath)
                , "hasAliasError: " <> T.pack (show hasAliasError)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmMissingInputMatchingOutputPathMsg ok details

runAsmFailurePreservesHardLinkInputOutputAliasTest :: IO (Either T.Text T.Text, String)
runAsmFailurePreservesHardLinkInputOutputAliasTest =
    flip finally
        (clean ["tmp.out", "tmp.err", "tmp-source.c", "tmp-source-link.c", fakeBadCcPath, fakeBadCcLogPath]) $ do
            htccCmd <- htccCommand
            writeFailingCompiler fakeBadCcPath fakeBadCcLogPath
            (ok, details) <- runAsmFailurePreservesHardLinkInputOutputAliasScenario htccCmd
            return $ mkResult runAsmFailurePreservesHardLinkInputOutputAliasMsg ok details

runAsmFailurePreservesHardLinkInputOutputAliasScenario :: T.Text -> IO (Bool, T.Text)
runAsmFailurePreservesHardLinkInputOutputAliasScenario htccCmd = do
    clean ["tmp-source.c", "tmp-source-link.c", "tmp.out", "tmp.err", fakeBadCcLogPath]
    T.writeFile "tmp-source.c" source
    execErrFin "ln tmp-source.c tmp-source-link.c"
    result <- exec $ mconcat
        [ "HTCC_ASSEMBLER=./"
        , T.pack fakeBadCcPath
        , " "
        , htccCmd
        , " -r -o tmp-source-link.c tmp-source.c > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    inputExists <- doesFileExist "tmp-source.c"
    outputExists <- doesFileExist "tmp-source-link.c"
    inputContents <- if inputExists then T.readFile "tmp-source.c" else pure ""
    outputContents <- if outputExists then T.readFile "tmp-source-link.c" else pure ""
    compilerArgs <- do
        hasLog <- doesFileExist fakeBadCcLogPath
        if hasLog
            then T.lines <$> T.readFile fakeBadCcLogPath
            else pure []
    let failed = exitCode (const True) False result
        compilerInvoked = not $ null compilerArgs
        hasAliasError = "-r output path must not overwrite an input file" `T.isInfixOf` stderrOut
        ok =
            failed
                && T.null stdoutLeak
                && inputExists
                && outputExists
                && inputContents == source
                && outputContents == source
                && not compilerInvoked
                && hasAliasError
        details = T.unlines
            [ "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "compilerArgs:"
            , T.unlines compilerArgs
            , "inputExists: " <> T.pack (show inputExists)
            , "outputExists: " <> T.pack (show outputExists)
            , "inputUnchanged: " <> T.pack (show (inputContents == source))
            , "outputUnchanged: " <> T.pack (show (outputContents == source))
            , "compilerInvoked: " <> T.pack (show compilerInvoked)
            , "hasAliasError: " <> T.pack (show hasAliasError)
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmMalformedAssemblerPreservesExistingOutputTest :: IO (Either T.Text T.Text, String)
runAsmMalformedAssemblerPreservesExistingOutputTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", "a.out"]) $ do
            htccCmd <- htccCommand
            defaultResult <- runAsmMalformedAssemblerPreservesExistingOutputScenario htccCmd "" "a.out"
            explicitResult <- runAsmMalformedAssemblerPreservesExistingOutputScenario htccCmd " -o tmp" "tmp"
            let ok = resultOk defaultResult && resultOk explicitResult
                details = T.unlines
                    [ "[default]"
                    , resultDetails defaultResult
                    , "[explicit]"
                    , resultDetails explicitResult
                    ]
            return $ mkResult runAsmMalformedAssemblerPreservesExistingOutputMsg ok details
    where
        resultOk (x, _) = x
        resultDetails (_, x) = x

runAsmMalformedAssemblerPreservesExistingOutputScenario :: T.Text -> T.Text -> FilePath -> IO (Bool, T.Text)
runAsmMalformedAssemblerPreservesExistingOutputScenario htccCmd outputArgs target = do
    clean [target, "tmp.out", "tmp.err"]
    T.writeFile target "stale output"
    result <- exec $ mconcat
        [ "echo '"
        , source
        , "' | "
        , "HTCC_ASSEMBLER=\"'\" "
        , htccCmd
        , " -r"
        , outputArgs
        , " /dev/stdin > tmp.out 2> tmp.err"
        ]
    stdoutLeak <- T.readFile "tmp.out"
    stderrOut <- T.readFile "tmp.err"
    targetExists <- doesFileExist target
    targetContents <- if targetExists then T.readFile target else pure ""
    let failed = exitCode (const True) False result
        ok = failed && T.null stdoutLeak && targetExists && targetContents == "stale output"
        details = T.unlines
            [ "target: " <> T.pack target
            , "stdout:"
            , stdoutLeak
            , "stderr:"
            , stderrOut
            , "targetExists: " <> T.pack (show targetExists)
            , "targetUnchanged: " <> T.pack (show (targetContents == "stale output"))
            , "exitCode: " <> T.pack (show result)
            ]
    pure (ok, details)

runAsmMalformedAssemblerTest :: IO (Either T.Text T.Text, String)
runAsmMalformedAssemblerTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeMalformedAssemblerTmpDir]) $ do
        htccCmd <- htccCommand
        createDirectoryIfMissing False fakeMalformedAssemblerTmpDir
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "TMPDIR=./"
            , T.pack fakeMalformedAssemblerTmpDir
            , " "
            , "HTCC_ASSEMBLER=\"'\" "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        tmpFiles <- listDirectory fakeMalformedAssemblerTmpDir
        let failed = exitCode (const True) False result
            ok = failed && T.null stdoutLeak && null tmpFiles
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "tmpFiles:"
                , T.pack (show tmpFiles)
                , "exitCode: " <> T.pack (show result)
                ]
        pure $ mkResult runAsmMalformedAssemblerMsg ok details

runAsmAcceptsFreeBsdElfTargetDriverTest :: IO (Either T.Text T.Text, String)
runAsmAcceptsFreeBsdElfTargetDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeFreeBsdDriverPath, fakeFreeBsdDriverLogPath, fakeFreeBsdDriverAsmPath]) $ do
        htccCmd <- htccCommand
        writeFakeAssemblerWithTarget "x86_64-unknown-freebsd13.2" fakeFreeBsdDriverLogPath fakeFreeBsdDriverAsmPath fakeFreeBsdDriverPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeFreeBsdDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        compilerArgs <- T.lines <$> T.readFile fakeFreeBsdDriverLogPath
        asm <- T.readFile fakeFreeBsdDriverAsmPath
        programResult <- exec "./tmp"
        let compilerSawExpectedArgs =
                all (`elem` compilerArgs)
                    [ "-x"
                    , "assembler"
                    , "-c"
                    , "-o"
                    ]
            hasRequiredAsm =
                all (`T.isInfixOf` asm)
                    [ ".intel_syntax noprefix"
                    , ".L.return.main:"
                    , ".L.label.main.done:"
                    ]
            succeeded = exitCode (const False) True result
            ranOk = exitCode (const False) True programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && compilerSawExpectedArgs
                    && hasRequiredAsm
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "compilerArgs:"
                , T.unlines compilerArgs
                , "hasRequiredAsm: " <> T.pack (show hasRequiredAsm)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> T.pack (show programResult)
                ]
        return $ mkResult runAsmAcceptsFreeBsdElfTargetDriverMsg ok details

runAsmRejectsMissingAssemblerDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsMissingAssemblerDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeMissingAssemblerDriverPath]) $ do
        htccCmd <- htccCommand
        clean [fakeMissingAssemblerDriverPath]
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER="
            , T.pack fakeMissingAssemblerDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed to start HTCC_ASSEMBLER probe" `T.isInfixOf` stderrOut
                    && T.pack fakeMissingAssemblerDriverPath `T.isInfixOf` stderrOut
            leakedRawProcessException =
                "readCreateProcessWithExitCode" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not leakedRawProcessException
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "leakedRawProcessException: " <> T.pack (show leakedRawProcessException)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsMissingAssemblerDriverMsg ok details

runAsmRejectsNonExecutableAssemblerDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsNonExecutableAssemblerDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeNonExecutableAssemblerDriverPath]) $ do
        htccCmd <- htccCommand
        T.writeFile fakeNonExecutableAssemblerDriverPath $ T.unlines
            [ "#!/bin/sh"
            , "exit 0"
            ]
        setFileMode fakeNonExecutableAssemblerDriverPath $
            ownerReadMode `unionFileModes` ownerWriteMode
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeNonExecutableAssemblerDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed to start HTCC_ASSEMBLER probe" `T.isInfixOf` stderrOut
                    && T.pack fakeNonExecutableAssemblerDriverPath `T.isInfixOf` stderrOut
            leakedRawProcessException =
                "readCreateProcessWithExitCode" `T.isInfixOf` stderrOut
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not leakedRawProcessException
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "leakedRawProcessException: " <> T.pack (show leakedRawProcessException)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsNonExecutableAssemblerDriverMsg ok details

runAsmRejectsAssemblerWithoutLinkDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsAssemblerWithoutLinkDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeAssembleOnlyDriverPath, fakeAssembleOnlyDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeAssembleOnlyDriver fakeAssembleOnlyDriverPath fakeAssembleOnlyDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeAssembleOnlyDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeAssembleOnlyDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed a link probe for -r" `T.isInfixOf` stderrOut
                    && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && sawLinkProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsAssemblerWithoutLinkDriverMsg ok details

runAsmRejectsSingleObjectOnlyLinkDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsSingleObjectOnlyLinkDriverTest =
    flip finally
        (clean ["tmp", "tmp.out", "tmp.err", fakeSingleObjectOnlyLinkDriverPath, fakeSingleObjectOnlyLinkDriverLogPath])
        $ do
            htccCmd <- htccCommand
            writeSingleObjectOnlyLinkDriver
                fakeSingleObjectOnlyLinkDriverPath
                fakeSingleObjectOnlyLinkDriverLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeSingleObjectOnlyLinkDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeSingleObjectOnlyLinkDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed a link probe for -r" `T.isInfixOf` stderrOut
                        && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe =
                    any
                        (\line ->
                            "-x assembler -c -o" `T.isInfixOf` line
                                && "htcc-probe-" `T.isInfixOf` line
                        )
                        driverInvocations
                sawLinkProbe =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "htcc-probe-" `T.isInfixOf` line
                        )
                        driverInvocations
                sawFinalLink =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "tmp.htcc-" `T.isInfixOf` line
                        )
                        driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && sawLinkProbe
                        && not sawFinalLink
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "sawFinalLink: " <> T.pack (show sawFinalLink)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            pure $ mkResult runAsmRejectsSingleObjectOnlyLinkDriverMsg ok details

runAsmRejectsScriptLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsScriptLinkProbeDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeScriptLinkProbeDriverPath, fakeScriptLinkProbeDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeScriptLinkProbeDriver fakeScriptLinkProbeDriverPath fakeScriptLinkProbeDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeScriptLinkProbeDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeScriptLinkProbeDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed a link probe for -r" `T.isInfixOf` stderrOut
                    && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && sawLinkProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsScriptLinkProbeDriverMsg ok details

runAsmRejectsSharedLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsSharedLinkProbeDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeSharedLinkProbeDriverPath, fakeSharedLinkProbeDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeSharedLinkProbeDriver fakeSharedLinkProbeDriverPath fakeSharedLinkProbeDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeSharedLinkProbeDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeSharedLinkProbeDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed a link probe for -r" `T.isInfixOf` stderrOut
                    && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && sawLinkProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsSharedLinkProbeDriverMsg ok details

runAsmRejectsForeignAbiLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsForeignAbiLinkProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeForeignAbiLinkProbeDriverPath
            , fakeForeignAbiLinkProbeDriverLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeForeignAbiLinkProbeDriver fakeForeignAbiLinkProbeDriverPath fakeForeignAbiLinkProbeDriverLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeForeignAbiLinkProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverLogExists <- doesFileExist fakeForeignAbiLinkProbeDriverLogPath
            driverInvocations <-
                catchIOError
                    (T.lines <$> T.readFile fakeForeignAbiLinkProbeDriverLogPath)
                    (const $ pure [])
            let succeeded = exitCode (const False) True result
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawProbeAssembly =
                    any
                        (\line ->
                            "-x assembler -c -o" `T.isInfixOf` line
                                && "htcc-probe-" `T.isInfixOf` line
                        )
                        driverInvocations
                sawProbeLink =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "htcc-probe-" `T.isInfixOf` line
                        )
                        driverInvocations
                sawFinalLink =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "tmp.htcc-" `T.isInfixOf` line
                        )
                        driverInvocations
                ok =
                    succeeded
                        && T.null stdoutLeak
                        && T.null stderrOut
                        && sawMetadataProbe
                        && sawProbeAssembly
                        && sawProbeLink
                        && sawFinalLink
                        && targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawProbeAssembly: " <> T.pack (show sawProbeAssembly)
                    , "sawProbeLink: " <> T.pack (show sawProbeLink)
                    , "sawFinalLink: " <> T.pack (show sawFinalLink)
                    , "driverLogExists: " <> T.pack (show driverLogExists)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsForeignAbiLinkProbeDriverMsg ok details

runAsmRejectsBlobLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsBlobLinkProbeDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeBlobLinkProbeDriverPath, fakeBlobLinkProbeDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeBlobLinkProbeDriver fakeBlobLinkProbeDriverPath fakeBlobLinkProbeDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeBlobLinkProbeDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeBlobLinkProbeDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed a link probe for -r" `T.isInfixOf` stderrOut
                    && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && sawLinkProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsBlobLinkProbeDriverMsg ok details

runAsmRejectsSymlinkLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsSymlinkLinkProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeSymlinkLinkProbeDriverPath
            , fakeSymlinkLinkProbeDriverLogPath
            , fakeSymlinkLinkProbeTargetPath
            ])
        $ do
            htccCmd <- htccCommand
            writeSymlinkLinkProbeDriver
                fakeSymlinkLinkProbeDriverPath
                fakeSymlinkLinkProbeDriverLogPath
                fakeSymlinkLinkProbeTargetPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeSymlinkLinkProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeSymlinkLinkProbeDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed a link probe for -r" `T.isInfixOf` stderrOut
                        && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
                sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && sawLinkProbe
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsSymlinkLinkProbeDriverMsg ok details

runAsmRejectsSymlinkSpecialLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsSymlinkSpecialLinkProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeSymlinkSpecialLinkProbeDriverPath
            , fakeSymlinkSpecialLinkProbeDriverLogPath
            ])
        $ do
            htccCmd <- htccCommand
            writeSymlinkSpecialLinkProbeDriver
                fakeSymlinkSpecialLinkProbeDriverPath
                fakeSymlinkSpecialLinkProbeDriverLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeSymlinkSpecialLinkProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeSymlinkSpecialLinkProbeDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed a link probe for -r" `T.isInfixOf` stderrOut
                        && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
                sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && sawLinkProbe
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsSymlinkSpecialLinkProbeDriverMsg ok details

runAsmRejectsNonExecutableLinkProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsNonExecutableLinkProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeNonExecutableLinkProbeDriverPath
            , fakeNonExecutableLinkProbeDriverLogPath
            , fakeNonExecutableLinkProbeObjectPath
            , fakeNonExecutableLinkProbeOutputPath
            ])
        $ do
            htccCmd <- htccCommand
            let probeMarker = "htcc-probe-marker:non-executable"
            B.writeFile fakeNonExecutableLinkProbeObjectPath $ linkedElfHeaderForProbe 1
            B.writeFile
                fakeNonExecutableLinkProbeOutputPath
                (linkedElfHeaderForProbe 2 <> asciiBytesForProbe probeMarker)
            setFileMode fakeNonExecutableLinkProbeOutputPath permissionFileModeMask
            writeNonExecutableLinkProbeDriver
                fakeNonExecutableLinkProbeDriverPath
                fakeNonExecutableLinkProbeDriverLogPath
                fakeNonExecutableLinkProbeObjectPath
                fakeNonExecutableLinkProbeOutputPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeNonExecutableLinkProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeNonExecutableLinkProbeDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed a link probe for -r" `T.isInfixOf` stderrOut
                        && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
                sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && sawLinkProbe
                        && not targetExists
            let details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsNonExecutableLinkProbeDriverMsg ok details

runAsmRejectsMarkerlessFinalOutputTest :: IO (Either T.Text T.Text, String)
runAsmRejectsMarkerlessFinalOutputTest =
    if not hostCanExecuteGeneratedElf
        then pure (Right runAsmRejectsMarkerlessFinalOutputMsg, "")
        else
            flip finally
                (clean
                    [ "tmp"
                    , "tmp.out"
                    , "tmp.err"
                    , fakeMarkerStrippingFinalLinkDriverPath
                    , fakeMarkerStrippingFinalLinkDriverLogPath
                    ])
                $ do
                    htccCmd <- htccCommand
                    writeMarkerStrippingFinalLinkDriver
                        fakeMarkerStrippingFinalLinkDriverPath
                        fakeMarkerStrippingFinalLinkDriverLogPath
                    result <- exec $ mconcat
                        [ "echo '"
                        , source
                        , "' | "
                        , "HTCC_ASSEMBLER=./"
                        , T.pack fakeMarkerStrippingFinalLinkDriverPath
                        , " "
                        , htccCmd
                        , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                        ]
                    stdoutLeak <- T.readFile "tmp.out"
                    stderrOut <- T.readFile "tmp.err"
                    targetExists <- doesFileExist "tmp"
                    driverInvocations <- T.lines <$> T.readFile fakeMarkerStrippingFinalLinkDriverLogPath
                    let failed = exitCode (const True) False result
                        hasExpectedError =
                            "non-runnable final output for -r" `T.isInfixOf` stderrOut
                        sawProbeLink =
                            any
                                (\line ->
                                    "-no-pie -o" `T.isInfixOf` line
                                        && "htcc-probe-" `T.isInfixOf` line
                                )
                                driverInvocations
                        sawFinalLink =
                            any
                                (\line ->
                                    "-no-pie -o" `T.isInfixOf` line
                                        && "tmp.htcc-" `T.isInfixOf` line
                                )
                                driverInvocations
                        ok =
                            failed
                                && T.null stdoutLeak
                                && hasExpectedError
                                && not targetExists
                                && sawProbeLink
                                && sawFinalLink
                    let details = T.unlines
                            [ "stdout:"
                            , stdoutLeak
                            , "stderr:"
                            , stderrOut
                            , "driverInvocations:"
                            , T.unlines driverInvocations
                            , "hasExpectedError: " <> T.pack (show hasExpectedError)
                            , "targetExists: " <> T.pack (show targetExists)
                            , "sawProbeLink: " <> T.pack (show sawProbeLink)
                            , "sawFinalLink: " <> T.pack (show sawFinalLink)
                            , "exitCode: " <> T.pack (show result)
                            ]
                    return $ mkResult runAsmRejectsMarkerlessFinalOutputMsg ok details

runAsmRejectsBogusFinalLinkOutputTest :: IO (Either T.Text T.Text, String)
runAsmRejectsBogusFinalLinkOutputTest =
    flip finally
        (clean
            [ "tmp-final-link.out"
            , "tmp.out"
            , "tmp.err"
            , fakeBogusFinalLinkDriverPath
            , fakeBogusFinalLinkDriverLogPath
            , fakeBogusFinalLinkTargetPath
            ])
        $ do
            htccCmd <- htccCommand
            writeBogusFinalLinkDriver
                fakeBogusFinalLinkDriverPath
                fakeBogusFinalLinkDriverLogPath
                fakeBogusFinalLinkTargetPath
            T.writeFile "tmp-final-link.out" "#!/bin/sh\nexit 99\n"
            setFileMode "tmp-final-link.out" permissionFileModeMask
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeBogusFinalLinkDriverPath
                , " "
                , htccCmd
                , " -r -o tmp-final-link.out /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp-final-link.out"
            driverInvocations <- T.lines <$> T.readFile fakeBogusFinalLinkDriverLogPath
            targetContents <- if targetExists then T.readFile "tmp-final-link.out" else pure ""
            targetRunResult <- if targetExists then Just <$> exec "./tmp-final-link.out" else pure Nothing
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "non-runnable final output for -r" `T.isInfixOf` stderrOut
                sawProbeLink = any ("-no-pie -o" `T.isInfixOf`) $
                    filter ("htcc-probe-" `T.isInfixOf`) driverInvocations
                sawFinalLink =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "tmp-final-link.out.htcc-" `T.isInfixOf` line
                        )
                        driverInvocations
                preservedTarget = targetContents == "#!/bin/sh\nexit 99\n"
                targetStillRuns = maybe False (exitCode (== 99) False) targetRunResult
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && targetExists
                        && preservedTarget
                        && targetStillRuns
                        && sawProbeLink
                        && sawFinalLink
            let details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "preservedTarget: " <> T.pack (show preservedTarget)
                    , "targetRunExitCode: " <> maybe "not-run" (T.pack . show) targetRunResult
                    , "sawProbeLink: " <> T.pack (show sawProbeLink)
                    , "sawFinalLink: " <> T.pack (show sawFinalLink)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsBogusFinalLinkOutputMsg ok details

runAsmRejectsNamedPipeFinalLinkOutputTest :: IO (Either T.Text T.Text, String)
runAsmRejectsNamedPipeFinalLinkOutputTest =
    flip finally
        (clean
            [ "tmp-final-link.fifo.out"
            , "tmp.out"
            , "tmp.err"
            , fakeNamedPipeFinalLinkDriverPath
            , fakeNamedPipeFinalLinkDriverLogPath
            ])
        $ do
            htccCmd <- htccCommand
            writeNamedPipeFinalLinkDriver
                fakeNamedPipeFinalLinkDriverPath
                fakeNamedPipeFinalLinkDriverLogPath
            T.writeFile "tmp-final-link.fifo.out" "#!/bin/sh\nexit 97\n"
            setFileMode "tmp-final-link.fifo.out" permissionFileModeMask
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeNamedPipeFinalLinkDriverPath
                , " "
                , htccCmd
                , " -r -o tmp-final-link.fifo.out /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp-final-link.fifo.out"
            driverInvocations <- T.lines <$> T.readFile fakeNamedPipeFinalLinkDriverLogPath
            targetContents <- if targetExists then T.readFile "tmp-final-link.fifo.out" else pure ""
            targetRunResult <- if targetExists then Just <$> exec "./tmp-final-link.fifo.out" else pure Nothing
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "non-runnable final output for -r" `T.isInfixOf` stderrOut
                sawProbeLink =
                    any ("-no-pie -o" `T.isInfixOf`) $
                        filter ("htcc-probe-" `T.isInfixOf`) driverInvocations
                sawFinalLink =
                    any
                        (\line ->
                            "-no-pie -o" `T.isInfixOf` line
                                && "tmp-final-link.fifo.out.htcc-" `T.isInfixOf` line
                        )
                        driverInvocations
                preservedTarget = targetContents == "#!/bin/sh\nexit 97\n"
                targetStillRuns = maybe False (exitCode (== 97) False) targetRunResult
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && targetExists
                        && preservedTarget
                        && targetStillRuns
                        && sawProbeLink
                        && sawFinalLink
            let details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "preservedTarget: " <> T.pack (show preservedTarget)
                    , "targetRunExitCode: " <> maybe "not-run" (T.pack . show) targetRunResult
                    , "sawProbeLink: " <> T.pack (show sawProbeLink)
                    , "sawFinalLink: " <> T.pack (show sawFinalLink)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsNamedPipeFinalLinkOutputMsg ok details

runAsmRejectsExecutableObjectProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsExecutableObjectProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeExecutableObjectProbeDriverPath
            , fakeExecutableObjectProbeDriverLogPath
            ])
        $ do
            htccCmd <- htccCommand
            writeExecutableObjectProbeDriver
                fakeExecutableObjectProbeDriverPath
                fakeExecutableObjectProbeDriverLogPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeExecutableObjectProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeExecutableObjectProbeDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "HTCC_ASSEMBLER must target x86_64-ELF for -r" `T.isInfixOf` stderrOut
                        && "non-relocatable x86_64-ELF file" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
                sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && not sawLinkProbe
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsExecutableObjectProbeDriverMsg ok details

runAsmRejectsSymlinkObjectProbeDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsSymlinkObjectProbeDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeSymlinkObjectProbeDriverPath
            , fakeSymlinkObjectProbeDriverLogPath
            , fakeSymlinkObjectProbeTargetPath
            ])
        $ do
            htccCmd <- htccCommand
            writeSymlinkObjectProbeDriver
                fakeSymlinkObjectProbeDriverPath
                fakeSymlinkObjectProbeDriverLogPath
                fakeSymlinkObjectProbeTargetPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeSymlinkObjectProbeDriverPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            driverInvocations <- T.lines <$> T.readFile fakeSymlinkObjectProbeDriverLogPath
            let failed = exitCode (const True) False result
                hasExpectedError =
                    "failed an x86_64-ELF assembly probe for -r" `T.isInfixOf` stderrOut
                sawMetadataProbe =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        driverInvocations
                sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
                sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && sawMetadataProbe
                        && sawAssemblyProbe
                        && not sawLinkProbe
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "driverInvocations:"
                    , T.unlines driverInvocations
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                    , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                    , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsSymlinkObjectProbeDriverMsg ok details

runAsmRejectsTouchingLinkDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsTouchingLinkDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeTouchingLinkDriverPath, fakeTouchingLinkDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeTouchingLinkDriver fakeTouchingLinkDriverPath fakeTouchingLinkDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeTouchingLinkDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeTouchingLinkDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError =
                "failed a link probe for -r" `T.isInfixOf` stderrOut
                    && "supports both assembly and linking for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            sawLinkProbe = any ("-no-pie -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && sawLinkProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "sawLinkProbe: " <> T.pack (show sawLinkProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsTouchingLinkDriverMsg ok details

runAsmRejectsIncompatibleTargetDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsIncompatibleTargetDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeHostDriverPath, fakeHostDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeHostTargetDriver fakeHostDriverPath fakeHostDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeHostDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverArgs <- T.lines <$> T.readFile fakeHostDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError = "HTCC_ASSEMBLER must target x86_64-ELF for -r" `T.isInfixOf` stderrOut
            sawMetadataProbe =
                any
                    (\line ->
                        "-dumpmachine" `T.isInfixOf` line
                            || "-print-target-triple" `T.isInfixOf` line
                    )
                    driverArgs
            sawAssemblyProbe =
                any
                    (\line ->
                        "-x assembler -c -o" `T.isInfixOf` line
                    )
                    driverArgs
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && not targetExists
                    && sawMetadataProbe
                    && sawAssemblyProbe
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverArgs:"
                , T.unlines driverArgs
                , "targetExists: " <> T.pack (show targetExists)
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsIncompatibleTargetDriverMsg ok details

runAsmRejectsMetadataSpoofingDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsMetadataSpoofingDriverTest =
    flip finally (clean ["tmp", "tmp.out", "tmp.err", fakeSpoofedTargetDriverPath, fakeSpoofedTargetDriverLogPath]) $ do
        htccCmd <- htccCommand
        writeMetadataSpoofingDriver fakeSpoofedTargetDriverPath fakeSpoofedTargetDriverLogPath
        result <- exec $ mconcat
            [ "echo '"
            , source
            , "' | "
            , "HTCC_ASSEMBLER=./"
            , T.pack fakeSpoofedTargetDriverPath
            , " "
            , htccCmd
            , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist "tmp"
        driverInvocations <- T.lines <$> T.readFile fakeSpoofedTargetDriverLogPath
        let failed = exitCode (const True) False result
            hasExpectedError = "HTCC_ASSEMBLER must target x86_64-ELF for -r" `T.isInfixOf` stderrOut
            detectedNonElfObject = "non-ELF object file" `T.isInfixOf` stderrOut
            sawMetadataProbe = any (`elem` ["-dumpmachine", "-print-target-triple"]) driverInvocations
            sawAssemblyProbe = any ("-x assembler -c -o" `T.isInfixOf`) driverInvocations
            ok =
                failed
                    && T.null stdoutLeak
                    && hasExpectedError
                    && detectedNonElfObject
                    && sawMetadataProbe
                    && sawAssemblyProbe
                    && not targetExists
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "driverInvocations:"
                , T.unlines driverInvocations
                , "hasExpectedError: " <> T.pack (show hasExpectedError)
                , "detectedNonElfObject: " <> T.pack (show detectedNonElfObject)
                , "sawMetadataProbe: " <> T.pack (show sawMetadataProbe)
                , "sawAssemblyProbe: " <> T.pack (show sawAssemblyProbe)
                , "targetExists: " <> T.pack (show targetExists)
                , "exitCode: " <> T.pack (show result)
                ]
        return $ mkResult runAsmRejectsMetadataSpoofingDriverMsg ok details

runAsmRejectsWrappedNonElfDriverTest :: IO (Either T.Text T.Text, String)
runAsmRejectsWrappedNonElfDriverTest =
    flip finally
        (clean
            [ "tmp"
            , "tmp.out"
            , "tmp.err"
            , fakeProbeHostAssemblerPath
            , fakeProbeHostAssemblerLogPath
            , fakeProbeHostAssemblerAsmPath
            , fakeProbeHostWrapperPath
            , fakeProbeHostWrapperLogPath
            ]
        ) $ do
            htccCmd <- htccCommand
            writeFakeAssemblerWithTarget
                "x86_64-w64-mingw32"
                fakeProbeHostAssemblerLogPath
                fakeProbeHostAssemblerAsmPath
                fakeProbeHostAssemblerPath
            writeProbeRejectingWrapper fakeProbeHostWrapperPath fakeProbeHostWrapperLogPath fakeProbeHostAssemblerPath
            result <- exec $ mconcat
                [ "echo '"
                , source
                , "' | "
                , "HTCC_ASSEMBLER=./"
                , T.pack fakeProbeHostWrapperPath
                , " "
                , htccCmd
                , " -r -o tmp /dev/stdin > tmp.out 2> tmp.err"
                ]
            stdoutLeak <- T.readFile "tmp.out"
            stderrOut <- T.readFile "tmp.err"
            targetExists <- doesFileExist "tmp"
            wrapperArgs <- T.lines <$> T.readFile fakeProbeHostWrapperLogPath
            assemblerArgs <- T.lines <$> T.readFile fakeProbeHostAssemblerLogPath
            let failed = exitCode (const True) False result
                hasExpectedError = "HTCC_ASSEMBLER must target x86_64-ELF for -r" `T.isInfixOf` stderrOut
                detectedNonElfObject = "non-ELF object file" `T.isInfixOf` stderrOut
                sawProbeAttempt =
                    any
                        (\line ->
                            "-dumpmachine" `T.isInfixOf` line
                                || "-print-target-triple" `T.isInfixOf` line
                        )
                        wrapperArgs
                sawProbeAssemble =
                    all (`elem` assemblerArgs)
                        [ "-x"
                        , "assembler"
                        , "-c"
                        , "-o"
                        ]
                ok =
                    failed
                        && T.null stdoutLeak
                        && hasExpectedError
                        && detectedNonElfObject
                        && sawProbeAttempt
                        && sawProbeAssemble
                        && not targetExists
                details = T.unlines
                    [ "stdout:"
                    , stdoutLeak
                    , "stderr:"
                    , stderrOut
                    , "wrapperArgs:"
                    , T.unlines wrapperArgs
                    , "assemblerArgs:"
                    , T.unlines assemblerArgs
                    , "hasExpectedError: " <> T.pack (show hasExpectedError)
                    , "detectedNonElfObject: " <> T.pack (show detectedNonElfObject)
                    , "sawProbeAttempt: " <> T.pack (show sawProbeAttempt)
                    , "sawProbeAssemble: " <> T.pack (show sawProbeAssemble)
                    , "targetExists: " <> T.pack (show targetExists)
                    , "exitCode: " <> T.pack (show result)
                    ]
            return $ mkResult runAsmRejectsWrappedNonElfDriverMsg ok details

runAsmFreshOutputRestrictiveUmaskTest :: IO (Either T.Text T.Text, String)
runAsmFreshOutputRestrictiveUmaskTest =
    flip finally (clean ["tmp-umask-bin", "tmp.out", "tmp.err", fakeAssemblerPath, fakeAssemblerLogPath, fakeAssemblerAsmPath]) $ do
        htccCmd <- htccCommand
        let target = "tmp-umask-bin"
            expectedMode = foldr1 unionFileModes
                [ ownerReadMode
                , groupReadMode
                , otherReadMode
                , ownerExecuteMode
                ]
        writeFakeAssembler fakeAssemblerPath
        result <- exec $ mconcat
            [ "sh -c \"umask 0222; echo '"
            , source
            , "' | HTCC_ASSEMBLER=./tmp-assembler.sh "
            , htccCmd
            , " -r -o "
            , T.pack target
            , " /dev/stdin > tmp.out 2> tmp.err\""
            ]
        stdoutLeak <- T.readFile "tmp.out"
        stderrOut <- T.readFile "tmp.err"
        targetExists <- doesFileExist target
        replacedMode <- if targetExists then fileMode <$> getFileStatus target else pure 0
        let replacedPermissions = permissionBits replacedMode
        programResult <- if targetExists then Just <$> exec "./tmp-umask-bin" else pure Nothing
        let succeeded = exitCode (const False) True result
            ranOk = maybe False (exitCode (const False) True) programResult
            ok =
                succeeded
                    && T.null stdoutLeak
                    && T.null stderrOut
                    && targetExists
                    && replacedPermissions == expectedMode
                    && ranOk
            details = T.unlines
                [ "stdout:"
                , stdoutLeak
                , "stderr:"
                , stderrOut
                , "targetExists: " <> T.pack (show targetExists)
                , "replacedMode: " <> T.pack (show replacedMode)
                , "replacedPermissions: " <> T.pack (show replacedPermissions)
                , "expectedMode: " <> T.pack (show expectedMode)
                , "compileExitCode: " <> T.pack (show result)
                , "runExitCode: " <> maybe "not-run" (T.pack . show) programResult
                ]
        return $ mkResult runAsmFreshOutputRestrictiveUmaskMsg ok details
