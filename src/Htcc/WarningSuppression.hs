module Htcc.WarningSuppression
    ( CompilerOutputChunk
    , CompilerWarningFilterDecision (..)
    , dropCompilerWarningOutput
    , emptyIncrementalCompilerWarningFilter
    , feedIncrementalCompilerWarningFilter
    , finalCompilerOutputChunk
    , finalizeIncrementalCompilerWarningFilter
    , filterCompilerOutputChunks
    , incompleteCompilerOutputNeedsMoreInputForWarningSuppression
    , IncrementalCompilerWarningFilter
    , newlineByte
    , normalizeCompilerOutputLine
    , splitCompleteCompilerOutputChunks
    , splitCompilerOutputChunks
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isDigit, isSpace, toLower)
import qualified Data.Map.Strict       as Map
import           Data.Word             (Word8)

type CompilerOutputChunk = (B.ByteString, B.ByteString)

data WarningSuppressionState
    = NotSuppressingWarnings
    | SuppressingWarning
    | AwaitingWarningSummary

data CompilerWarningFilterDecision chunk
    = RetainCompilerWarningFilterChunk chunk
    | SuppressCompilerWarningFilterChunk chunk

data IncrementalCompilerWarningFilter chunk = IncrementalCompilerWarningFilter
    { incrementalCompilerWarningLeadInChunks :: [chunk]
    , incrementalCompilerWarningState        :: WarningSuppressionState
    , incrementalCompilerWarningDeferred     :: [chunk]
    }

data LookaheadStatus
    = LookaheadMatched
    | LookaheadNoMatch
    | LookaheadNeedsMoreInput
    deriving (Eq)

emptyIncrementalCompilerWarningFilter :: IncrementalCompilerWarningFilter chunk
emptyIncrementalCompilerWarningFilter =
    IncrementalCompilerWarningFilter
        { incrementalCompilerWarningLeadInChunks = []
        , incrementalCompilerWarningState = NotSuppressingWarnings
        , incrementalCompilerWarningDeferred = []
        }

feedIncrementalCompilerWarningFilter
    :: (chunk -> B.ByteString)
    -> (chunk -> B.ByteString)
    -> IncrementalCompilerWarningFilter chunk
    -> [chunk]
    -> (IncrementalCompilerWarningFilter chunk, [CompilerWarningFilterDecision chunk])
feedIncrementalCompilerWarningFilter chunkBytes chunkLine warningFilter newChunks =
    let (pendingLeadIn, warningState, deferredChunks, decisions) =
            stepIncrementalCompilerWarningFilter
                False
                chunkBytes
                chunkLine
                (incrementalCompilerWarningLeadInChunks warningFilter)
                (incrementalCompilerWarningState warningFilter)
                (incrementalCompilerWarningDeferred warningFilter <> newChunks)
     in ( IncrementalCompilerWarningFilter
            { incrementalCompilerWarningLeadInChunks = pendingLeadIn
            , incrementalCompilerWarningState = warningState
            , incrementalCompilerWarningDeferred = deferredChunks
            }
        , decisions
        )

finalizeIncrementalCompilerWarningFilter
    :: (chunk -> B.ByteString)
    -> (chunk -> B.ByteString)
    -> IncrementalCompilerWarningFilter chunk
    -> [CompilerWarningFilterDecision chunk]
finalizeIncrementalCompilerWarningFilter chunkBytes chunkLine warningFilter =
    let (_, _, _, decisions) =
            stepIncrementalCompilerWarningFilter
                True
                chunkBytes
                chunkLine
                (incrementalCompilerWarningLeadInChunks warningFilter)
                (incrementalCompilerWarningState warningFilter)
                (incrementalCompilerWarningDeferred warningFilter)
     in decisions

stepIncrementalCompilerWarningFilter
    :: Bool
    -> (chunk -> B.ByteString)
    -> (chunk -> B.ByteString)
    -> [chunk]
    -> WarningSuppressionState
    -> [chunk]
    -> ([chunk], WarningSuppressionState, [chunk], [CompilerWarningFilterDecision chunk])
stepIncrementalCompilerWarningFilter inputComplete _chunkBytes chunkLine =
    go
    where
        go pendingWarningLeadIn SuppressingWarning [] =
            if inputComplete
                then
                    ( []
                    , NotSuppressingWarnings
                    , []
                    , suppressCompilerWarningFilterChunks pendingWarningLeadIn
                    )
                else
                    ( pendingWarningLeadIn
                    , SuppressingWarning
                    , []
                    , []
                    )
        go pendingWarningLeadIn warningState [] =
            if inputComplete
                then
                    ( []
                    , NotSuppressingWarnings
                    , []
                    , retainCompilerWarningFilterChunks pendingWarningLeadIn
                    )
                else
                    ( pendingWarningLeadIn
                    , warningState
                    , []
                    , []
                    )
        go pendingWarningLeadIn SuppressingWarning chunks@(chunk:rest) =
            let line = chunkLine chunk
             in if isCompilerWarningSummaryLine line
                    then
                        let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                go pendingWarningLeadIn SuppressingWarning rest
                         in ( pendingLeadIn'
                            , warningState'
                            , deferredChunks'
                            , SuppressCompilerWarningFilterChunk chunk : decisions
                            )
                    else case suppressedCompilerDiagnosticStatus line rest of
                        LookaheadMatched ->
                            let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                    go pendingWarningLeadIn SuppressingWarning rest
                             in ( pendingLeadIn'
                                , warningState'
                                , deferredChunks'
                                , SuppressCompilerWarningFilterChunk chunk : decisions
                                )
                        LookaheadNeedsMoreInput ->
                            ( pendingWarningLeadIn
                            , SuppressingWarning
                            , chunks
                            , []
                            )
                        LookaheadNoMatch ->
                            let retainPendingLeadIn =
                                    isCompilerRetainedDiagnosticChunk line
                                nextState
                                    | retainPendingLeadIn = NotSuppressingWarnings
                                    | otherwise = AwaitingWarningSummary
                                (pendingLeadIn', warningState', deferredChunks', decisions) =
                                    go [] nextState rest
                             in ( pendingLeadIn'
                                , warningState'
                                , deferredChunks'
                                , ( if retainPendingLeadIn
                                        then retainCompilerWarningFilterChunks pendingWarningLeadIn
                                        else suppressCompilerWarningFilterChunks pendingWarningLeadIn
                                  )
                                    <> (RetainCompilerWarningFilterChunk chunk : decisions)
                                )
        go pendingWarningLeadIn NotSuppressingWarnings chunks =
            goNonSuppressing False pendingWarningLeadIn chunks
        go pendingWarningLeadIn AwaitingWarningSummary chunks =
            goNonSuppressing True pendingWarningLeadIn chunks

        goNonSuppressing suppressTrailingSummary pendingWarningLeadIn chunks@(chunk:rest) =
            let line = chunkLine chunk
                nextState = NotSuppressingWarnings
             in if suppressTrailingSummary && isCompilerWarningSummaryLine line
                    then
                        let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                go [] NotSuppressingWarnings rest
                         in ( pendingLeadIn'
                            , warningState'
                            , deferredChunks'
                            , retainCompilerWarningFilterChunks pendingWarningLeadIn
                                <> (SuppressCompilerWarningFilterChunk chunk : decisions)
                            )
                    else case compilerWarningChunkStatus line rest of
                        LookaheadMatched ->
                            let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                    go pendingWarningLeadIn SuppressingWarning rest
                             in ( pendingLeadIn'
                                , warningState'
                                , deferredChunks'
                                , SuppressCompilerWarningFilterChunk chunk : decisions
                                )
                        LookaheadNeedsMoreInput ->
                            ( pendingWarningLeadIn
                            , nextState
                            , chunks
                            , []
                            )
                        LookaheadNoMatch
                            | isCompilerWarningLeadInChunk line ->
                                go
                                    (pendingWarningLeadIn <> [chunk])
                                    nextState
                                    rest
                            | not (null pendingWarningLeadIn) ->
                                case suppressedCompilerDiagnosticStatus line rest of
                                    LookaheadMatched ->
                                        go
                                            (pendingWarningLeadIn <> [chunk])
                                            nextState
                                            rest
                                    LookaheadNeedsMoreInput ->
                                        ( pendingWarningLeadIn
                                        , nextState
                                        , chunks
                                        , []
                                        )
                                    LookaheadNoMatch ->
                                        let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                                go [] nextState rest
                                         in ( pendingLeadIn'
                                            , warningState'
                                            , deferredChunks'
                                            , retainCompilerWarningFilterChunks pendingWarningLeadIn
                                                <> (RetainCompilerWarningFilterChunk chunk : decisions)
                                            )
                            | otherwise ->
                                let (pendingLeadIn', warningState', deferredChunks', decisions) =
                                        go [] nextState rest
                                 in ( pendingLeadIn'
                                    , warningState'
                                    , deferredChunks'
                                    , RetainCompilerWarningFilterChunk chunk : decisions
                                    )
        goNonSuppressing suppressTrailingSummary pendingWarningLeadIn [] =
            go pendingWarningLeadIn nextState []
            where
                nextState
                    | suppressTrailingSummary = AwaitingWarningSummary
                    | otherwise = NotSuppressingWarnings

        retainCompilerWarningFilterChunks =
            map RetainCompilerWarningFilterChunk

        suppressCompilerWarningFilterChunks =
            map SuppressCompilerWarningFilterChunk

        isCompilerWarningLeadInChunk line =
            isCompilerWarningPreambleLine line
                || isAttachedCompilerWarningNoteLine line

        compilerWarningChunkStatus line rest
            | not (isCompilerWarningLine line) =
                LookaheadNoMatch
            | not (isBareCompilerWarningCaptureChunk line) =
                LookaheadMatched
            | otherwise =
                invertLookaheadStatus $
                    compilerDiagnosticSnippetContinuationStatus rest

        suppressedCompilerDiagnosticStatus line rest =
            case suppressedNonNoteCompilerDiagnosticStatus line rest of
                LookaheadMatched ->
                    LookaheadMatched
                LookaheadNeedsMoreInput ->
                    LookaheadNeedsMoreInput
                LookaheadNoMatch ->
                    suppressedCompilerWarningNoteStatus line rest

        suppressedNonNoteCompilerDiagnosticStatus line rest
            | isCompilerWarningLine line =
                compilerWarningLineSuppressionStatus line rest
            | isCompilerWarningSummaryLine line =
                LookaheadMatched
            | isCompilerWarningContinuationLine line =
                LookaheadMatched
            | isCompilerDiagnosticAnnotationLine line =
                LookaheadMatched
            | isGenericCompilerDiagnosticSourceSnippetLine line =
                compilerDiagnosticSnippetContinuationStatus rest
            | otherwise =
                LookaheadNoMatch
            where
                compilerWarningLineSuppressionStatus warningLine remaining
                    | not (isBareCompilerWarningCaptureChunk warningLine) =
                        LookaheadMatched
                    | otherwise =
                        compilerDiagnosticSnippetContinuationStatus remaining

        suppressedCompilerWarningNoteStatus line rest
            | not (isCompilerWarningNoteLine line) =
                LookaheadNoMatch
            | isAttachedCompilerWarningNoteLine line =
                LookaheadMatched
            | otherwise =
                nextChunkContinuesSuppressedWarningStatus rest

        nextChunkContinuesSuppressedWarningStatus [] =
            incompleteLookaheadNoMatch
        nextChunkContinuesSuppressedWarningStatus (next:remaining) =
            suppressedNonNoteCompilerDiagnosticStatus (chunkLine next) remaining

        compilerDiagnosticSnippetContinuationStatus [] =
            incompleteLookaheadNoMatch
        compilerDiagnosticSnippetContinuationStatus (next:remaining)
            | isCompilerDiagnosticAnnotationLine nextLine =
                LookaheadMatched
            | isGenericCompilerDiagnosticSourceSnippetLine nextLine =
                compilerDiagnosticSnippetContinuationStatus remaining
            | otherwise =
                LookaheadNoMatch
            where
                nextLine = chunkLine next

        incompleteLookaheadNoMatch
            | inputComplete =
                LookaheadNoMatch
            | otherwise =
                LookaheadNeedsMoreInput

        invertLookaheadStatus lookaheadStatus =
            case lookaheadStatus of
                LookaheadMatched        -> LookaheadNoMatch
                LookaheadNoMatch        -> LookaheadMatched
                LookaheadNeedsMoreInput -> LookaheadNeedsMoreInput

        isCompilerWarningLine =
            isCompilerWarningCaptureChunk

        isCompilerWarningPreambleLine =
            isCompilerWarningPreambleCaptureChunk

        isCompilerWarningNoteLine =
            isCompilerWarningNoteCaptureChunk

        isCompilerWarningSummaryLine =
            isCompilerWarningSummaryCaptureChunk

        isCompilerWarningContinuationLine =
            isCompilerWarningContinuationCaptureChunk

        isCompilerRetainedDiagnosticChunk =
            isCompilerErrorCaptureChunk

        isAttachedCompilerWarningNoteLine line =
            case BC.breakSubstring noteNeedle (BC.map toLower line) of
                (prefix, suffix) ->
                    not (B.null suffix)
                        && BC.count ':' prefix >= 2
                        && BC.any isDigit prefix

        isGenericCompilerDiagnosticSourceSnippetLine line =
            let trimmed = BC.dropWhile isSpace line
             in not (B.null trimmed)
                    && not (isCompilerWarningPreambleLine line)
                    && not (isCompilerWarningLine line)
                    && not (isCompilerWarningSummaryLine line)
                    && not (isCompilerWarningNoteLine line)
                    && not (isCompilerDiagnosticAnnotationLine line)

        isCompilerDiagnosticAnnotationLine =
            isCompilerDiagnosticAnnotationCaptureChunk

        noteNeedle = BC.pack "note:"

dropCompilerWarningOutput :: B.ByteString -> B.ByteString
dropCompilerWarningOutput =
    B.concat . map fst . filterCompilerOutputChunks fst snd . splitCompilerOutputChunks

incompleteCompilerOutputNeedsMoreInputForWarningSuppression :: B.ByteString -> Bool
incompleteCompilerOutputNeedsMoreInputForWarningSuppression line =
    BC.all isSpace line
        || isPotentialBareWarningPrefix loweredLine
        || isPotentialLocatedWarningPrefix loweredLine
        || isPotentialBareWarningLeadInPrefix loweredLine
        || isPotentialLocatedWarningLeadInPrefix loweredLine
        || isPotentialWarningSummaryPrefix loweredLine
        || isPotentialDiagnosticAnnotationPrefix line
    where
        loweredLine =
            BC.map toLower $ BC.dropWhile isSpace line

        isPotentialBareWarningPrefix current =
            any (current `BC.isPrefixOf`) warningPrefixes

        isPotentialLocatedWarningPrefix current =
            case lastColonSeparatedSuffix current of
                Just (headerPrefix, suffix) ->
                    looksLikeLocatedDiagnosticPrefix headerPrefix
                        && any (suffix `BC.isPrefixOf`) warningPrefixes
                Nothing ->
                    False

        isPotentialBareWarningLeadInPrefix current =
            any (current `BC.isPrefixOf`) bareWarningLeadInPrefixes

        isPotentialLocatedWarningLeadInPrefix current =
            case lastColonSeparatedSuffix current of
                Just (headerPrefix, suffix) ->
                    looksLikeLocatedDiagnosticPrefix headerPrefix
                        && any (suffix `BC.isPrefixOf`) locatedWarningLeadInPrefixes
                Nothing ->
                    False

        isPotentialWarningSummaryPrefix current =
            case BC.span isDigit current of
                (countPrefix, rest)
                    | not (B.null countPrefix) ->
                        let summaryPrefix = BC.dropWhile isSpace rest
                         in B.null summaryPrefix
                                || any (summaryPrefix `BC.isPrefixOf`) warningSummaryPrefixes
                _ ->
                    False

        isPotentialDiagnosticAnnotationPrefix rawLine =
            let trimmedLine = BC.dropWhile isSpace rawLine
             in any (trimmedLine `BC.isPrefixOf`) diagnosticAnnotationPrefixes

        lastColonSeparatedSuffix current =
            case B.elemIndices (fromIntegral $ fromEnum ':') current of
                [] ->
                    Nothing
                colonIndices ->
                    let colonIndex = last colonIndices
                     in Just
                            ( B.take colonIndex current
                            , BC.dropWhile isSpace $ B.drop (colonIndex + 1) current
                            )

        looksLikeLocatedDiagnosticPrefix headerPrefix =
            looksLikeCompilerDiagnosticHeaderPrefix headerPrefix
                && BC.any isLocatedDiagnosticHeaderPrefixChar headerPrefix

        isLocatedDiagnosticHeaderPrefixChar c =
            isDigit c || c `elem` ("./\\{}()-_" :: String)

        warningPrefixes =
            [ BC.pack "warning:"
            , BC.pack "note:"
            ]

        bareWarningLeadInPrefixes =
            [ BC.pack "in file included from "
            , BC.pack "from "
            ]

        locatedWarningLeadInPrefixes =
            [ BC.pack "in function "
            , BC.pack "assembler messages:"
            ]

        warningSummaryPrefixes =
            [ BC.pack "warning"
            , BC.pack "warnings"
            , BC.pack "warning generated"
            , BC.pack "warning generated."
            , BC.pack "warnings generated"
            , BC.pack "warnings generated."
            , BC.pack "warning emitted"
            , BC.pack "warning emitted."
            , BC.pack "warnings emitted"
            , BC.pack "warnings emitted."
            ]

        diagnosticAnnotationPrefixes =
            [ BC.pack "^"
            , BC.pack "|"
            ]

filterCompilerOutputChunks
    :: (chunk -> B.ByteString)
    -> (chunk -> B.ByteString)
    -> [chunk]
    -> [chunk]
filterCompilerOutputChunks chunkBytes chunkLine chunks =
    map snd $
        filter
            (\(chunkIndex, _) -> Map.findWithDefault True chunkIndex decisionMap)
            indexedChunks
    where
        indexedChunks = zip [0 :: Int ..] chunks
        (warningFilter, initialDecisions) =
            feedIncrementalCompilerWarningFilter
                (chunkBytes . snd)
                (chunkLine . snd)
                emptyIncrementalCompilerWarningFilter
                indexedChunks
        decisions =
            initialDecisions
                <> finalizeIncrementalCompilerWarningFilter
                    (chunkBytes . snd)
                    (chunkLine . snd)
                    warningFilter
        decisionMap =
            Map.fromList $
                map compilerWarningFilterDecisionEntry decisions

        compilerWarningFilterDecisionEntry decision =
            case decision of
                RetainCompilerWarningFilterChunk (chunkIndex, _) ->
                    (chunkIndex, True)
                SuppressCompilerWarningFilterChunk (chunkIndex, _) ->
                    (chunkIndex, False)

isCompilerWarningCaptureChunk :: B.ByteString -> Bool
isCompilerWarningCaptureChunk line =
    isBareCompilerDiagnosticCaptureChunk warningNeedles line
        || isLocatedCompilerDiagnosticCaptureChunk warningNeedles normalizedLine
    where
        normalizedLine = normalizeCompilerDiagnosticCaptureChunk line
        warningNeedles = [BC.pack "warning:"]

isBareCompilerWarningCaptureChunk :: B.ByteString -> Bool
isBareCompilerWarningCaptureChunk =
    isBareCompilerDiagnosticCaptureChunk [warningNeedle]
    where
        warningNeedle = BC.pack "warning:"

isCompilerErrorCaptureChunk :: B.ByteString -> Bool
isCompilerErrorCaptureChunk line =
    isBareCompilerDiagnosticCaptureChunk errorNeedles line
        || isLocatedCompilerDiagnosticCaptureChunk errorNeedles normalizedLine
    where
        normalizedLine = normalizeCompilerDiagnosticCaptureChunk line
        errorNeedles =
            [ BC.pack "error:"
            , BC.pack "fatal error:"
            ]

isBareCompilerDiagnosticCaptureChunk :: [B.ByteString] -> B.ByteString -> Bool
isBareCompilerDiagnosticCaptureChunk needles line =
    any (`BC.isPrefixOf` normalizedLine) needles
    where
        normalizedLine = normalizeCompilerDiagnosticCaptureChunk line

isLocatedCompilerDiagnosticCaptureChunk
    :: [B.ByteString]
    -> B.ByteString
    -> Bool
isLocatedCompilerDiagnosticCaptureChunk needles normalized =
    any locatedDiagnosticNeedleMatches needles
    where
        locatedDiagnosticNeedleMatches diagnosticNeedle =
            case BC.breakSubstring diagnosticNeedle normalized of
                (prefix, suffix)
                    | not (B.null suffix) ->
                        let trimmedPrefix = BC.dropWhileEnd isSpace prefix
                         in not (B.null trimmedPrefix)
                                && BC.last trimmedPrefix == ':'
                                && looksLikeCompilerDiagnosticHeaderPrefix
                                    (BC.init trimmedPrefix)
                _ ->
                    False

normalizeCompilerDiagnosticCaptureChunk :: B.ByteString -> B.ByteString
normalizeCompilerDiagnosticCaptureChunk =
    BC.dropWhile isSpace . BC.map toLower

looksLikeCompilerDiagnosticHeaderPrefix :: B.ByteString -> Bool
looksLikeCompilerDiagnosticHeaderPrefix prefix =
    not (B.null prefix)
        && not (BC.any isClearlyNonDiagnosticHeaderPunctuation prefix)

isClearlyNonDiagnosticHeaderPunctuation :: Char -> Bool
isClearlyNonDiagnosticHeaderPunctuation c =
    c `elem` ("\";" :: String)

isCompilerWarningPreambleCaptureChunk :: B.ByteString -> Bool
isCompilerWarningPreambleCaptureChunk line =
    let normalizedLine = BC.map toLower $ BC.dropWhile isSpace line
     in includedFromPrefix `BC.isPrefixOf` normalizedLine
            || fromPrefix `BC.isPrefixOf` normalizedLine
            || inFunctionNeedle `BC.isInfixOf` normalizedLine
            || assemblerMessagesSuffix `BC.isSuffixOf` normalizedLine
    where
        includedFromPrefix = BC.pack "in file included from "
        fromPrefix = BC.pack "from "
        inFunctionNeedle = BC.pack ": in function "
        assemblerMessagesSuffix = BC.pack ": assembler messages:"

isCompilerWarningSummaryCaptureChunk :: B.ByteString -> Bool
isCompilerWarningSummaryCaptureChunk line =
    case BC.words (BC.map toLower line) of
        [count, warningWord, trailer] ->
            BC.all isDigit count
                && warningWord `elem` [warningWordSingular, warningWordPlural]
                && trailer `elem` [generatedWord, generatedWordPeriod, emittedWord, emittedWordPeriod]
        _ ->
            False
    where
        warningWordSingular = BC.pack "warning"
        warningWordPlural = BC.pack "warnings"
        generatedWord = BC.pack "generated"
        generatedWordPeriod = BC.pack "generated."
        emittedWord = BC.pack "emitted"
        emittedWordPeriod = BC.pack "emitted."

isCompilerWarningNoteCaptureChunk :: B.ByteString -> Bool
isCompilerWarningNoteCaptureChunk line =
    noteNeedle `BC.isInfixOf` BC.map toLower line
    where
        noteNeedle = BC.pack "note:"

isCompilerWarningContinuationCaptureChunk :: B.ByteString -> Bool
isCompilerWarningContinuationCaptureChunk =
    BC.all isSpace

isCompilerDiagnosticAnnotationCaptureChunk :: B.ByteString -> Bool
isCompilerDiagnosticAnnotationCaptureChunk line =
    isCompilerDiagnosticCaretCaptureChunk line || isCompilerDiagnosticPipeCaptureChunk line

isCompilerDiagnosticCaretCaptureChunk :: B.ByteString -> Bool
isCompilerDiagnosticCaretCaptureChunk line =
    let trimmed = BC.dropWhile isSpace line
     in not (B.null trimmed)
            && BC.all (`elem` ("^~|" :: String)) trimmed

isCompilerDiagnosticPipeCaptureChunk :: B.ByteString -> Bool
isCompilerDiagnosticPipeCaptureChunk line =
    let trimmed = BC.dropWhile isSpace line
        (prefixDigits, rest) = BC.span isDigit trimmed
        pipePrefix = BC.pack "|"
        spacedPipePrefix = BC.pack " |"
     in pipePrefix `BC.isPrefixOf` trimmed
            || ( not (B.null prefixDigits)
                    && (pipePrefix `BC.isPrefixOf` rest
                            || spacedPipePrefix `BC.isPrefixOf` rest
                       )
               )

splitCompilerOutputChunks :: B.ByteString -> [CompilerOutputChunk]
splitCompilerOutputChunks bytes =
    completedChunks <> finalCompilerOutputChunk trailingBytes
    where
        (completedChunks, trailingBytes) = splitCompleteCompilerOutputChunks bytes

splitCompleteCompilerOutputChunks
    :: B.ByteString
    -> ([CompilerOutputChunk], B.ByteString)
splitCompleteCompilerOutputChunks bytes
    | B.null bytes =
        ([], B.empty)
    | otherwise =
        let (line, rest) = B.break (== newlineByte) bytes
         in case B.uncons rest of
                Just (_, remaining) ->
                    let rawLine = line `B.snoc` newlineByte
                        normalizedLine = normalizeCompilerOutputLine line
                        (remainingChunks, trailingBytes) =
                            splitCompleteCompilerOutputChunks remaining
                     in ((rawLine, normalizedLine) : remainingChunks, trailingBytes)
                Nothing ->
                    ([], line)

finalCompilerOutputChunk :: B.ByteString -> [CompilerOutputChunk]
finalCompilerOutputChunk bytes
    | B.null bytes = []
    | otherwise = [(bytes, normalizeCompilerOutputLine bytes)]

normalizeCompilerOutputLine :: B.ByteString -> B.ByteString
normalizeCompilerOutputLine line =
    stripAnsiEscapeSequences $
        B.dropWhileEnd isTrailingLineEndingByte line
    where
        isTrailingLineEndingByte byte =
            byte == carriageReturnByte || byte == newlineByte

stripAnsiEscapeSequences :: B.ByteString -> B.ByteString
stripAnsiEscapeSequences bytes =
    case B.uncons bytes of
        Just (escapeByte, rest)
            | escapeByte == ansiEscapeByte ->
                stripAnsiEscapeSequence rest
        Just (byte, rest) ->
            B.cons byte (stripAnsiEscapeSequences rest)
        Nothing ->
            B.empty

stripAnsiEscapeSequence :: B.ByteString -> B.ByteString
stripAnsiEscapeSequence bytes =
    case B.uncons bytes of
        Just (openBracketByte, rest)
            | openBracketByte == ansiControlSequenceIntroducerByte ->
                stripAnsiControlSequence rest
        Just (_, rest) ->
            stripAnsiEscapeSequences rest
        Nothing ->
            B.empty

stripAnsiControlSequence :: B.ByteString -> B.ByteString
stripAnsiControlSequence bytes =
    case B.uncons bytes of
        Just (byte, rest)
            | isAnsiControlSequenceParameterByte byte
                || isAnsiControlSequenceIntermediateByte byte ->
                    stripAnsiControlSequence rest
            | isAnsiControlSequenceFinalByte byte ->
                stripAnsiEscapeSequences rest
        _ ->
            B.empty

isAnsiControlSequenceParameterByte :: Word8 -> Bool
isAnsiControlSequenceParameterByte byte =
    byte >= 0x30 && byte <= 0x3f

isAnsiControlSequenceIntermediateByte :: Word8 -> Bool
isAnsiControlSequenceIntermediateByte byte =
    byte >= 0x20 && byte <= 0x2f

isAnsiControlSequenceFinalByte :: Word8 -> Bool
isAnsiControlSequenceFinalByte byte =
    byte >= 0x40 && byte <= 0x7e

carriageReturnByte :: Word8
carriageReturnByte = 13

newlineByte :: Word8
newlineByte = 10

ansiEscapeByte :: Word8
ansiEscapeByte = 27

ansiControlSequenceIntroducerByte :: Word8
ansiControlSequenceIntroducerByte = 91
