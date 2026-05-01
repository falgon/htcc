{-# LANGUAGE OverloadedStrings #-}
module Tests.ComponentsTests.AsmOutput (
    test
) where

import           Control.Exception                           (IOException,
                                                              finally, try)
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Char8                       as BC
import           Data.Either                                 (isLeft)
import qualified Data.Map.Strict                             as Map
import qualified Data.Text                                   as T
import qualified Data.Text.IO                                as T
import           Data.Void                                   (Void)
import           Htcc.Asm                                    (casm',
                                                              normalizeAsmInput,
                                                              prepareAsmInput,
                                                              prepareVisualizableInput)
import qualified Htcc.Asm.Intrinsic.Structure.Internal       as SI
import qualified Htcc.CRules.Types                           as CT
import qualified Htcc.MegaparsecCompat                       as M
import           Htcc.Output                                 (ReplacementOutputMode (..),
                                                              creationMaskedOutputMode,
                                                              replaceExistingOutputFromPathWith,
                                                              withReplacementOutputPath,
                                                              withReplacementOutputPathAndResolvedPath)
import           Htcc.Parser                                 (ASTs, ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..))
import           Htcc.Parser.Combinators                     (parser, runParser)
import           Htcc.Parser.ConstructionData.Core           (Warnings)
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import           Htcc.Parser.ConstructionData.Scope.Var      (GVar (..),
                                                              GVarInitWith (..),
                                                              GlobalVars,
                                                              Literals,
                                                              materializeTentativeIncompleteArray)
import           Htcc.Visualizer                             (mkWidth,
                                                              visualize,
                                                              writeVisualization)
import           Htcc.WarningSuppression                     (CompilerWarningFilterDecision (..),
                                                              dropCompilerWarningOutput,
                                                              emptyIncrementalCompilerWarningFilter,
                                                              feedIncrementalCompilerWarningFilter,
                                                              finalizeIncrementalCompilerWarningFilter)
import qualified Htcc.WarningSuppression                     as WS
import           System.Directory                            (createDirectory,
                                                              doesDirectoryExist,
                                                              getTemporaryDirectory,
                                                              removeDirectory,
                                                              removeDirectoryRecursive,
                                                              removeFile)
import           System.FilePath                             (takeDirectory,
                                                              (</>))
import           System.IO                                   (IOMode (ReadMode, WriteMode),
                                                              hClose,
                                                              openTempFile,
                                                              withBinaryFile)
import           System.IO.Error                             (catchIOError)
import           System.Posix.Files                          (createLink,
                                                              createSymbolicLink,
                                                              fileMode,
                                                              getFileStatus,
                                                              intersectFileModes,
                                                              ownerExecuteMode,
                                                              ownerReadMode,
                                                              ownerWriteMode,
                                                              setFileMode,
                                                              unionFileModes)
import           System.Posix.IO                             (closeFd,
                                                              createFile)
import           System.Posix.Temp                           (mkdtemp)
import           System.Posix.Types                          (FileMode)
import           Test.HUnit                                  (Test (..),
                                                              assertBool,
                                                              assertEqual,
                                                              assertFailure)

parseAsmSource :: T.Text -> IO (ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
parseAsmSource source =
    case runParser parser "<components>" source
        :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer) of
        Left err ->
            assertFailure (M.errorBundlePretty err)
        Right (_, asts, gvars, lits, funcs) ->
            pure (asts, gvars, lits, funcs)

renderAsm :: T.Text -> IO T.Text
renderAsm = renderAsmWith id

renderAsmWith :: (GlobalVars Integer -> GlobalVars Integer) -> T.Text -> IO T.Text
renderAsmWith transformGVars source = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-asm.s"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        (asts, gvars, lits, funcs) <- parseAsmSource source
        SI.runAsmWithHandle h (casm' asts (transformGVars gvars) lits funcs :: SI.Asm SI.AsmCodeCtx Integer ())
        hClose h
        T.readFile path
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

renderVisualization :: T.Text -> IO T.Text
renderVisualization source = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-visualizer.svg"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        hClose h
        (asts, _, _, _) <- parseAsmSource source
        visualize asts (mkWidth 200) path
        T.readFile path
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

renderVisualizationFromAsts :: ASTs Integer -> IO T.Text
renderVisualizationFromAsts asts = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-visualizer-ast.svg"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        hClose h
        visualize asts (mkWidth 200) path
        T.readFile path
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

withVisualizerSymlinkPaths :: String -> String -> (FilePath -> FilePath -> IO a) -> IO a
withVisualizerSymlinkPaths targetName aliasName action = do
    tmpDir <- getTemporaryDirectory
    probeDir <- mkdtemp (tmpDir </> "htcc-components-visualizer-symlinkXXXXXX")
    let targetPath = probeDir </> targetName
        aliasPath = probeDir </> aliasName
        cleanup =
            ignoreIOException (removeFile aliasPath)
                >> ignoreIOException (removeFile targetPath)
                >> ignoreIOException (removeDirectory probeDir)
    flip finally cleanup $
        action targetPath aliasPath
    where
        ignoreIOException = flip catchIOError $ const $ pure ()
assertContains :: String -> [T.Text] -> T.Text -> IO ()
assertContains label needles haystack =
    assertBool label $ all (`T.isInfixOf` haystack) needles

assertContainsInOrder :: String -> [T.Text] -> T.Text -> IO ()
assertContainsInOrder label needles haystack =
    assertBool label $ go needles (T.lines haystack)
    where
        go [] _ = True
        go _ [] = False
        go remaining@(needle:rest) (line:lines')
            | needle `T.isInfixOf` line = go rest lines'
            | otherwise = go remaining lines'

assertOccursBefore :: String -> T.Text -> T.Text -> T.Text -> IO ()
assertOccursBefore label first second haystack =
    assertBool label $
        case (T.breakOn first haystack, T.breakOn second haystack) of
            ((_, restFirst), (_, restSecond))
                | T.null restFirst || T.null restSecond -> False
                | otherwise -> T.length restFirst > T.length restSecond

replacementFailurePreservesWriteOnlyOutputTest :: ReplacementOutputMode -> T.Text -> FileMode -> Test
replacementFailurePreservesWriteOnlyOutputTest modeStrategy label targetMode = TestLabel (T.unpack label) $ TestCase $ do
    tmpDir <- getTemporaryDirectory
    (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
    (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
    let cleanup =
            ignoreIOException (hClose targetHandle)
                >> ignoreIOException (hClose stagedHandle)
                >> ignoreIOException (removeFile targetPath)
                >> ignoreIOException (removeFile stagedPath)
        failDuringReplacement src dst
            | src == stagedPath =
                withBinaryFile src ReadMode $ \srcHandle ->
                    withBinaryFile dst WriteMode $ \dstHandle -> do
                        chunk <- B.hGetSome srcHandle 4
                        B.hPut dstHandle chunk
                        ioError $ userError "simulated replacement failure"
            | otherwise =
                assertFailure "unexpected replacement source path"
    flip finally cleanup $ do
        hClose targetHandle
        hClose stagedHandle
        T.writeFile targetPath staleOutput
        setFileMode targetPath targetMode
        T.writeFile stagedPath replacementOutput
        stagedMode <- fileMode <$> getFileStatus stagedPath
        result <- try
            (replaceExistingOutputFromPathWith failDuringReplacement modeStrategy targetPath targetMode stagedMode stagedPath)
            :: IO (Either IOException ())
        case result of
            Left _ -> pure ()
            Right _ -> assertFailure "replacement should fail after partially overwriting the target"
        restoredMode <- fileMode <$> getFileStatus targetPath
        setFileMode targetPath $ restoredMode `unionFileModes` ownerReadMode
        restoredOutput <- T.readFile targetPath
        assertEqual "restored file mode" targetMode $ intersectFileModes restoredMode ownerPermissionMask
        assertEqual "restored file contents" staleOutput restoredOutput
    where
        ownerPermissionMask =
            ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
        staleOutput = "stale output\n"
        replacementOutput = "replacement output\n"
        ignoreIOException = flip catchIOError $ const $ pure ()

extractFunctionSection :: T.Text -> T.Text -> T.Text
extractFunctionSection name asm =
    fst $
        T.breakOn "\n.global " $
            snd $
                T.breakOn ("\n" <> name <> ":\n") ("\n" <> asm)

returnLabelTest :: Test
returnLabelTest = TestLabel "Asm.Output.return" $ TestCase $ do
    asm <- renderAsm "int main() { return 0; }"
    assertContains
        "return labels are emitted into the requested handle"
        [ ".intel_syntax noprefix"
        , "jmp .L.return.main"
        , ".L.return.main:"
        ]
        asm

controlFlowLabelTest :: Test
controlFlowLabelTest = TestLabel "Asm.Output.control-flow" $ TestCase $ do
    asm <- renderAsm "int main() { int x; x = 0; while (x < 2) { if (x == 1) goto done; x = x + 1; continue; } done: switch (x) { case 1: return 0; default: return 1; } }"
    assertContains
        "control-flow labels and references stay on the requested handle"
        [ ".L.continue.main."
        , ".L.break.main."
        , ".L.label.main.done:"
        , "jmp .L.label.main.done"
        , ".L.case.main."
        , "je .L.case.main."
        ]
        asm

callArgumentGotoLabelTest :: Test
callArgumentGotoLabelTest = TestLabel "Asm.Output.call-argument-goto-label" $ TestCase $ do
    asm <- renderAsm "int f(int, int, char*); int main(void) { return f(3, ({ int i = 0; goto a; a: ++i; b: ++i; c: ++i; i; }), \"x\"); }"
    assertEqual
        "call argument code should not duplicate goto label definitions"
        [1, 1, 1]
        (map (\ident -> T.count (".L.label.main." <> ident <> ":") asm) ["a", "b", "c"])

globalInitializerCastTest :: Test
globalInitializerCastTest = TestLabel "Asm.Output.global-initializer-cast" $ TestCase $ do
    asm <- renderAsm "int g = (char)0x1234; int h = (char)0xff; int main() { return g == 52 && h == -1; }"
    assertContains
        "global initializer casts are folded before emitting data bytes"
        [ "g:"
        , ".4byte 52"
        , "h:"
        , ".4byte -1"
        ]
        asm
    assertBool
        "global initializer should not retain the uncast value"
        (not $ any (`T.isInfixOf` asm) [".4byte 4660", ".4byte 255"])

globalInitializerNullPointerCastTest :: Test
globalInitializerNullPointerCastTest = TestLabel "Asm.Output.global-initializer-null-pointer-cast" $ TestCase $ do
    asm <- renderAsm "char *p = (char*)0; int main(void) { return p == 0; }"
    assertContains
        "file-scope null pointer casts are emitted as zero-initialized pointer storage"
        [ "p:"
        , ".zero 8"
        ]
        asm

globalInitializerNestedNullPointerCastTest :: Test
globalInitializerNestedNullPointerCastTest = TestLabel "Asm.Output.global-initializer-nested-null-pointer-cast" $ TestCase $ do
    asm <- renderAsm "int *p = (int*)(void*)0; int main(void) { return p == 0; }"
    assertContains
        "nested file-scope null pointer casts are emitted as zero-initialized pointer storage"
        [ "p:"
        , ".zero 8"
        ]
        asm

globalInitializerFunctionNullPointerCastTest :: Test
globalInitializerFunctionNullPointerCastTest = TestLabel "Asm.Output.global-initializer-function-null-pointer-cast" $ TestCase $ do
    asm <- renderAsm "int (*fp)(void) = (int (*)(void))0; int main(void) { return fp == 0; }"
    assertContains
        "file-scope function-pointer null casts are emitted as zero-initialized pointer storage"
        [ "fp:"
        , ".zero 8"
        ]
        asm

globalInitializerWideCastTruncationTest :: Test
globalInitializerWideCastTruncationTest = TestLabel "Asm.Output.global-initializer-wide-cast-truncation" $ TestCase $ do
    asm <- renderAsm "long g = (long)0x10000000000000000; long h = (long)0x10000000000000001; char *p = (char*)0x10000000000000000; int main(void) { return g == 0 && h == 1 && p == 0; }"
    assertBool
        "8-byte global initializer casts are truncated before data emission"
        (all (`T.isInfixOf` asm)
            [ "g:\n\t.zero 8"
            , "h:\n\t.8byte 1"
            , "p:\n\t.zero 8"
            ]
        )
    assertBool
        "8-byte global initializer casts should not leak untruncated literals"
        (not $ any (`T.isInfixOf` asm)
            [ ".8byte 18446744073709551616"
            , ".8byte 18446744073709551617"
            ]
        )

tentativeIncompleteArrayTest :: Test
tentativeIncompleteArrayTest = TestLabel "Asm.Output.tentative-incomplete-array" $ TestCase $ do
    asm <- renderAsm "int x[]; int main() { x[0] = 1; return x[0]; }"
    assertContains
        "tentative incomplete arrays are materialized as one element before data emission"
        [ "x:"
        , ".zero 4"
        ]
        asm

tentativeIncompleteArrayDecayRetypeFallbackTest :: Test
tentativeIncompleteArrayDecayRetypeFallbackTest = TestLabel "Asm.Output.tentative-incomplete-array-decay-retype-fallback" $ TestCase $ do
    asm <- renderAsm "int x[]; int *f(void) { return x; } int main(void) { return 0; }"
    let fSection = extractFunctionSection "f" asm
    assertContainsInOrder
        "tentative arrays without a later completing declaration are retyped for decay-only codegen after fallback materialization"
        [ "f:"
        , "push offset x"
        , "pop rax"
        , "jmp .L.return.f"
        ]
        fSection
    assertBool
        "fallback-sized tentative-array decay sites should not load the first element as a scalar"
        (not $ "movsxd rax, dword ptr [rax]" `T.isInfixOf` fSection)

tentativeIncompleteArraySizeofFallbackTest :: Test
tentativeIncompleteArraySizeofFallbackTest = TestLabel "Asm.Output.tentative-incomplete-array-sizeof-fallback" $ TestCase $ do
    let incompleteTy :: CT.StorageClass Integer
        incompleteTy = CT.SCAuto $ CT.CTIncomplete (CT.IncompleteArray CT.CTInt)
        materializedTy :: CT.StorageClass Integer
        materializedTy = CT.SCAuto $ CT.CTArray 1 CT.CTInt
        gvars :: GlobalVars Integer
        gvars = Map.fromList [("x", GVar incompleteTy GVarInitWithZero 0)]
        sizeofExpr =
            ATNode
                ATSizeof
                (CT.SCAuto CT.CTInt)
                (ATNode (ATGVar incompleteTy "x") incompleteTy ATEmpty ATEmpty)
                ATEmpty
    case prepareAsmInput Map.empty [sizeofExpr] gvars of
        Left err ->
            assertFailure err
        Right ([ATNode ATSizeof _ (ATNode (ATGVar resolvedTy _) _ _ _) _], preparedGVars) -> do
            assertEqual
                "tentative incomplete arrays should materialize before revalidating sizeof"
                materializedTy
                resolvedTy
            assertEqual
                "prepareAsmInput should materialize tentative incomplete arrays in global storage too"
                (Just materializedTy)
                (gvtype <$> Map.lookup "x" preparedGVars)
        Right _ ->
            assertFailure "internal test error: prepareAsmInput returned an unexpected AST shape"

staticTentativeIncompleteArrayUseSiteRejectedTest :: Test
staticTentativeIncompleteArrayUseSiteRejectedTest = TestLabel "Asm.Output.static-tentative-incomplete-array-use-site-rejected" $ TestCase $
    assertBool
        "static tentative incomplete arrays should remain incomplete at expression use sites"
        (isLeft
            (runParser parser "<components>"
                "static int x[]; int main(void) { return sizeof x / sizeof x[0]; }"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

nestedTentativeIncompleteArrayUseSiteRejectedTest :: Test
nestedTentativeIncompleteArrayUseSiteRejectedTest = TestLabel "Asm.Output.nested-tentative-incomplete-array-use-site-rejected" $ TestCase $
    assertBool
        "address arithmetic on tentative incomplete arrays should be rejected before data emission"
        (isLeft
            (runParser parser "<components>"
                "int x[][4]; int main(void) { return ((char*)(&x + 1)) - ((char*)&x); }"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

tentativeNestedArrayExtentInferenceTest :: Test
tentativeNestedArrayExtentInferenceTest = TestLabel "Asm.Output.tentative-nested-array-extent-inference" $ TestCase $ do
    asm <- renderAsm "int x[][4]; int x[2][4]; int main(void) { return ((char*)(&x + 1)) - ((char*)&x); }"
    assertContains
        "same-translation-unit tentative nested arrays infer the missing outer extent from a later redeclaration"
        [ "x:"
        , ".zero 32"
        , "imul rdi, 32"
        ]
        asm

tentativeArrayEarlierFunctionDecayRetypeTest :: Test
tentativeArrayEarlierFunctionDecayRetypeTest = TestLabel "Asm.Output.tentative-array-earlier-function-decay-retype" $ TestCase $ do
    asm <- renderAsm "int x[]; int *f(void) { return x; } int x[4]; int main(void) { return 0; }"
    let fSection = extractFunctionSection "f" asm
    assertContainsInOrder
        "same-translation-unit tentative arrays retype earlier decay-only uses before codegen"
        [ "f:"
        , "push offset x"
        , "pop rax"
        , "jmp .L.return.f"
        ]
        fSection
    assertBool
        "retyped tentative-array decay sites should not load the first element as a scalar"
        (not $ "movsxd rax, dword ptr [rax]" `T.isInfixOf` fSection)

tentativeNestedArrayOuterExtentMergeUnitTest :: Test
tentativeNestedArrayOuterExtentMergeUnitTest = TestLabel "Asm.Output.tentative-nested-array-merge-unit" $ TestCase $ do
    let incompleteTy :: CT.TypeKind Integer
        incompleteTy = CT.CTIncomplete (CT.IncompleteArray (CT.CTArray 4 CT.CTInt))
        completeTy :: CT.TypeKind Integer
        completeTy = CT.CTArray 2 (CT.CTArray 4 CT.CTInt)
    assertEqual
        "tentative nested-array merging should infer only the missing outer extent"
        (Just completeTy)
        (CT.mergeTentativeArrayTypeKinds incompleteTy completeTy)
    assertEqual
        "tentative nested-array merging should be symmetric for the inferred outer extent"
        (Just completeTy)
        (CT.mergeTentativeArrayTypeKinds completeTy incompleteTy)

compatiblePointerArrayExtentCompletionMergeUnitTest :: Test
compatiblePointerArrayExtentCompletionMergeUnitTest = TestLabel "Asm.Output.compatible-pointer-array-extent-completion-merge-unit" $ TestCase $ do
    let incompletePtrTy :: CT.TypeKind Integer
        incompletePtrTy = CT.CTPtr $ CT.CTIncomplete (CT.IncompleteArray CT.CTInt)
        completePtrTy :: CT.TypeKind Integer
        completePtrTy = CT.CTPtr $ CT.CTArray 4 CT.CTInt
    assertEqual
        "pointer-compatible type merging should reject inferring an omitted array bound through a pointee"
        Nothing
        (CT.mergeCompatibleTypeKinds incompletePtrTy completePtrTy)
    assertEqual
        "pointer-compatible type merging should reject pointee-bound inference symmetrically"
        Nothing
        (CT.mergeCompatibleTypeKinds completePtrTy incompletePtrTy)

incompatiblePointerArrayExtentConflictMergeUnitTest :: Test
incompatiblePointerArrayExtentConflictMergeUnitTest = TestLabel "Asm.Output.incompatible-pointer-array-extent-conflict-merge-unit" $ TestCase $ do
    let lhsPtrTy :: CT.TypeKind Integer
        lhsPtrTy = CT.CTPtr $ CT.CTArray 3 CT.CTInt
        rhsPtrTy :: CT.TypeKind Integer
        rhsPtrTy = CT.CTPtr $ CT.CTArray 4 CT.CTInt
    assertEqual
        "pointer-compatible type merging should reject conflicting pointee array bounds"
        Nothing
        (CT.mergeCompatibleTypeKinds lhsPtrTy rhsPtrTy)
    assertEqual
        "pointer-compatible type merging should reject conflicting pointee array bounds symmetrically"
        Nothing
        (CT.mergeCompatibleTypeKinds rhsPtrTy lhsPtrTy)

compatibleFunctionParamRefinementMergeUnitTest :: Test
compatibleFunctionParamRefinementMergeUnitTest = TestLabel "Asm.Output.compatible-function-param-refinement-merge-unit" $ TestCase $ do
    let incompleteFnTy :: CT.TypeKind Integer
        incompleteFnTy =
            CT.CTFunc
                CT.CTInt
                [ (CT.CTPtr $ CT.CTIncomplete $ CT.IncompleteArray CT.CTInt, Nothing)
                ]
        refinedFnTy :: CT.TypeKind Integer
        refinedFnTy =
            CT.CTFunc
                CT.CTInt
                [ (CT.CTPtr $ CT.CTArray 4 CT.CTInt, Nothing)
                ]
    assertEqual
        "function-compatible type merging should reject omitted pointee bounds completed through pointer-to-array parameters"
        Nothing
        (CT.mergeCompatibleTypeKinds incompleteFnTy refinedFnTy)
    assertEqual
        "function-compatible type merging should reject pointer-to-array parameter bound inference symmetrically"
        Nothing
        (CT.mergeCompatibleTypeKinds refinedFnTy incompleteFnTy)

incompatibleFunctionParamArrayExtentConflictMergeUnitTest :: Test
incompatibleFunctionParamArrayExtentConflictMergeUnitTest = TestLabel "Asm.Output.incompatible-function-param-array-extent-conflict-merge-unit" $ TestCase $ do
    let lhsFnTy :: CT.TypeKind Integer
        lhsFnTy =
            CT.CTFunc
                CT.CTInt
                [ (CT.CTPtr $ CT.CTArray 3 CT.CTInt, Nothing)
                ]
        rhsFnTy :: CT.TypeKind Integer
        rhsFnTy =
            CT.CTFunc
                CT.CTInt
                [ (CT.CTPtr $ CT.CTArray 4 CT.CTInt, Nothing)
                ]
    assertEqual
        "function-compatible type merging should reject conflicting pointer-to-array parameter bounds"
        Nothing
        (CT.mergeCompatibleTypeKinds lhsFnTy rhsFnTy)
    assertEqual
        "function-compatible type merging should reject conflicting pointer-to-array parameter bounds symmetrically"
        Nothing
        (CT.mergeCompatibleTypeKinds rhsFnTy lhsFnTy)

compatibleTaggedStructCompletionMergeUnitTest :: Test
compatibleTaggedStructCompletionMergeUnitTest = TestLabel "Asm.Output.compatible-tagged-struct-completion-merge-unit" $ TestCase $ do
    let members :: Map.Map T.Text (CT.StructMember Integer)
        members = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        incompletePtrTy :: CT.TypeKind Integer
        incompletePtrTy = CT.CTPtr $ CT.CTIncomplete $ CT.IncompleteStruct "Foo" (CT.ScopeId 0)
        completePtrTy :: CT.TypeKind Integer
        completePtrTy = CT.CTPtr $ CT.CTNamedStruct "Foo" (CT.ScopeId 0) members
    assertEqual
        "pointer-compatible type merging should accept completion of a tagged opaque struct declaration"
        (Just completePtrTy)
        (CT.mergeCompatibleTypeKinds incompletePtrTy completePtrTy)
    assertEqual
        "pointer-compatible type merging should stay symmetric when the tagged struct definition appears first"
        (Just completePtrTy)
        (CT.mergeCompatibleTypeKinds completePtrTy incompletePtrTy)

incompatibleTaggedStructAliasMergeUnitTest :: Test
incompatibleTaggedStructAliasMergeUnitTest = TestLabel "Asm.Output.incompatible-tagged-struct-alias-merge-unit" $ TestCase $ do
    let members :: Map.Map T.Text (CT.StructMember Integer)
        members = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        fooPtrTy :: CT.TypeKind Integer
        fooPtrTy = CT.CTPtr $ CT.CTNamedStruct "Foo" (CT.ScopeId 0) members
        barPtrTy :: CT.TypeKind Integer
        barPtrTy = CT.CTPtr $ CT.CTNamedStruct "Bar" (CT.ScopeId 0) members
    assertEqual
        "pointer-compatible type merging should reject tagged structs that only match structurally"
        Nothing
        (CT.mergeCompatibleTypeKinds fooPtrTy barPtrTy)
    assertEqual
        "pointer-compatible type merging should reject structurally identical tagged structs symmetrically"
        Nothing
        (CT.mergeCompatibleTypeKinds barPtrTy fooPtrTy)

compatibleAnonymousStructMergeUnitTest :: Test
compatibleAnonymousStructMergeUnitTest = TestLabel "Asm.Output.compatible-anonymous-struct-merge-unit" $ TestCase $ do
    let lhsMembers :: Map.Map T.Text (CT.StructMember Integer)
        lhsMembers = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        rhsMembers :: Map.Map T.Text (CT.StructMember Integer)
        rhsMembers = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        lhsPtrTy :: CT.TypeKind Integer
        lhsPtrTy = CT.CTPtr $ CT.CTStruct lhsMembers
        rhsPtrTy :: CT.TypeKind Integer
        rhsPtrTy = CT.CTPtr $ CT.CTStruct rhsMembers
    assertEqual
        "pointer-compatible type merging should accept anonymous structs that match structurally"
        (Just rhsPtrTy)
        (CT.mergeCompatibleTypeKinds lhsPtrTy rhsPtrTy)
    assertEqual
        "pointer-compatible type merging should accept structurally identical anonymous structs symmetrically"
        (Just rhsPtrTy)
        (CT.mergeCompatibleTypeKinds rhsPtrTy lhsPtrTy)

compatibleNamedStructAnonymousMemberMergeUnitTest :: Test
compatibleNamedStructAnonymousMemberMergeUnitTest = TestLabel "Asm.Output.compatible-named-struct-anonymous-member-merge-unit" $ TestCase $ do
    let lhsAnonMembers :: Map.Map T.Text (CT.StructMember Integer)
        lhsAnonMembers = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        rhsAnonMembers :: Map.Map T.Text (CT.StructMember Integer)
        rhsAnonMembers = Map.fromList
            [ ("value", CT.StructMember CT.CTInt 0)
            ]
        lhsOuterMembers :: Map.Map T.Text (CT.StructMember Integer)
        lhsOuterMembers = Map.fromList
            [ ("anon", CT.StructMember (CT.CTStruct lhsAnonMembers) 0)
            ]
        rhsOuterMembers :: Map.Map T.Text (CT.StructMember Integer)
        rhsOuterMembers = Map.fromList
            [ ("anon", CT.StructMember (CT.CTStruct rhsAnonMembers) 0)
            ]
        lhsTy :: CT.TypeKind Integer
        lhsTy = CT.CTNamedStruct "Outer" (CT.ScopeId 0) lhsOuterMembers
        rhsTy :: CT.TypeKind Integer
        rhsTy = CT.CTNamedStruct "Outer" (CT.ScopeId 0) rhsOuterMembers
    assertEqual
        "struct-compatible type merging should accept named structs whose anonymous member structs match structurally"
        (Just rhsTy)
        (CT.mergeCompatibleTypeKinds lhsTy rhsTy)
    assertEqual
        "struct-compatible type merging should accept nested anonymous member structs symmetrically"
        (Just rhsTy)
        (CT.mergeCompatibleTypeKinds rhsTy lhsTy)

tentativeNestedArrayMaterializationUnitTest :: Test
tentativeNestedArrayMaterializationUnitTest = TestLabel "Asm.Output.tentative-nested-array-materialization-unit" $ TestCase $ do
    let gvar :: GVar Integer
        gvar =
            GVar
                { gvtype = CT.SCAuto $ CT.CTIncomplete (CT.IncompleteArray (CT.CTArray 4 CT.CTInt))
                , initWith = GVarInitWithZero
                , gvNestDepth = 0
                }
        expectedTy :: CT.StorageClass Integer
        expectedTy = CT.SCAuto $ CT.CTArray 4 (CT.CTArray 1 CT.CTInt)
    assertEqual
        "tentative nested-array globals should materialize one outer row before codegen"
        expectedTy
        (gvtype $ materializeTentativeIncompleteArray gvar)

tentativeArrayUseSiteRejectedTest :: Test
tentativeArrayUseSiteRejectedTest = TestLabel "Asm.Output.tentative-array-use-site-rejected" $ TestCase $
    assertBool
        "later global completions must not retroactively legitimize earlier sizeof uses"
        (isLeft
            (runParser parser "<components>"
                "int x[]; int main(void) { return sizeof x; } int x[4];"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

tentativeArrayAddressUseSiteRejectedTest :: Test
tentativeArrayAddressUseSiteRejectedTest = TestLabel "Asm.Output.tentative-array-address-use-site-rejected" $ TestCase $
    assertBool
        "later global completions must not retroactively legitimize earlier address arithmetic"
        (isLeft
            (runParser parser "<components>"
                "int x[]; int main(void) { return ((char*)(&x + 1)) - ((char*)&x); } int x[4];"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

globalInitializerRelocAddendTest :: Test
globalInitializerRelocAddendTest = TestLabel "Asm.Output.global-initializer-reloc-addend" $ TestCase $ do
    asm <- renderAsm "int a[4]; int *p = a + (1 + 1); char *end = (char*)(&a + 1); char *q = \"ab\" + 1; void *self = &self; int main() { return p != 0 && end != 0 && q[0] == 'b' && self != 0; }"
    assertContains
        "global pointer initializers retain compatible self-references and folded symbol addends in emitted relocations"
        [ "a:"
        , ".zero 16"
        , "p:"
        , ".quad a+8"
        , "end:"
        , ".quad a+16"
        , "q:"
        , ".quad .L.data.0+1"
        , "self:"
        , ".quad self"
        ]
        asm

globalInitializerArraySubobjectRelocTest :: Test
globalInitializerArraySubobjectRelocTest = TestLabel "Asm.Output.global-initializer-array-subobject-reloc" $ TestCase $ do
    asm <- renderAsm "int x[2][4]; int *row0 = x[0]; int *first = &x[0][0]; int *row1 = x[1]; int main(void) { return row0 == &x[0][0] && first == &x[0][0] && row1 == &x[1][0]; }"
    assertContains
        "global pointer initializers fold array subobjects reached through dereferences into relocations"
        [ "x:"
        , ".zero 32"
        , "row0:"
        , ".quad x"
        , "first:"
        , ".quad x"
        , "row1:"
        , ".quad x+16"
        ]
        asm

globalInitializerTentativeNestedArrayFallbackRelocTest :: Test
globalInitializerTentativeNestedArrayFallbackRelocTest = TestLabel "Asm.Output.global-initializer-tentative-nested-array-fallback-reloc" $ TestCase $ do
    asm <- renderAsm "int x[][4]; char *p = (char*)(x + 1); int main(void) { return p == ((char*)x) + 16; }"
    assertContains
        "tentative nested arrays should materialize before folding global initializer relocations"
        [ "x:"
        , ".zero 16"
        , "p:"
        , ".quad x+16"
        ]
        asm

globalInitializerTentativeArrayUseSiteRejectedTest :: Test
globalInitializerTentativeArrayUseSiteRejectedTest = TestLabel "Asm.Output.global-initializer-tentative-array-use-site-rejected" $ TestCase $
    assertBool
        "earlier global initializers must not be retyped from later tentative-array completions"
        (isLeft
            (runParser parser "<components>"
                "int x[]; int y = sizeof x; char *p = (char*)(&x + 1); int x[4]; int main(void) { return y == 16 && p == ((char*)&x) + 16; }"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

globalInitializerSelfRetypeSizeofTest :: Test
globalInitializerSelfRetypeSizeofTest = TestLabel "Asm.Output.global-initializer-self-retype-sizeof" $ TestCase $ do
    asm <- renderAsm "int x[]; int x[4] = { sizeof x }; int main(void) { return x[0] == 16; }"
    assertContains
        "completed array definitions retype self-referential sizeof expressions before folding the defining initializer"
        [ "x:"
        , ".4byte 16"
        , ".zero 12"
        ]
        asm

globalInitializerSelfRetypeAddressTest :: Test
globalInitializerSelfRetypeAddressTest = TestLabel "Asm.Output.global-initializer-self-retype-address" $ TestCase $ do
    asm <- renderAsm "char *x[]; char *x[4] = { (char*)(&x + 1) }; int main(void) { return x[0] == ((char*)&x) + 32; }"
    assertContains
        "completed array definitions retype self-referential address arithmetic before folding the defining initializer"
        [ "x:"
        , ".quad x+32"
        , ".zero 24"
        ]
        asm

globalInitializerFunctionRelocTest :: Test
globalInitializerFunctionRelocTest = TestLabel "Asm.Output.global-initializer-function-reloc" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 1; } int (*fp)(void) = foo; int (*fq)(void) = &foo; int main(void) { return fp != 0 && fq != 0; }"
    assertContains
        "file-scope function-pointer initializers emit relocations for bare and addressed function designators"
        [ "foo:"
        , "fp:"
        , ".quad foo"
        , "fq:"
        , ".quad foo"
        ]
        asm

bareFunctionDesignatorDerefCallTest :: Test
bareFunctionDesignatorDerefCallTest = TestLabel "Asm.Output.bare-function-designator-deref-call" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 1; } int main(void) { return (*foo)(); }"
    assertContains
        "dereferencing a bare function designator remains callable"
        [ "foo:"
        , "main:"
        , "call foo"
        ]
        asm

functionPointerArrayZeroInitializerTest :: Test
functionPointerArrayZeroInitializerTest = TestLabel "Asm.Output.function-pointer-array-zero-initializer" $ TestCase $ do
    asm <- renderAsm "int (*fps[2])(void) = { 0, 0 }; int main(void) { return fps[0] != 0 || fps[1] != 0; }"
    assertContains
        "brace initializers keep scanning all zeroed function-pointer array elements"
        [ "fps:"
        , ".zero 16"
        ]
        asm

functionPointerArrayFunctionInitializerTest :: Test
functionPointerArrayFunctionInitializerTest = TestLabel "Asm.Output.function-pointer-array-function-initializer" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 1; } int (*fps[2])(void) = { foo, &foo }; int main(void) { return fps[0]() != 1 || fps[1]() != 1; }"
    assertContains
        "brace initializers keep scanning all function-pointer array elements"
        [ "foo:"
        , "fps:"
        ]
        asm
    assertEqual
        "each function-pointer initializer should emit its own relocation"
        2
        (T.count ".quad foo" asm)

omittedBoundArrayPointerDerefDecayTest :: Test
omittedBoundArrayPointerDerefDecayTest = TestLabel "Asm.Output.omitted-bound-array-pointer-deref-decay" $ TestCase $ do
    asm <- renderAsm "int *f(int (*p)[]) { return *p; } int main(void) { int x[4]; int (*p)[] = (int (*)[])&x; return f(p) != x; }"
    let fSection = extractFunctionSection "f" asm
    assertEqual
        "dereferencing a pointer-to-omitted-bound-array should keep the array lvalue and decay it without an extra scalar load"
        1
        (T.count "mov rax, [rax]" fSection)

indirectFunctionPointerCallTest :: Test
indirectFunctionPointerCallTest = TestLabel "Asm.Output.indirect-function-pointer-call" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 1; } int main(void) { int (*fp)(void) = foo; return fp(); }"
    assertContains
        "callable variables are lowered as indirect calls through the loaded function pointer"
        [ "call r11" ]
        asm
    assertBool
        "callable variables must not be emitted as implicit direct symbol calls"
        (not $ "call \"fp\"" `T.isInfixOf` asm)

indirectFunctionPointerCallAlignmentTest :: Test
indirectFunctionPointerCallAlignmentTest = TestLabel "Asm.Output.indirect-function-pointer-call-alignment" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 1; } int main(void) { int (*fp)(void); fp = foo; return fp(); }"
    assertContainsInOrder
        "zero-argument indirect calls load the callee once and keep a padded fallback path"
        [ "pop r11"
        , "mov rax, rsp"
        , "and rax, 15"
        , "jnz .L.call."
        , "mov rax, 0"
        , "call r11"
        , ".L.call."
        , "sub rsp, 8"
        , "mov rax, 0"
        , "call r11"
        , "add rsp, 8"
        ]
        asm

directBoolFunctionCallNormalizationTest :: Test
directBoolFunctionCallNormalizationTest = TestLabel "Asm.Output.direct-bool-function-call-normalization" $ TestCase $ do
    asm <- renderAsm "_Bool foo(void) { return 2; } int main(void) { return foo(); }"
    assertContainsInOrder
        "zero-argument direct _Bool calls normalize the ABI-defined low byte before pushing the result"
        [ "call foo"
        , "cmp al, 0"
        , "setne al"
        , "movzb rax, al"
        , "push rax"
        ]
        asm

indirectBoolFunctionPointerCallNormalizationTest :: Test
indirectBoolFunctionPointerCallNormalizationTest = TestLabel "Asm.Output.indirect-bool-function-pointer-call-normalization" $ TestCase $ do
    asm <- renderAsm "_Bool foo(void) { return 2; } int main(void) { _Bool (*fp)(void); fp = foo; return fp(); }"
    assertContainsInOrder
        "zero-argument indirect _Bool calls normalize the ABI-defined low byte before pushing the result"
        [ "call r11"
        , "cmp al, 0"
        , "setne al"
        , "movzb rax, al"
        , "push rax"
        ]
        asm

directIntegralFunctionCallNormalizationTest :: Test
directIntegralFunctionCallNormalizationTest = TestLabel "Asm.Output.direct-integral-function-call-normalization" $ TestCase $ do
    asm <- renderAsm "char ret_char(void) { return -1; } short ret_short(void) { return -1; } int ret_int(void) { return -1; } int main(void) { return ret_char() == -1 && ret_short() == -1 && ret_int() == -1; }"
    let mainSection = extractFunctionSection "main" asm
    assertContainsInOrder
        "direct char/short/int calls truncate the ABI return register before the result is consumed"
        [ "call ret_char"
        , "movsx rax, al"
        , "push rax"
        , "call ret_short"
        , "movsx rax, ax"
        , "push rax"
        , "call ret_int"
        , "movsxd rax, eax"
        , "push rax"
        ]
        mainSection

blockScopeExternObjectShadowsOuterLocalAsmTest :: Test
blockScopeExternObjectShadowsOuterLocalAsmTest = TestLabel "Asm.Output.block-scope-extern-object-shadows-outer-local" $ TestCase $ do
    asm <- renderAsm "int foo = 2; int main(void) { int foo = 1; { extern int foo; return foo; } }"
    let mainSection = extractFunctionSection "main" asm
    assertContains
        "block-scope extern object references the global symbol instead of the outer local"
        [ "push offset foo"
        ]
        mainSection

blockScopeExternObjectShadowsEnumeratorAsmTest :: Test
blockScopeExternObjectShadowsEnumeratorAsmTest = TestLabel "Asm.Output.block-scope-extern-object-shadows-enumerator" $ TestCase $ do
    asm <- renderAsm "enum E { A = 5 }; int f(void) { extern int A; return A; }"
    let fSection = extractFunctionSection "f" asm
    assertContains
        "block-scope extern object resolves to a global symbol load instead of folding the enumerator"
        [ "push offset A"
        ]
        fSection
    assertBool
        "block-scope extern object should not fold to the outer enumerator constant"
        (not $ "push 5" `T.isInfixOf` fSection)

blockScopeExternFunctionShadowsEnumeratorAsmTest :: Test
blockScopeExternFunctionShadowsEnumeratorAsmTest = TestLabel "Asm.Output.block-scope-extern-function-shadows-enumerator" $ TestCase $ do
    asm <- renderAsm "enum E { foo = 1 }; int f(void) { extern int foo(void); return foo(); }"
    let fSection = extractFunctionSection "f" asm
    assertContains
        "block-scope extern prototype resolves to a function call instead of the outer enumerator"
        [ "call foo"
        ]
        fSection

blockScopeExternStaticFunctionAsmTest :: Test
blockScopeExternStaticFunctionAsmTest = TestLabel "Asm.Output.block-scope-extern-static-function" $ TestCase $ do
    asm <- renderAsm "static int foo(void) { return 3; } int main(void) { extern int foo(void); return foo(); }"
    let mainSection = extractFunctionSection "main" asm
    assertContains
        "block-scope extern prototypes inherit visible static function linkage"
        [ "call foo"
        ]
        mainSection

blockScopeExternStaticObjectAsmTest :: Test
blockScopeExternStaticObjectAsmTest = TestLabel "Asm.Output.block-scope-extern-static-object" $ TestCase $ do
    asm <- renderAsm "static int x = 4; int main(void) { extern int x; return x; }"
    let mainSection = extractFunctionSection "main" asm
    assertContains
        "block-scope extern objects inherit visible static object linkage"
        [ "push offset x"
        ]
        mainSection

indirectIntegralFunctionPointerCallNormalizationTest :: Test
indirectIntegralFunctionPointerCallNormalizationTest = TestLabel "Asm.Output.indirect-integral-function-pointer-call-normalization" $ TestCase $ do
    asm <- renderAsm "char ret_char(void) { return -1; } short ret_short(void) { return -1; } int ret_int(void) { return -1; } int main(void) { char (*char_fp)(void); short (*short_fp)(void); int (*int_fp)(void); char_fp = ret_char; short_fp = ret_short; int_fp = ret_int; return char_fp() == -1 && short_fp() == -1 && int_fp() == -1; }"
    let mainSection = extractFunctionSection "main" asm
    assertContainsInOrder
        "indirect char/short/int calls truncate the ABI return register before the result is consumed"
        [ "call r11"
        , "movsx rax, al"
        , "push rax"
        , "call r11"
        , "movsx rax, ax"
        , "push rax"
        , "call r11"
        , "movsxd rax, eax"
        , "push rax"
        ]
        mainSection

boolFunctionReturnNormalizationTest :: Test
boolFunctionReturnNormalizationTest = TestLabel "Asm.Output.bool-function-return-normalization" $ TestCase $ do
    asm <- renderAsm "_Bool foo(void) { return 256; } int main(void) { return foo(); }"
    assertContainsInOrder
        "bool function epilogues normalize the full return register at the shared return label"
        [ ".L.return.foo:"
        , "cmp rax, 0"
        , "setne al"
        , "movzb rax, al"
        , "leave"
        , "ret"
        ]
        asm

directBoolFunctionArgNormalizationTest :: Test
directBoolFunctionArgNormalizationTest = TestLabel "Asm.Output.direct-bool-function-arg-normalization" $ TestCase $ do
    asm <- renderAsm "int takes_bool(_Bool x) { return x; } int main(void) { return takes_bool(256); }"
    let mainSection = extractFunctionSection "main" asm
    assertContainsInOrder
        "direct calls cast integer arguments to _Bool before materializing the argument register from the scratch slot"
        [ "main:"
        , "push 256"
        , "cmp rax, 0"
        , "setne al"
        , "movzb rax, al"
        , "mov [rbx+0], rdx"
        , "mov rdi, [rax+0]"
        , "call takes_bool"
        ]
        mainSection

directOldStyleBoolFunctionArgPromotionTest :: Test
directOldStyleBoolFunctionArgPromotionTest = TestLabel "Asm.Output.direct-old-style-bool-function-arg-promotion" $ TestCase $ do
    asm <- renderAsm "int takes_bool(); int main(void) { return takes_bool(256); }"
    let mainSection = extractFunctionSection "main" asm
    assertContainsInOrder
        "old-style direct calls pass the promoted integer argument through the scratch slot without _Bool normalization"
        [ "main:"
        , "push 256"
        , "mov [rbx+0], rdx"
        , "mov rdi, [rax+0]"
        , "call takes_bool"
        ]
        mainSection
    assertBool
        "old-style direct calls must not normalize the argument to _Bool at the call site"
        (not $ any (`T.isInfixOf` mainSection) ["cmp rax, 0", "setne al", "movzb rax, al"])

indirectBoolFunctionPointerArgNormalizationTest :: Test
indirectBoolFunctionPointerArgNormalizationTest = TestLabel "Asm.Output.indirect-bool-function-pointer-arg-normalization" $ TestCase $ do
    asm <- renderAsm "int takes_bool(_Bool x) { return x; } int main(void) { int (*fp)(_Bool); fp = takes_bool; return fp(256); }"
    let mainSection = extractFunctionSection "main" asm
    assertContainsInOrder
        "indirect calls cast integer arguments to _Bool before materializing the argument and callee from scratch slots"
        [ "main:"
        , "push 256"
        , "cmp rax, 0"
        , "setne al"
        , "movzb rax, al"
        , "mov [rbx+0], rdx"
        , "mov rdi, [rax+0]"
        , "mov r11, [rax+8]"
        , "mov rax, 0"
        , "call r11"
        ]
        mainSection

indirectOldStyleBoolFunctionPointerPromotionConflictTest :: Test
indirectOldStyleBoolFunctionPointerPromotionConflictTest = TestLabel "Asm.Output.indirect-old-style-bool-function-pointer-promotion-conflict" $ TestCase $ do
    assertBool
        "old-style function pointers must reject _Bool parameters that only match after default promotions"
        (isLeft
            ( runParser parser "<components>"
                "int takes_bool(_Bool x) { return x; } int main(void) { int (*fp)(); fp = takes_bool; return fp(256); }"
                :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

indirectFunctionPointerArgAlignmentTest :: Test
indirectFunctionPointerArgAlignmentTest = TestLabel "Asm.Output.indirect-function-pointer-arg-alignment" $ TestCase $ do
    asm <- renderAsm "int inc(int x) { return x + 1; } int main(void) { int (*fp)(int); fp = inc; return fp(41); }"
    assertContainsInOrder
        "indirect calls with register arguments probe rsp alignment before restoring the argument and callee from scratch slots"
        [ "mov rax, rsp"
        , "and rax, 15"
        , "jnz .L.call."
        , "mov rdi, [rax+0]"
        , "mov r11, [rax+8]"
        , "mov rax, 0"
        , "call r11"
        ]
        asm

directFunctionStackArgAlignmentTest :: Test
directFunctionStackArgAlignmentTest = TestLabel "Asm.Output.direct-function-stack-arg-alignment" $ TestCase $ do
    asm <- renderAsm "long sum7(long a, long b, long c, long d, long e, long f, long g) { return a + b + c + d + e + f + g; } int main(void) { return sum7(1, 2, 3, 4, 5, 6, 7) - 28; }"
    assertContainsInOrder
        "stack-passed direct calls probe the final call alignment before preparing args and keep any padding ahead of outgoing stack arguments"
        [ "mov rax, rsp"
        , "sub rax, 8"
        , "and rax, 15"
        , "jnz .L.call."
        , "push [rax+48]"
        , "call sum7"
        , ".L.call."
        , "sub rsp, 8"
        , "push [rax+48]"
        , "call sum7"
        , "add rsp, 8"
        , "add rsp, 8"
        ]
        asm

directFunctionLateStackArgCallOrderTest :: Test
directFunctionLateStackArgCallOrderTest = TestLabel "Asm.Output.direct-function-late-stack-arg-call-order" $ TestCase $ do
    asm <- renderAsm "long g(void) { return 7; } long sum8(long a, long b, long c, long d, long e, long f, long g_, long h) { return g_ + h; } int main(void) { return sum8(1, 2, 3, 4, 5, 6, g(), 8) - 15; }"
    let mainSection = extractFunctionSection "main" asm
    assertOccursBefore
        "later stack arguments should not be pushed before evaluating an earlier stack argument that contains a nested call"
        "call g"
        "push 8"
        mainSection

stackPassedParameterSpillTest :: Test
stackPassedParameterSpillTest = TestLabel "Asm.Output.stack-passed-parameter-spill" $ TestCase $ do
    asm <- renderAsm "long last7(long a, long b, long c, long d, long e, long f, long g) { return g; } int main(void) { return last7(1, 2, 3, 4, 5, 6, 7) - 7; }"
    assertContainsInOrder
        "function prologues spill stack-passed parameters into their local slots before use"
        [ "last7:"
        , "mov rax, [rbp+16]"
        , "mov [rbp-56], rax"
        ]
        asm

stackPassedBoolParameterSpillTest :: Test
stackPassedBoolParameterSpillTest = TestLabel "Asm.Output.stack-passed-bool-parameter-spill" $ TestCase $ do
    asm <- renderAsm "int bool7(int a, int b, int c, int d, int e, int f, _Bool g) { return g; } int main(void) { return bool7(1, 2, 3, 4, 5, 6, 256); }"
    assertContainsInOrder
        "stack-passed _Bool parameters normalize the ABI-defined low byte before spilling into the local slot"
        [ "bool7:"
        , "mov rax, [rbp+16]"
        , "cmp al, 0"
        , "setne al"
        , "movzb rax, al"
        , "mov [rbp-"
        ]
        asm

writeOnlyFallbackReplacementRestoreTest :: Test
writeOnlyFallbackReplacementRestoreTest =
    replacementFailurePreservesWriteOnlyOutputTest
        PreserveReplacementOutputMode
        "Asm.Output.write-only-fallback-replacement-restore"
        ownerWriteMode

writeOnlyExecutableFallbackReplacementRestoreTest :: Test
writeOnlyExecutableFallbackReplacementRestoreTest =
    replacementFailurePreservesWriteOnlyOutputTest
        PreserveReplacementOutputModeKeepingExecutableBits
        "Asm.Output.write-only-executable-fallback-replacement-restore"
        (ownerWriteMode `unionFileModes` ownerExecuteMode)

unreadableStagedFallbackReplacementTest :: Test
unreadableStagedFallbackReplacementTest =
    TestLabel "Asm.Output.unreadable-staged-fallback-replacement" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeFile stagedPath)
            existingMode = 0o555
            stagedMode = 0o055
            replacementOutput = "#!/bin/sh\nexit 0\n"
            copyReplacementOutput src dst =
                B.readFile src >>= B.writeFile dst
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "#!/bin/sh\nexit 99\n"
            setFileMode targetPath existingMode
            T.writeFile stagedPath replacementOutput
            setFileMode stagedPath stagedMode
            replaceExistingOutputFromPathWith
                copyReplacementOutput
                PreserveReplacementOutputModeKeepingExecutableBits
                targetPath
                existingMode
                stagedMode
                stagedPath
            replacedMode <- fileMode <$> getFileStatus targetPath
            setFileMode targetPath $ replacedMode `unionFileModes` ownerReadMode
            replacedOutput <- T.readFile targetPath
            assertEqual
                "fallback replacement should temporarily restore owner read on unreadable staged outputs"
                existingMode
                (intersectFileModes replacedMode 0o777)
            assertEqual "fallback replacement should copy the staged output" replacementOutput replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

rollbackFailureSurfacedTest :: Test
rollbackFailureSurfacedTest =
    TestLabel "Asm.Output.rollback-failure-surfaced" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile stagedPath)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeDirectory targetPath)
            existingMode = ownerReadMode `unionFileModes` ownerWriteMode
            stagedMode = ownerReadMode `unionFileModes` ownerWriteMode
            failDuringReplacement src dst
                | src == stagedPath = do
                    withBinaryFile src ReadMode $ \srcHandle ->
                        withBinaryFile dst WriteMode $ \dstHandle -> do
                            chunk <- B.hGetSome srcHandle 4
                            B.hPut dstHandle chunk
                    removeFile dst
                    createDirectory dst
                    ioError $ userError "simulated replacement failure"
                | otherwise =
                    assertFailure "unexpected replacement source path"
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "stale output\n"
            setFileMode targetPath existingMode
            T.writeFile stagedPath "replacement output\n"
            result <- try
                (replaceExistingOutputFromPathWith
                    failDuringReplacement
                    PreserveReplacementOutputMode
                    targetPath
                    existingMode
                    stagedMode
                    stagedPath
                )
                :: IO (Either IOException ())
            case result of
                Left ioErr -> do
                    let errText = T.pack $ show ioErr
                    assertBool
                        "rollback failures should be surfaced to the caller"
                        ("failed to restore original output after replacement failure" `T.isInfixOf` errText)
                    assertBool
                        "the surfaced error should retain the original replacement failure"
                        ("simulated replacement failure" `T.isInfixOf` errText)
                Right _ ->
                    assertFailure "replacement should fail when both replacement and rollback fail"
            targetIsDirectory <- doesDirectoryExist targetPath
            assertBool
                "the failed rollback fixture should leave the destination in its mutated state"
                targetIsDirectory
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

executableOnlyFallbackReplacementPreservesModeTest :: Test
executableOnlyFallbackReplacementPreservesModeTest =
    TestLabel "Asm.Output.executable-only-fallback-replacement-preserves-mode" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeFile stagedPath)
            existingMode = 0o555
            currentMode = 0o755
            replacementOutput = "#!/bin/sh\nexit 0\n"
            copyReplacementOutput src dst =
                B.readFile src >>= B.writeFile dst
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "#!/bin/sh\nexit 99\n"
            setFileMode targetPath existingMode
            T.writeFile stagedPath replacementOutput
            setFileMode stagedPath currentMode
            replaceExistingOutputFromPathWith
                copyReplacementOutput
                PreserveReplacementOutputModeKeepingExecutableBits
                targetPath
                existingMode
                currentMode
                stagedPath
            replacedMode <- fileMode <$> getFileStatus targetPath
            replacedOutput <- T.readFile targetPath
            assertEqual
                "fallback replacement should temporarily make executable-only outputs writable and then restore the original mode"
                existingMode
                (intersectFileModes replacedMode 0o777)
            assertEqual "fallback replacement should copy the staged output" replacementOutput replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

replacementExecutableBitsIgnoreReadBitsTest :: Test
replacementExecutableBitsIgnoreReadBitsTest =
    TestLabel "Asm.Output.replacement-executable-bits-ignore-read-bits" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeFile stagedPath)
            existingMode = 0o640
            currentMode = 0o755
            expectedMode = 0o740
            copyReplacementOutput src dst =
                B.readFile src >>= B.writeFile dst
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "stale output\n"
            setFileMode targetPath existingMode
            T.writeFile stagedPath "#!/bin/sh\nexit 0\n"
            replaceExistingOutputFromPathWith
                copyReplacementOutput
                PreserveReplacementOutputModeKeepingExecutableBits
                targetPath
                existingMode
                currentMode
                stagedPath
            replacedMode <- fileMode <$> getFileStatus targetPath
            replacedOutput <- T.readFile targetPath
            assertEqual
                "replacement should preserve the prior permission mask and add only owner execute"
                expectedMode
                (intersectFileModes replacedMode 0o777)
            assertEqual "replacement should copy the staged output" "#!/bin/sh\nexit 0\n" replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

replacementExecutableBitsRestoreOwnerExecuteTest :: Test
replacementExecutableBitsRestoreOwnerExecuteTest =
    TestLabel "Asm.Output.replacement-executable-bits-restore-owner-execute" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeFile stagedPath)
            existingMode = 0o055
            currentMode = 0o755
            expectedMode = 0o155
            replacementOutput = "#!/bin/sh\nexit 0\n"
            copyReplacementOutput src dst =
                B.readFile src >>= B.writeFile dst
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "#!/bin/sh\nexit 99\n"
            setFileMode targetPath existingMode
            T.writeFile stagedPath replacementOutput
            setFileMode stagedPath currentMode
            replaceExistingOutputFromPathWith
                copyReplacementOutput
                PreserveReplacementOutputModeKeepingExecutableBits
                targetPath
                existingMode
                currentMode
                stagedPath
            replacedMode <- fileMode <$> getFileStatus targetPath
            setFileMode targetPath $ replacedMode `unionFileModes` ownerReadMode
            replacedOutput <- T.readFile targetPath
            assertEqual
                "replacement should restore owner execute when the prior execute mask only covered group/other"
                expectedMode
                (intersectFileModes replacedMode 0o777)
            assertEqual "replacement should copy the staged output" replacementOutput replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

freshExecutableReplacementPreservesExecuteBitsTest :: Test
freshExecutableReplacementPreservesExecuteBitsTest =
    TestLabel "Asm.Output.fresh-executable-replacement-preserves-execute-bits" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        creationMode <- creationMaskedOutputMode
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (removeFile targetPath)
            expectedMode = intersectFileModes creationMode 0o777 `unionFileModes` 0o111
        flip finally cleanup $ do
            hClose targetHandle
            removeFile targetPath
            withReplacementOutputPath PreserveReplacementOutputModeKeepingExecutableBits targetPath $ \tmpOutputPath -> do
                T.writeFile tmpOutputPath "#!/bin/sh\nexit 0\n"
                setFileMode tmpOutputPath 0o755
            replacedMode <- fileMode <$> getFileStatus targetPath
            replacedOutput <- T.readFile targetPath
            assertEqual
                "fresh executable replacements should preserve all execute bits emitted by the linker"
                expectedMode
                (intersectFileModes replacedMode 0o777)
            assertEqual "fresh replacement should write the staged output" "#!/bin/sh\nexit 0\n" replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

freshExecutableReplacementRestoresOwnerExecuteTest :: Test
freshExecutableReplacementRestoresOwnerExecuteTest =
    TestLabel "Asm.Output.fresh-executable-replacement-restores-owner-execute" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        let cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (removeFile targetPath)
            currentMode = 0o055
        flip finally cleanup $ do
            hClose targetHandle
            removeFile targetPath
            withReplacementOutputPath PreserveReplacementOutputModeKeepingExecutableBits targetPath $ \tmpOutputPath -> do
                T.writeFile tmpOutputPath "#!/bin/sh\nexit 0\n"
                setFileMode tmpOutputPath currentMode
            replacedMode <- fileMode <$> getFileStatus targetPath
            replacedOutput <- T.readFile targetPath
            assertBool
                "fresh executable replacements should restore owner execute when the linker only leaves group/other execute"
                (intersectFileModes replacedMode ownerExecuteMode /= 0)
            assertEqual
                "fresh executable replacements should preserve the linker-provided group/other execute mask"
                currentMode
                (intersectFileModes replacedMode currentMode)
            assertEqual "fresh replacement should write the staged output" "#!/bin/sh\nexit 0\n" replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

creationMaskedOutputModeMatchesActualCreationTest :: Test
creationMaskedOutputModeMatchesActualCreationTest =
    TestLabel "Asm.Output.creation-masked-output-mode-matches-actual-creation" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        probeDir <- mkdtemp (tmpDir <> "/htcc-output-modeXXXXXX")
        let probePath = probeDir <> "/mask-probe"
            cleanup =
                ignoreIOException (removeFile probePath)
                    >> ignoreIOException (removeDirectory probeDir)
        flip finally cleanup $ do
            expectedMode <- creationMaskedOutputMode
            probeFd <- createFile probePath 0o666
            finally
                ( do
                    actualMode <- intersectFileModes 0o666 . fileMode <$> getFileStatus probePath
                    assertEqual
                        "creationMaskedOutputMode should match the mode that POSIX file creation actually receives"
                        expectedMode
                        actualMode
                )
                (closeFd probeFd)
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

hardLinkedFallbackReplacementRejectedTest :: Test
hardLinkedFallbackReplacementRejectedTest =
    TestLabel "Asm.Output.hard-linked-fallback-replacement-rejected" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        (stagedPath, stagedHandle) <- openTempFile tmpDir "htcc-output-staged"
        let aliasPath = targetPath <> ".alias"
            cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (hClose stagedHandle)
                    >> ignoreIOException (removeFile aliasPath)
                    >> ignoreIOException (removeFile targetPath)
                    >> ignoreIOException (removeFile stagedPath)
            existingMode = 0o644
            currentMode = 0o644
            copyReplacementOutput src dst =
                B.readFile src >>= B.writeFile dst
        flip finally cleanup $ do
            hClose targetHandle
            hClose stagedHandle
            T.writeFile targetPath "stale output\n"
            createLink targetPath aliasPath
            setFileMode targetPath existingMode
            T.writeFile stagedPath "replacement output\n"
            result <- try
                (replaceExistingOutputFromPathWith
                    copyReplacementOutput
                    PreserveReplacementOutputMode
                    targetPath
                    existingMode
                    currentMode
                    stagedPath
                )
                :: IO (Either IOException ())
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "fallback replacement should reject hard-linked outputs"
            targetContents <- T.readFile targetPath
            aliasContents <- T.readFile aliasPath
            assertEqual "target should remain unchanged" "stale output\n" targetContents
            assertEqual "hard-linked alias should remain unchanged" "stale output\n" aliasContents
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

hardLinkedRenameReplacementPreservesAliasTest :: Test
hardLinkedRenameReplacementPreservesAliasTest =
    TestLabel "Asm.Output.hard-linked-rename-replacement-preserves-alias" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        (targetPath, targetHandle) <- openTempFile tmpDir "htcc-output-target"
        let aliasPath = targetPath <> ".alias"
            cleanup =
                ignoreIOException (hClose targetHandle)
                    >> ignoreIOException (removeFile aliasPath)
                    >> ignoreIOException (removeFile targetPath)
            staleOutput = "stale output\n"
            replacementOutput = "replacement output\n"
        flip finally cleanup $ do
            hClose targetHandle
            T.writeFile targetPath staleOutput
            createLink targetPath aliasPath
            result <- try
                (withReplacementOutputPath PreserveReplacementOutputMode targetPath $ \tmpOutputPath ->
                    T.writeFile tmpOutputPath replacementOutput
                )
                :: IO (Either IOException ())
            case result of
                Left ioErr ->
                    assertFailure $
                        "rename replacement should permit hard-linked outputs: " <> show ioErr
                Right _ ->
                    pure ()
            targetContents <- T.readFile targetPath
            aliasContents <- T.readFile aliasPath
            assertEqual "target should receive the replacement output" replacementOutput targetContents
            assertEqual "hard-linked alias should retain the previous inode contents" staleOutput aliasContents
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

withReplacementOutputPathAndResolvedPathDirectFallbackTest :: Test
withReplacementOutputPathAndResolvedPathDirectFallbackTest =
    TestLabel "Asm.Output.with-replacement-output-path-and-resolved-path-direct-fallback" $ TestCase $ do
        tmpDir <- getTemporaryDirectory
        targetDir <- mkdtemp (tmpDir </> "htcc-output-direct-fallbackXXXXXX")
        let targetPath = targetDir </> "htcc-output-target"
            replacementOutput = "#!/bin/sh\nexit 0\n"
            cleanup = do
                ignoreIOException $ setFileMode targetDir 0o755
                ignoreIOException $ removeFile targetPath
                ignoreIOException $ removeDirectoryRecursive targetDir
        flip finally cleanup $ do
            T.writeFile targetPath "#!/bin/sh\nexit 99\n"
            setFileMode targetPath 0o644
            setFileMode targetDir 0o555
            (finalPath, stagedPath) <-
                withReplacementOutputPathAndResolvedPath PreserveReplacementOutputModeKeepingExecutableBits targetPath $ \tmpOutputPath -> do
                    T.writeFile tmpOutputPath replacementOutput
                    setFileMode tmpOutputPath 0o755
                    pure tmpOutputPath
            replacedOutput <- T.readFile targetPath
            assertEqual
                "direct fallback should report the final destination path rather than the temporary staging path"
                targetPath
                finalPath
            assertBool
                "direct fallback should stage the replacement away from the target directory"
                (stagedPath /= targetPath && takeDirectory stagedPath /= targetDir)
            assertEqual "direct fallback should replace the target output" replacementOutput replacedOutput
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

suppressWarnsRunAsmPreservesDirectiveLikePostWarningOutputTest :: Test
suppressWarnsRunAsmPreservesDirectiveLikePostWarningOutputTest =
    TestLabel "Asm.Output.suppress-warns-run-asm-preserves-directive-like-post-warning-output" $ TestCase $
        let filteredStderr =
                dropCompilerWarningOutput $
                    BC.unlines
                        [ "warning: this warning should be suppressed"
                        , ".section keep"
                        , "# generated by fake HTCC_ASSEMBLER wrapper"
                        , "1 warning generated."
                        ]
            expectedStderr =
                BC.unlines
                    [ ".section keep"
                    , "# generated by fake HTCC_ASSEMBLER wrapper"
                    , "1 warning generated."
                    ]
         in do
                assertEqual
                    "summary-shaped output should be preserved once unrelated post-warning output intervenes"
                    expectedStderr
                    filteredStderr

suppressWarnsRunAsmPreservesLeadInForRetainedErrorTest :: Test
suppressWarnsRunAsmPreservesLeadInForRetainedErrorTest =
    TestLabel "Asm.Output.suppress-warns-run-asm-preserves-lead-in-for-retained-error" $ TestCase $
        let filteredStderr =
                dropCompilerWarningOutput $
                    BC.unlines
                        [ "In file included from fake-header.h:1:"
                        , "                 from fake-source.c:2:"
                        , "warning: this warning should be suppressed"
                        , "1 warning generated."
                        , "error: fake HTCC_ASSEMBLER failure"
                        ]
            expectedStderr =
                BC.unlines
                    [ "In file included from fake-header.h:1:"
                    , "                 from fake-source.c:2:"
                    , "error: fake HTCC_ASSEMBLER failure"
                    ]
         in do
                assertEqual
                    "lead-in lines should stay attached when a retained error follows the suppressed warning"
                    expectedStderr
                    filteredStderr
                assertBool "suppressed warning text should be removed" $
                    not $
                        "warning: this warning should be suppressed" `BC.isInfixOf` filteredStderr

suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest :: Test
suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest =
    TestLabel "Asm.Output.suppress-warns-run-asm-preserves-warning-label-error-snippet" $ TestCase $
        let filteredStderr =
                dropCompilerWarningOutput $
                    BC.unlines
                        [ "warning: this warning should be suppressed"
                        , "{standard input}:1:1: error: fake HTCC_ASSEMBLER failure"
                        , "warning: return 1;"
                        , "^~~~~~~~~~~~~~~~~"
                        ]
         in do
                assertBool "standalone warnings should still be suppressed" $
                    not $
                        "warning: this warning should be suppressed" `BC.isInfixOf` filteredStderr
                assertBool "the real error should be preserved" $
                    "error: fake HTCC_ASSEMBLER failure" `BC.isInfixOf` filteredStderr
                assertBool "warning-label snippets should not be suppressed" $
                    "warning: return 1;" `BC.isInfixOf` filteredStderr
                assertBool "the snippet caret should remain attached" $
                    "^~~~~~~~~~~~~~~~~" `BC.isInfixOf` filteredStderr

suppressWarnsRunAsmSuppressesLocatedWarningsFromParenthesizedPathsTest :: Test
suppressWarnsRunAsmSuppressesLocatedWarningsFromParenthesizedPathsTest =
    TestLabel "Asm.Output.suppress-warns-run-asm-suppresses-located-warnings-from-parenthesized-paths" $ TestCase $
        let filteredStderr =
                dropCompilerWarningOutput $
                    BC.unlines
                        [ "/tmp/a(b)=c/x.c:1:1: warning: this warning should be suppressed"
                        , "1 warning generated."
                        , ".section keep"
                        ]
            expectedStderr =
                BC.unlines
                    [ ".section keep"
                    ]
         in assertEqual
                "located warnings should still be suppressed when the diagnostic path contains parentheses or equals signs"
                expectedStderr
                filteredStderr

incrementalWarningFilterRetainsStandalonePartialOutputTest :: Test
incrementalWarningFilterRetainsStandalonePartialOutputTest =
    TestLabel "Asm.Output.incremental-warning-filter-retains-standalone-partial-output" $ TestCase $
        let promptChunk = ("stdout: wrapper prompt", "stdout: wrapper prompt")
            (_, decisions) =
                feedIncrementalCompilerWarningFilter
                    fst
                    snd
                    emptyIncrementalCompilerWarningFilter
                    [promptChunk]
         in case decisions of
                [RetainCompilerWarningFilterChunk retainedChunk] ->
                    assertEqual
                        "standalone partial output should be retained immediately"
                        promptChunk
                        retainedChunk
                _ ->
                    assertFailure $
                        "expected a retained prompt chunk, got " <> show (length decisions) <> " decisions"

incrementalWarningFilterDefersPartialWarningPrefixTest :: Test
incrementalWarningFilterDefersPartialWarningPrefixTest =
    TestLabel "Asm.Output.incremental-warning-filter-defers-partial-warning-prefix" $ TestCase $
        let warningChunk = ("warning: fake HTCC_ASSEMBLER warning", "warning: fake HTCC_ASSEMBLER warning")
            (warningFilter, decisions) =
                feedIncrementalCompilerWarningFilter
                    fst
                    snd
                    emptyIncrementalCompilerWarningFilter
                    [warningChunk]
            finalDecisions =
                finalizeIncrementalCompilerWarningFilter
                    fst
                    snd
                    warningFilter
         in do
                assertBool
                    "partial warning prefixes should remain deferred until more input or finalization"
                    (null decisions)
                case finalDecisions of
                    [SuppressCompilerWarningFilterChunk suppressedChunk] ->
                        assertEqual
                            "finalization should still suppress the deferred warning chunk"
                            warningChunk
                            suppressedChunk
                    _ ->
                        assertFailure $
                            "expected a suppressed warning chunk after finalization, got "
                                <> show (length finalDecisions)
                                <> " decisions"

incompleteWarningSuppressionFlushesRetainedPromptPrefixTest :: Test
incompleteWarningSuppressionFlushesRetainedPromptPrefixTest =
    TestLabel "Asm.Output.incomplete-warning-suppression-flushes-retained-prompt-prefix" $ TestCase $
        assertBool
            "non-diagnostic interactive prompt text should not stay buffered waiting for a newline"
            (not $ WS.incompleteCompilerOutputNeedsMoreInputForWarningSuppression "stdout: wrapper prompt")

incompleteWarningSuppressionKeepsLocatedWarningPrefixBufferedTest :: Test
incompleteWarningSuppressionKeepsLocatedWarningPrefixBufferedTest =
    TestLabel "Asm.Output.incomplete-warning-suppression-keeps-located-warning-prefix-buffered" $ TestCase $
        assertBool
            "partial located warning prefixes still need buffering so later bytes can be suppressed as one diagnostic"
            (WS.incompleteCompilerOutputNeedsMoreInputForWarningSuppression "{standard input}:1:1: warn")

indirectFunctionPointerStackArgAlignmentTest :: Test
indirectFunctionPointerStackArgAlignmentTest = TestLabel "Asm.Output.indirect-function-pointer-stack-arg-alignment" $ TestCase $ do
    asm <- renderAsm "long sum7(long a, long b, long c, long d, long e, long f, long g) { return a + b + c + d + e + f + g; } int main(void) { long (*fp)(long, long, long, long, long, long, long); fp = sum7; return fp(1, 2, 3, 4, 5, 6, 7) - 28; }"
    assertContainsInOrder
        "stack-passed indirect calls probe the final call alignment before preparing args and keep any padding ahead of outgoing stack arguments"
        [ "mov rax, rsp"
        , "sub rax, 8"
        , "and rax, 15"
        , "jnz .L.call."
        , "mov r11, [rax+56]"
        , "push [rax+48]"
        , "mov rax, 0"
        , "call r11"
        , "add rsp, 8"
        , ".L.call."
        , "sub rsp, 8"
        , "mov r11, [rax+56]"
        , "push [rax+48]"
        , "mov rax, 0"
        , "call r11"
        , "add rsp, 8"
        , "add rsp, 8"
        ]
        asm

indirectFunctionLateStackArgCallOrderTest :: Test
indirectFunctionLateStackArgCallOrderTest = TestLabel "Asm.Output.indirect-function-late-stack-arg-call-order" $ TestCase $ do
    asm <- renderAsm "long g(void) { return 7; } long sum8(long a, long b, long c, long d, long e, long f, long g_, long h) { return g_ + h; } int main(void) { long (*fp)(long, long, long, long, long, long, long, long); fp = sum8; return fp(1, 2, 3, 4, 5, 6, g(), 8) - 15; }"
    let mainSection = extractFunctionSection "main" asm
    assertOccursBefore
        "indirect-call preparation should evaluate nested stack-argument calls before pushing later stack arguments"
        "call g"
        "push 8"
        mainSection

functionPointerGlobalObjectAddressRejectedTest :: Test
functionPointerGlobalObjectAddressRejectedTest = TestLabel "Asm.Output.function-pointer-global-object-address-rejected" $ TestCase $
    assertBool
        "function-pointer global initializers should reject object addresses, even when cast to a function-pointer type"
        (all rejected
            [ "int g; int (*fp)(void) = &g;"
            , "int g; int (*fp)(void) = (int (*)(void))&g;"
            ]
        )
    where
        rejected source =
            isLeft
                (runParser parser "<components>" source
                    :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
                )

objectPointerGlobalAddressMismatchRejectedTest :: Test
objectPointerGlobalAddressMismatchRejectedTest = TestLabel "Asm.Output.object-pointer-global-address-mismatch-rejected" $ TestCase $
    assertBool
        "object-pointer global initializers should reject already-known incompatible object addresses"
        (all rejected
            [ "int x[4]; int (*p)[5] = &x;"
            , "int x[4]; char *p = &x;"
            ]
        )
    where
        rejected source =
            isLeft
                (runParser parser "<components>" source
                    :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
                )

globalInitializerGnuConditionalTest :: Test
globalInitializerGnuConditionalTest = TestLabel "Asm.Output.global-initializer-gnu-conditional" $ TestCase $ do
    asm <- renderAsm "int g = 1 ?: 2; int h = 0 ?: 2; int i = 42 ?: 7; int main(void) { return g == 1 && h == 2 && i == 42; }"
    assertBool
        "GNU omitted-middle conditionals fold by reusing the condition value on the true branch"
        (all (`T.isInfixOf` asm)
            [ "g:\n\t.4byte 1"
            , "h:\n\t.4byte 2"
            , "i:\n\t.4byte 42"
            ]
        )

globalInitializerConditionalFunctionDecaySizeofTest :: Test
globalInitializerConditionalFunctionDecaySizeofTest = TestLabel "Asm.Output.global-initializer-conditional-function-decay-sizeof" $ TestCase $ do
    asm <- renderAsm "int f(void); int g = sizeof(1 ? f : 0); int main(void) { return g == 8; }"
    assertContains
        "conditional expressions preserve their decayed result type during normalization"
        [ "g:"
        , ".4byte 8"
        ]
        asm

globalInitializerConditionalRelocTest :: Test
globalInitializerConditionalRelocTest = TestLabel "Asm.Output.global-initializer-conditional-reloc" $ TestCase $ do
    asm <- renderAsm "int x; int f(void) { return 0; } int *p = 1 ? &x : 0; int (*fp)(void) = 1 ? f : 0;"
    assertContains
        "file-scope conditional initializers preserve wrapped object and function relocations"
        [ "x:"
        , ".zero 4"
        , "p:"
        , ".quad x"
        , "fp:"
        , ".quad f"
        ]
        asm

globalInitializerAddressConditionBoolTest :: Test
globalInitializerAddressConditionBoolTest = TestLabel "Asm.Output.global-initializer-address-condition-bool" $ TestCase $ do
    asm <- renderAsm "int x; int g = &x && 1; int h = &x ? 1 : 0; int main(void) { return g == 1 && h == 1; }"
    assertContains
        "file-scope scalar initializers fold address constants in boolean contexts"
        [ "x:"
        , ".zero 4"
        , "g:"
        , ".4byte 1"
        , "h:"
        , ".4byte 1"
        ]
        asm

globalInitializerAddressConditionRelocTest :: Test
globalInitializerAddressConditionRelocTest = TestLabel "Asm.Output.global-initializer-address-condition-reloc" $ TestCase $ do
    asm <- renderAsm "int x; int f(void) { return 0; } int *p = &x ? &x : 0; int (*fp)(void) = f ? f : 0;"
    assertContains
        "file-scope pointer initializers fold address-constant conditions before selecting relocations"
        [ "x:"
        , ".zero 4"
        , "p:"
        , ".quad x"
        , "fp:"
        , ".quad f"
        ]
        asm

commaFunctionDesignatorCallDecayTest :: Test
commaFunctionDesignatorCallDecayTest = TestLabel "Asm.Output.comma-function-designator-call-decay" $ TestCase $ do
    asm <- renderAsm "int foo(void) { return 42; } int main(void) { return (0, foo)(); }"
    assertContains
        "comma expressions decay bare function designators before indirect-call lowering"
        [ "foo:"
        , "main:"
        ]
        asm

commaAssignmentDiscardsLhsTest :: Test
commaAssignmentDiscardsLhsTest = TestLabel "Asm.Output.comma-assignment-discards-lhs" $ TestCase $ do
    asm <- renderAsm "int main(void) { int x; x = (1, 2); return x; }"
    assertContainsInOrder
        "comma expressions discard the lhs result before feeding assignments"
        [ "push 1"
        , "add rsp, 8"
        , "push 2"
        ]
        asm

globalInitializerStmtExprArrayDecaySizeofTest :: Test
globalInitializerStmtExprArrayDecaySizeofTest = TestLabel "Asm.Output.global-initializer-stmt-expr-array-decay-sizeof" $ TestCase $ do
    asm <- renderAsm "int x[4]; int y = sizeof(({ x; })); int main(void) { return y == 8; }"
    assertContains
        "statement expressions preserve their decayed result type during normalization"
        [ "x:"
        , ".zero 16"
        , "y:"
        , ".4byte 8"
        ]
        asm

globalInitializerModRemainderTest :: Test
globalInitializerModRemainderTest = TestLabel "Asm.Output.global-initializer-mod-remainder" $ TestCase $ do
    asm <- renderAsm "int g = -5 % 2; int h = 5 % -2; int main(void) { return g == -1 && h == 1; }"
    assertBool
        "global initializer modulo folding follows C remainder semantics"
        (all (`T.isInfixOf` asm)
            [ "g:\n\t.4byte -1"
            , "h:\n\t.4byte 1"
            ]
        )

globalInitializerDivTruncationTest :: Test
globalInitializerDivTruncationTest = TestLabel "Asm.Output.global-initializer-div-truncation" $ TestCase $ do
    asm <- renderAsm "int g = -5 / 2; int h = 5 / -2; char *p = \"abc\" + (-5 / 2 + 3); int main(void) { return g == -2 && h == -2 && p[0] == 'b'; }"
    assertBool
        "global initializer division folding truncates toward zero for integers and reloc addends"
        (all (`T.isInfixOf` asm)
            [ "g:\n\t.4byte -2"
            , "h:\n\t.4byte -2"
            , "p:\n\t.quad .L.data.0+1"
            ]
        )

normalizeAsmInputPreservesOperatorTypesTest :: Test
normalizeAsmInputPreservesOperatorTypesTest = TestLabel "Asm.Output.normalize-input-preserves-operator-types" $ TestCase $ do
    let intTy :: CT.StorageClass Integer
        intTy = CT.SCAuto CT.CTInt
        lit n = ATNode (ATNum n) intTy ATEmpty ATEmpty
        lessNode = ATNode ATLT intTy (lit 1) (lit 2)
        bitNode = ATNode ATAnd intTy (lit 1) (lit 2)
    case normalizeAsmInput [lessNode, bitNode] Map.empty of
        Left err ->
            assertFailure err
        Right ([normalizedLess, normalizedBit], _) -> do
            assertEqual
                "normalization should preserve existing comparison-node types when no rewritten global changes them"
                intTy
                (atype normalizedLess)
            assertEqual
                "normalization should preserve existing bitwise-node types when no rewritten global changes them"
                intTy
                (atype normalizedBit)
        Right _ ->
            assertFailure "internal test error: normalizeAsmInput returned an unexpected AST shape"

assertPrepareAsmInputError :: String -> T.Text -> String -> IO ()
assertPrepareAsmInputError label source expected = do
    (asts, gvars, _, funcs) <- parseAsmSource source
    case prepareAsmInput funcs asts gvars of
        Left err ->
            assertEqual label expected err
        Right _ ->
            assertFailure $ label <> ": expected asm input preparation failure"

assertPrepareVisualizableInputError :: String -> T.Text -> String -> IO ()
assertPrepareVisualizableInputError label source expected = do
    (asts, gvars, _, funcs) <- parseAsmSource source
    case prepareVisualizableInput funcs asts gvars of
        Left err ->
            assertEqual label expected err
        Right _ ->
            assertFailure $ label <> ": expected visualizable input preparation failure"

functionCallRefinementRevalidationTest :: Test
functionCallRefinementRevalidationTest = TestLabel "Asm.Output.function-call-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should revalidate refined function calls before codegen"
        "int f(); int main(void) { return f(1); } int f(void) { return 1; }"
        "too many arguments to function call"

objectPointerAssignmentRefinementRevalidationTest :: Test
objectPointerAssignmentRefinementRevalidationTest = TestLabel "Asm.Output.object-pointer-assignment-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject object-pointer assignments after later tentative-array completion"
        "int x[]; int main(void) { int (*p)[]; p = &x; return 0; } int x[4];"
        "invalid operands to assignment"

globalInitializerFunctionPointerRefinementRevalidationTest :: Test
globalInitializerFunctionPointerRefinementRevalidationTest = TestLabel "Asm.Output.global-initializer-function-pointer-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject file-scope function-pointer initializers after later function refinement"
        "int f(); int (*p)(void) = f; int f(int x) { return x; }"
        "invalid initializer for scalar object"

globalInitializerObjectPointerRefinementRevalidationTest :: Test
globalInitializerObjectPointerRefinementRevalidationTest = TestLabel "Asm.Output.global-initializer-object-pointer-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject file-scope object-pointer initializers after later tentative-array completion"
        "int x[]; int (*p)[] = &x; int main(void) { return 0; } int x[4];"
        "invalid initializer for scalar object"

globalInitializerIncompleteSizeofRevalidationTest :: Test
globalInitializerIncompleteSizeofRevalidationTest = TestLabel "Asm.Output.global-initializer-incomplete-sizeof-revalidation" $ TestCase $ do
    let longTy :: CT.StorageClass Integer
        longTy = CT.SCAuto $ CT.CTLong CT.CTInt
        incompleteArrayTy :: CT.StorageClass Integer
        incompleteArrayTy = CT.SCAuto $ CT.CTIncomplete (CT.IncompleteArray CT.CTInt)
        arrayPointerTy :: CT.StorageClass Integer
        arrayPointerTy = CT.SCAuto $ CT.CTPtr (CT.CTIncomplete (CT.IncompleteArray CT.CTInt))
        lhs =
            ATNode
                (ATLVar longTy 0)
                longTy
                ATEmpty
                ATEmpty
        rhs =
            ATNode
                ATSizeof
                longTy
                (ATNode
                    ATDeref
                    incompleteArrayTy
                    (ATNode (ATGVar arrayPointerTy "p") arrayPointerTy ATEmpty ATEmpty)
                    ATEmpty
                )
                ATEmpty
        initAst =
            ATNode
                (ATBlock [ATNode ATExprStmt longTy (ATNode ATAssign longTy lhs rhs) ATEmpty])
                longTy
                ATEmpty
                ATEmpty
        gvars :: GlobalVars Integer
        gvars =
            Map.fromList
                [ ("p", GVar arrayPointerTy GVarInitWithZero 0)
                , ("n", GVar longTy (GVarInitWithAST initAst) 0)
                ]
    case prepareAsmInput Map.empty [] gvars of
        Left err ->
            assertEqual
                "asm input preparation should reject deferred sizeof in global initializers when the operand stays incomplete"
                "invalid application of 'sizeof' to incomplete type"
                err
        Right _ ->
            assertFailure "expected asm input preparation failure"

functionPointerReturnRefinementRevalidationTest :: Test
functionPointerReturnRefinementRevalidationTest = TestLabel "Asm.Output.function-pointer-return-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject function-pointer return expressions after later function refinement"
        "int f(); int (*g(void))(void) { return f; } int f(int x) { return x; }"
        "invalid return type"

objectPointerReturnRefinementRevalidationTest :: Test
objectPointerReturnRefinementRevalidationTest = TestLabel "Asm.Output.object-pointer-return-refinement-revalidation" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject object-pointer return expressions after later tentative-array completion"
        "int a[]; int (*f(void))[3] { return &a; } int a[4];"
        "invalid return type"

pointerIncDecRefinementRevalidationTest :: Test
pointerIncDecRefinementRevalidationTest = TestLabel "Asm.Output.pointer-inc-dec-refinement-revalidation" $ TestCase $
    assertBool
        "same-input pointer ++/-- should reject pointer-to-array redeclarations that refine pointee bounds"
        (isLeft
            (runParser parser "<components>"
                "int (*p)[]; int main(void) { ++p; p++; --p; p--; return 0; } int (*p)[4];"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

pointerAddSubAssignRefinementRevalidationTest :: Test
pointerAddSubAssignRefinementRevalidationTest = TestLabel "Asm.Output.pointer-add-sub-assign-refinement-revalidation" $ TestCase $
    assertBool
        "same-input pointer compound assignments should reject pointer-to-array redeclarations that refine pointee bounds"
        (isLeft
            (runParser parser "<components>"
                "int (*p)[]; int main(void) { p += 1; p -= 1; return 0; } int (*p)[4];"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

pointerIncDecIncompleteRevalidationFailureTest :: Test
pointerIncDecIncompleteRevalidationFailureTest = TestLabel "Asm.Output.pointer-inc-dec-incomplete-revalidation-failure" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject deferred ++/-- on pointers that stay incomplete"
        "int (*f(void))[]; int main(void) { int (*p)[] = f(); ++p; return 0; }"
        "invalid use of pointer to incomplete type"

pointerAddSubAssignIncompleteRevalidationFailureTest :: Test
pointerAddSubAssignIncompleteRevalidationFailureTest = TestLabel "Asm.Output.pointer-add-sub-assign-incomplete-revalidation-failure" $ TestCase $
    assertPrepareAsmInputError
        "asm input preparation should reject deferred +=/-= on pointers that stay incomplete"
        "int (*f(void))[]; int main(void) { int (*p)[] = f(); p += 1; return 0; }"
        "invalid use of pointer to incomplete type"

visualizableInputAcceptsAsmNormalizationFailureTest :: Test
visualizableInputAcceptsAsmNormalizationFailureTest =
    TestLabel "Visualizer.prepare-input-accepts-asm-normalization-failure" $ TestCase $ do
        let source =
                "int (*f(void))[]; int main(void) { int (*p)[] = f(); ++p; return 0; }"
        (asts, gvars, _, funcs) <- parseAsmSource source
        case prepareVisualizableInput funcs asts gvars of
            Left err ->
                assertFailure $
                    "visualizable input preparation should keep parseable incomplete-pointer arithmetic renderable: "
                        <> err
            Right (preparedAsts, _) -> do
                svg <- renderVisualizationFromAsts preparedAsts
                assertBool
                    "visualizable input preparation should still produce a renderable AST"
                    ("<svg" `T.isInfixOf` svg
                        && "main" `T.isInfixOf` svg
                        && "return" `T.isInfixOf` svg
                    )

visualizableInputFunctionCallRefinementRevalidationTest :: Test
visualizableInputFunctionCallRefinementRevalidationTest =
    TestLabel "Visualizer.prepare-input-function-call-refinement-revalidation" $ TestCase $
        assertPrepareVisualizableInputError
            "visualizable input preparation should still reject merged direct calls with too many arguments"
            "int foo(); int main(void) { return foo(1); } int foo(void) { return 0; }"
            "too many arguments to function call"

incompleteGlobalSelfReferenceRejectedTest :: Test
incompleteGlobalSelfReferenceRejectedTest = TestLabel "Asm.Output.incomplete-global-self-reference-rejected" $ TestCase $
    assertBool
        "self-referential incomplete-array global initializers should be rejected before later normalization can retype them"
        (isLeft
            (runParser parser "<components>"
                "int x[] = { sizeof x, 0 };"
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, GlobalVars Integer, Literals Integer, PF.Functions Integer)
            )
        )

visualizerSizeofExprTest :: Test
visualizerSizeofExprTest = TestLabel "Visualizer.sizeof-expr" $ TestCase $ do
    svg <- renderVisualization "int main(void) { int x; return sizeof x; }"
    assertBool
        "visualizer renders expression-form sizeof nodes"
        ("sizeof" `T.isInfixOf` svg)

visualizerFunctionDesignatorTest :: Test
visualizerFunctionDesignatorTest = TestLabel "Visualizer.function-designator" $ TestCase $ do
    svg <- renderVisualization "int foo(void) { return 1; } int (*fp)(void) = &foo;"
    assertBool
        "visualizer renders bare function designators emitted as ATFuncPtr nodes"
        ("<svg" `T.isInfixOf` svg && "foo" `T.isInfixOf` svg)

visualizerPrunesEmptyDescendantsTest :: Test
visualizerPrunesEmptyDescendantsTest = TestLabel "Visualizer.prunes-empty-descendants" $ TestCase $ do
    let intTy :: CT.StorageClass Integer
        intTy = CT.SCAuto CT.CTInt
        nullTy :: CT.StorageClass Integer
        nullTy = CT.SCUndef CT.CTUndef
        literal = ATNode (ATNum 0) intTy ATEmpty ATEmpty
        wrappedLiteral = ATNode (ATNull literal) nullTy ATEmpty ATEmpty
        ast = ATNode ATReturn intTy wrappedLiteral ATEmpty
    svg <- renderVisualizationFromAsts [ast]
    assertBool
        "visualizer should keep renderable descendants hidden behind ATNull wrappers"
        ("return" `T.isInfixOf` svg && "0 (" `T.isInfixOf` svg)
    assertBool
        "visualizer should not emit placeholder Null nodes for empty descendants"
        (not $ "Null" `T.isInfixOf` svg)
    assertEqual
        "visualizer should only render the meaningful return and literal nodes"
        2
        (T.count "<text " svg)

visualizerPreservesNullStatementsInBlocksTest :: Test
visualizerPreservesNullStatementsInBlocksTest =
    TestLabel "Visualizer.preserves-null-statements-in-blocks" $ TestCase $ do
        nullStatementSvg <- renderVisualization "int main(void) { ; }"
        emptyBlockSvg <- renderVisualization "int main(void) { }"
        assertBool
            "visualizer should render null statements inside statement lists instead of pruning them as empty descendants"
            ("Null" `T.isInfixOf` nullStatementSvg)
        assertBool
            "visualizer should distinguish a block containing ';' from an actually empty block"
            (T.count "<text " nullStatementSvg > T.count "<text " emptyBlockSvg)

visualizerPreservesOmittedConditionalMiddleTest :: Test
visualizerPreservesOmittedConditionalMiddleTest =
    TestLabel "Visualizer.preserves-omitted-conditional-middle" $ TestCase $ do
        let intTy :: CT.StorageClass Integer
            intTy = CT.SCAuto CT.CTInt
            cond = ATNode (ATNum 1) intTy ATEmpty ATEmpty
            alternative = ATNode (ATNum 2) intTy ATEmpty ATEmpty
            ast = ATNode (ATConditional cond ATEmpty alternative) intTy ATEmpty ATEmpty
        svg <- renderVisualizationFromAsts [ast]
        assertBool
            "visualizer should keep GNU omitted-middle conditionals renderable"
            ("?:" `T.isInfixOf` svg && "1 (" `T.isInfixOf` svg && "2 (" `T.isInfixOf` svg)
        assertEqual
            "visualizer should render a placeholder child for the omitted middle expression"
            1
            (T.count "Null" svg)
        assertEqual
            "visualizer should render the conditional operator, both operands, and the placeholder"
            4
            (T.count "<text " svg)

visualizerPreservesEmptyForSectionsTest :: Test
visualizerPreservesEmptyForSectionsTest =
    TestLabel "Visualizer.preserves-empty-for-sections" $ TestCase $ do
        let intTy :: CT.StorageClass Integer
            intTy = CT.SCAuto CT.CTInt
            nullTy :: CT.StorageClass Integer
            nullTy = CT.SCUndef CT.CTUndef
            literal = ATNode (ATNum 0) intTy ATEmpty ATEmpty
            body = ATNode ATReturn intTy literal ATEmpty
            ast =
                ATNode
                    (ATFor
                        [ ATForInit ATEmpty
                        , ATForCond ATEmpty
                        , ATForIncr ATEmpty
                        , ATForStmt body
                        ]
                    )
                    nullTy
                    ATEmpty
                    ATEmpty
        svg <- renderVisualizationFromAsts [ast]
        assertBool
            "visualizer should render for-loop bodies alongside empty section placeholders"
            ("for" `T.isInfixOf` svg && "return" `T.isInfixOf` svg && "0 (" `T.isInfixOf` svg)
        assertEqual
            "visualizer should keep placeholders for omitted init, condition, and increment sections"
            3
            (T.count "Null" svg)
        assertEqual
            "visualizer should render the loop, three placeholders, the return, and the literal"
            6
            (T.count "<text " svg)

visualizerPreservesParsedEmptyForSectionsTest :: Test
visualizerPreservesParsedEmptyForSectionsTest =
    TestLabel "Visualizer.preserves-parsed-empty-for-sections" $ TestCase $ do
        svg <- renderVisualization "int main(){for(;;) return 1;}"
        assertBool
            "visualizer should preserve omitted for-loop sections from parsed input"
            ("for" `T.isInfixOf` svg && "return" `T.isInfixOf` svg && "1 (" `T.isInfixOf` svg)
        assertEqual
            "visualizer should preserve parsed placeholders for omitted init, condition, and increment sections"
            3
            (T.count "Null" svg)

visualizerPreservesParsedEmptyForBodyTest :: Test
visualizerPreservesParsedEmptyForBodyTest =
    TestLabel "Visualizer.preserves-parsed-empty-for-body" $ TestCase $ do
        svg <- renderVisualization "int main(){for(;;);}"
        assertBool
            "visualizer should keep parsed empty-body for-loops visible"
            ("for" `T.isInfixOf` svg)
        assertEqual
            "visualizer should preserve parsed placeholders for omitted init, condition, increment, and body sections"
            4
            (T.count "Null" svg)

visualizerPreservesEmptyControlFlowBodiesTest :: Test
visualizerPreservesEmptyControlFlowBodiesTest =
    TestLabel "Visualizer.preserves-empty-control-flow-bodies" $ TestCase $ do
        let intTy :: CT.StorageClass Integer
            intTy = CT.SCAuto CT.CTInt
            nullTy :: CT.StorageClass Integer
            nullTy = CT.SCUndef CT.CTUndef
            ifWithoutBodies =
                ATNode
                    ATElse
                    nullTy
                    (ATNode
                        ATIf
                        nullTy
                        (ATNode (ATNum 1) intTy ATEmpty ATEmpty)
                        ATEmpty
                    )
                    ATEmpty
            whileWithoutBody =
                ATNode
                    ATWhile
                    nullTy
                    (ATNode (ATNum 1) intTy ATEmpty ATEmpty)
                    ATEmpty
        svg <- renderVisualizationFromAsts [ifWithoutBodies, whileWithoutBody]
        assertBool
            "visualizer should keep empty control-flow bodies visible alongside their predicates"
            ("if" `T.isInfixOf` svg
                && "else" `T.isInfixOf` svg
                && "while" `T.isInfixOf` svg
                && "1 (" `T.isInfixOf` svg
            )
        assertEqual
            "visualizer should render placeholder children for empty if/else/while statement bodies"
            3
            (T.count "Null" svg)

visualizerPreservesEmptySwitchBodiesTest :: Test
visualizerPreservesEmptySwitchBodiesTest =
    TestLabel "Visualizer.preserves-empty-switch-bodies" $ TestCase $ do
        let intTy :: CT.StorageClass Integer
            intTy = CT.SCAuto CT.CTInt
            nullTy :: CT.StorageClass Integer
            nullTy = CT.SCUndef CT.CTUndef
            cond = ATNode (ATNum 1) intTy ATEmpty ATEmpty
            switchStmt =
                ATNode
                    (ATSwitch cond [])
                    nullTy
                    ATEmpty
                    ATEmpty
        svg <- renderVisualizationFromAsts [switchStmt]
        assertBool
            "visualizer should keep empty switch bodies visible alongside their predicates"
            ("switch" `T.isInfixOf` svg
                && "1 (" `T.isInfixOf` svg
            )
        assertEqual
            "visualizer should render placeholder children for empty switch bodies"
            2
            (T.count "Null" svg)
        assertEqual
            "visualizer should render the switch, its condition, and both empty-body placeholders"
            4
            (T.count "<text " svg)

visualizerPreservesEmptySwitchLabelBodiesTest :: Test
visualizerPreservesEmptySwitchLabelBodiesTest =
    TestLabel "Visualizer.preserves-empty-switch-label-bodies" $ TestCase $ do
        let intTy :: CT.StorageClass Integer
            intTy = CT.SCAuto CT.CTInt
            nullTy :: CT.StorageClass Integer
            nullTy = CT.SCUndef CT.CTUndef
            cond = ATNode (ATNum 1) intTy ATEmpty ATEmpty
            emptyStmt = ATNode (ATNull ATEmpty) nullTy ATEmpty ATEmpty
            caseStmt = ATNode (ATCase 0 1) nullTy emptyStmt ATEmpty
            defaultStmt = ATNode (ATDefault 0) nullTy emptyStmt ATEmpty
            switchStmt =
                ATNode
                    (ATSwitch cond [caseStmt, defaultStmt])
                    nullTy
                    ATEmpty
                    ATEmpty
        svg <- renderVisualizationFromAsts [switchStmt]
        assertBool
            "visualizer should keep empty switch label bodies visible"
            ("switch" `T.isInfixOf` svg
                && "case 1" `T.isInfixOf` svg
                && "default" `T.isInfixOf` svg
            )
        assertEqual
            "visualizer should render placeholders for empty case/default statement bodies"
            2
            (T.count "Null" svg)
        assertEqual
            "visualizer should render the switch, its condition, both labels, and two placeholders"
            6
            (T.count "<text " svg)

visualizerEmptyAstRejectsExistingOutputTest :: Test
visualizerEmptyAstRejectsExistingOutputTest = TestLabel "Visualizer.empty-ast-rejects-existing-output" $ TestCase $ do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-visualizer-empty.svg"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        hClose h
        T.writeFile path "stale output"
        result <- try
            (writeVisualization ([] :: ASTs Integer) (mkWidth 200) path)
            :: IO (Either IOException ())
        contents <- T.readFile path
        assertBool
            "empty ASTs should fail instead of reporting success"
            (isLeft result)
        assertEqual
            "empty ASTs should not overwrite an existing SVG target after failing"
            "stale output"
            contents
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

visualizerDeclarationOnlyAstRejectsExistingOutputTest :: Test
visualizerDeclarationOnlyAstRejectsExistingOutputTest = TestLabel "Visualizer.declaration-only-ast-rejects-existing-output" $ TestCase $ do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-visualizer-decl-only.svg"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        hClose h
        T.writeFile path "stale output"
        (asts, _, _, _) <- parseAsmSource "int g;"
        result <- try
            (writeVisualization asts (mkWidth 200) path)
            :: IO (Either IOException ())
        contents <- T.readFile path
        assertBool
            "declaration-only ASTs should fail instead of reporting success"
            (isLeft result)
        assertEqual
            "declaration-only ASTs should not overwrite an existing SVG target after failing"
            "stale output"
            contents
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

visualizerRejectsNonSvgOutputTest :: Test
visualizerRejectsNonSvgOutputTest = TestLabel "Visualizer.rejects-non-svg-output" $ TestCase $ do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "htcc-components-visualizer-invalid.png"
    flip finally (ignoreIOException (hClose h) >> ignoreIOException (removeFile path)) $ do
        hClose h
        T.writeFile path "stale output"
        (asts, _, _, _) <- parseAsmSource "int main(void) { return 0; }"
        result <- try
            (writeVisualization asts (mkWidth 200) path)
            :: IO (Either IOException ())
        contents <- T.readFile path
        assertBool
            "non-SVG outputs should be rejected before rendering"
            (isLeft result)
        case result of
            Left ioErr ->
                assertBool
                    "non-SVG failures should explain the required extension"
                    ("AST visualization output path must use the .svg extension" `T.isInfixOf` T.pack (show ioErr))
            Right _ ->
                assertFailure "non-SVG outputs should not render successfully"
        assertEqual
            "non-SVG failures should not overwrite an existing target"
            "stale output"
            contents
    where
        ignoreIOException = flip catchIOError $ const $ pure ()

visualizerRejectsSymlinkedNonSvgOutputTargetTest :: Test
visualizerRejectsSymlinkedNonSvgOutputTargetTest = TestLabel "Visualizer.rejects-symlinked-non-svg-output-target" $ TestCase $ do
    withVisualizerSymlinkPaths "target.txt" "alias.svg" $ \targetPath aliasPath -> do
        T.writeFile targetPath "stale output"
        createSymbolicLink targetPath aliasPath
        (asts, _, _, _) <- parseAsmSource "int main(void) { return 0; }"
        result <- try
            (writeVisualization asts (mkWidth 200) aliasPath)
            :: IO (Either IOException ())
        contents <- T.readFile targetPath
        assertBool
            "symlinked non-SVG targets should be rejected before rendering"
            (isLeft result)
        case result of
            Left ioErr ->
                assertBool
                    "symlink target failures should explain the required extension"
                    ("AST visualization output path must use the .svg extension" `T.isInfixOf` T.pack (show ioErr))
            Right _ ->
                assertFailure "symlinked non-SVG targets should not render successfully"
        assertEqual
            "symlink target failures should not overwrite the resolved target"
            "stale output"
            contents

visualizerAcceptsSymlinkAliasToSvgTargetTest :: Test
visualizerAcceptsSymlinkAliasToSvgTargetTest = TestLabel "Visualizer.accepts-symlink-alias-to-svg-target" $ TestCase $ do
    withVisualizerSymlinkPaths "target.svg" "current" $ \targetPath aliasPath -> do
        T.writeFile targetPath "stale output"
        createSymbolicLink targetPath aliasPath
        (asts, _, _, _) <- parseAsmSource "int main(void) { return 0; }"
        result <- try
            (writeVisualization asts (mkWidth 200) aliasPath)
            :: IO (Either IOException ())
        contents <- T.readFile targetPath
        case result of
            Left ioErr ->
                assertFailure $
                    "symlink aliases that resolve to SVG targets should render successfully, but failed with: "
                        <> show ioErr
            Right _ ->
                assertBool
                    "symlink aliases that resolve to SVG targets should update the resolved SVG"
                    ("<svg" `T.isInfixOf` contents && contents /= "stale output")

test :: Test
test = TestLabel "Asm.Output" $
    TestList
        [ returnLabelTest
        , controlFlowLabelTest
        , callArgumentGotoLabelTest
        , globalInitializerCastTest
        , globalInitializerDivTruncationTest
        , globalInitializerModRemainderTest
        , globalInitializerNullPointerCastTest
        , globalInitializerNestedNullPointerCastTest
        , globalInitializerFunctionNullPointerCastTest
        , globalInitializerWideCastTruncationTest
        , normalizeAsmInputPreservesOperatorTypesTest
        , functionCallRefinementRevalidationTest
        , objectPointerAssignmentRefinementRevalidationTest
        , globalInitializerFunctionPointerRefinementRevalidationTest
        , globalInitializerObjectPointerRefinementRevalidationTest
        , globalInitializerIncompleteSizeofRevalidationTest
        , functionPointerReturnRefinementRevalidationTest
        , objectPointerReturnRefinementRevalidationTest
        , pointerIncDecRefinementRevalidationTest
        , pointerAddSubAssignRefinementRevalidationTest
        , pointerIncDecIncompleteRevalidationFailureTest
        , pointerAddSubAssignIncompleteRevalidationFailureTest
        , visualizableInputAcceptsAsmNormalizationFailureTest
        , visualizableInputFunctionCallRefinementRevalidationTest
        , tentativeIncompleteArrayTest
        , tentativeIncompleteArrayDecayRetypeFallbackTest
        , tentativeIncompleteArraySizeofFallbackTest
        , staticTentativeIncompleteArrayUseSiteRejectedTest
        , nestedTentativeIncompleteArrayUseSiteRejectedTest
        , tentativeNestedArrayExtentInferenceTest
        , tentativeArrayEarlierFunctionDecayRetypeTest
        , tentativeNestedArrayOuterExtentMergeUnitTest
        , compatiblePointerArrayExtentCompletionMergeUnitTest
        , incompatiblePointerArrayExtentConflictMergeUnitTest
        , compatibleFunctionParamRefinementMergeUnitTest
        , incompatibleFunctionParamArrayExtentConflictMergeUnitTest
        , compatibleTaggedStructCompletionMergeUnitTest
        , incompatibleTaggedStructAliasMergeUnitTest
        , compatibleAnonymousStructMergeUnitTest
        , compatibleNamedStructAnonymousMemberMergeUnitTest
        , tentativeNestedArrayMaterializationUnitTest
        , tentativeArrayUseSiteRejectedTest
        , tentativeArrayAddressUseSiteRejectedTest
        , globalInitializerRelocAddendTest
        , globalInitializerArraySubobjectRelocTest
        , globalInitializerTentativeNestedArrayFallbackRelocTest
        , incompleteGlobalSelfReferenceRejectedTest
        , globalInitializerTentativeArrayUseSiteRejectedTest
        , globalInitializerSelfRetypeSizeofTest
        , globalInitializerSelfRetypeAddressTest
        , globalInitializerFunctionRelocTest
        , bareFunctionDesignatorDerefCallTest
        , functionPointerArrayZeroInitializerTest
        , functionPointerArrayFunctionInitializerTest
        , omittedBoundArrayPointerDerefDecayTest
        , indirectFunctionPointerCallTest
        , indirectFunctionPointerCallAlignmentTest
        , directBoolFunctionCallNormalizationTest
        , blockScopeExternObjectShadowsOuterLocalAsmTest
        , blockScopeExternObjectShadowsEnumeratorAsmTest
        , blockScopeExternFunctionShadowsEnumeratorAsmTest
        , blockScopeExternStaticFunctionAsmTest
        , blockScopeExternStaticObjectAsmTest
        , indirectBoolFunctionPointerCallNormalizationTest
        , directIntegralFunctionCallNormalizationTest
        , indirectIntegralFunctionPointerCallNormalizationTest
        , boolFunctionReturnNormalizationTest
        , directBoolFunctionArgNormalizationTest
        , directOldStyleBoolFunctionArgPromotionTest
        , indirectBoolFunctionPointerArgNormalizationTest
        , indirectOldStyleBoolFunctionPointerPromotionConflictTest
        , indirectFunctionPointerArgAlignmentTest
        , directFunctionStackArgAlignmentTest
        , directFunctionLateStackArgCallOrderTest
        , stackPassedParameterSpillTest
        , stackPassedBoolParameterSpillTest
        , writeOnlyFallbackReplacementRestoreTest
        , writeOnlyExecutableFallbackReplacementRestoreTest
        , unreadableStagedFallbackReplacementTest
        , rollbackFailureSurfacedTest
        , executableOnlyFallbackReplacementPreservesModeTest
        , replacementExecutableBitsIgnoreReadBitsTest
        , replacementExecutableBitsRestoreOwnerExecuteTest
        , freshExecutableReplacementPreservesExecuteBitsTest
        , freshExecutableReplacementRestoresOwnerExecuteTest
        , creationMaskedOutputModeMatchesActualCreationTest
        , hardLinkedFallbackReplacementRejectedTest
        , hardLinkedRenameReplacementPreservesAliasTest
        , withReplacementOutputPathAndResolvedPathDirectFallbackTest
        , suppressWarnsRunAsmPreservesDirectiveLikePostWarningOutputTest
        , suppressWarnsRunAsmPreservesLeadInForRetainedErrorTest
        , suppressWarnsRunAsmPreservesWarningLabelErrorSnippetTest
        , suppressWarnsRunAsmSuppressesLocatedWarningsFromParenthesizedPathsTest
        , incrementalWarningFilterRetainsStandalonePartialOutputTest
        , incrementalWarningFilterDefersPartialWarningPrefixTest
        , incompleteWarningSuppressionFlushesRetainedPromptPrefixTest
        , incompleteWarningSuppressionKeepsLocatedWarningPrefixBufferedTest
        , indirectFunctionPointerStackArgAlignmentTest
        , indirectFunctionLateStackArgCallOrderTest
        , objectPointerGlobalAddressMismatchRejectedTest
        , functionPointerGlobalObjectAddressRejectedTest
        , globalInitializerGnuConditionalTest
        , globalInitializerConditionalFunctionDecaySizeofTest
        , globalInitializerConditionalRelocTest
        , globalInitializerAddressConditionBoolTest
        , globalInitializerAddressConditionRelocTest
        , commaFunctionDesignatorCallDecayTest
        , commaAssignmentDiscardsLhsTest
        , globalInitializerStmtExprArrayDecaySizeofTest
        , visualizerSizeofExprTest
        , visualizerFunctionDesignatorTest
        , visualizerPrunesEmptyDescendantsTest
        , visualizerPreservesNullStatementsInBlocksTest
    , visualizerPreservesOmittedConditionalMiddleTest
    , visualizerPreservesEmptyForSectionsTest
    , visualizerPreservesParsedEmptyForSectionsTest
    , visualizerPreservesParsedEmptyForBodyTest
    , visualizerPreservesEmptyControlFlowBodiesTest
        , visualizerPreservesEmptySwitchBodiesTest
        , visualizerPreservesEmptySwitchLabelBodiesTest
        , visualizerEmptyAstRejectsExistingOutputTest
        , visualizerDeclarationOnlyAstRejectsExistingOutputTest
        , visualizerRejectsNonSvgOutputTest
        , visualizerRejectsSymlinkedNonSvgOutputTargetTest
        , visualizerAcceptsSymlinkAliasToSvgTargetTest
        ]
