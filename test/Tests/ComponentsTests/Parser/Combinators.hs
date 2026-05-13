{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes #-}
module Tests.ComponentsTests.Parser.Combinators (
    test
) where
import           Control.Monad                               (void)
import           Control.Monad.Trans.State.Lazy              (runStateT)
import qualified Data.ByteString                             as B
import           Data.Char                                   (chr, ord)
import           Data.Either                                 (isLeft, isRight)
import           Data.Functor.Identity                       (runIdentity)
import           Data.List                                   (isPrefixOf,
                                                              isSuffixOf)
import qualified Data.Map                                    as MP
import           Data.Maybe                                  (fromMaybe,
                                                              listToMaybe,
                                                              mapMaybe)
import qualified Data.Sequence                               as SQ
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import           Data.Void                                   (Void)
import qualified Htcc.CRules                                 as CR
import qualified Htcc.CRules.Types                           as CT
import qualified Htcc.MegaparsecCompat                       as M
import           Htcc.Parser.AST                             (ASTs, ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              fromATKindFor)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.ParserType          (runParserAllowSameInputExternalCollisions)
import           Htcc.Parser.Combinators.Program             (assign, parser)
import           Htcc.Parser.Combinators.Utils               (registerLVar)
import           Htcc.Parser.Combinators.Var                 (varInit)
import           Htcc.Parser.ConstructionData.Core           (ConstructionData,
                                                              Warnings,
                                                              initConstructionData,
                                                              lookupLVar)
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import qualified Htcc.Parser.ConstructionData.Scope.Var      as PV
import           Test.HUnit                                  (Test (..),
                                                              assertBool,
                                                              assertEqual,
                                                              assertFailure,
                                                              (~:), (~?=))
import qualified Text.Parsec.Pos                             as PP

type TestParser = M.Parsec Void T.Text

charLiteralTest,
    errorBundlePrettyTest,
    stringLiteralTest,
    hexadecimalTest,
    octalTest,
    naturalTest,
    integerTest,
    identifierTest :: Test

charLiteralTest = TestLabel "Parser.Combinators.Core.charLiteral" $
    TestList [
        TestLabel "Parser.Combinators.Core.charLiteral success patterns" $
            TestList [
                TestLabel "valid characters" $ TestList [
                    (show x <> " == " <> show x) ~:
                        M.runParser charLiteral' "" (T.singleton '\'' <> T.singleton x <> T.singleton '\'') ~?= Right (ord x)
                            | x <- charSets
                ]
              , TestLabel "multi-character constants" $ TestList [
                    "\'ab\' == 0x6162" ~:
                        M.runParser charLiteral' "" "\'ab\'" ~?= Right 0x6162
                  , "\'abc\' == 0x616263" ~:
                        M.runParser charLiteral' "" "\'abc\'" ~?= Right 0x616263
                  , "\'abcd\' == 0x61626364" ~:
                        M.runParser charLiteral' "" "\'abcd\'" ~?= Right 0x61626364
                  , "\'\\x7f\\x7f\\x7f\\x7f\' == 0x7f7f7f7f" ~:
                        M.runParser charLiteral' "" "'\\x7f\\x7f\\x7f\\x7f'" ~?= Right 0x7f7f7f7f
                  , "\'\\xff\' == 0xff" ~:
                        M.runParser charLiteral' "" "'\\xff'" ~?= Right 0xff
                  , "\'\\x000\' == 0" ~:
                        M.runParser charLiteral' "" "'\\x000'" ~?= Right 0
                  , "\'\\x0ff\' == 0xff" ~:
                        M.runParser charLiteral' "" "'\\x0ff'" ~?= Right 0xff
                  , "\'\\\'\' == 0x27" ~:
                        M.runParser charLiteral' "" "'\\''" ~?= Right (ord '\'')
                  , "\'\\\\\' == 0x5c" ~:
                        M.runParser charLiteral' "" "'\\\\'" ~?= Right (ord '\\')
                  , "\'\\n\' == 0x0a" ~:
                        M.runParser charLiteral' "" "'\\n'" ~?= Right (ord '\n')
                  , "\'\\r\' == 0x0d" ~:
                        M.runParser charLiteral' "" "'\\r'" ~?= Right (ord '\r')
                ]
              , TestLabel "partial characters" $ TestList [
                    "\'a\'b == a" ~:
                        M.runParser charLiteral' "" "\'a\'b" ~?= Right (ord 'a')
                  , "\'!\'b == !" ~:
                        M.runParser charLiteral' "" "\'!\'b" ~?= Right (ord '!')
                ]
            ]
      , TestLabel "Parser.Combinators.Core.charLiteral fail patterns" $
            TestList [
                "ab" ~: isLeft (M.runParser charLiteral' "" "ab") ~?= True
              , "123" ~: isLeft (M.runParser charLiteral' "" "123") ~?= True
              , "\'\'" ~: isLeft (M.runParser charLiteral' "" "\'\'") ~?= True
              , "\'\'\'" ~: isLeft (M.runParser charLiteral' "" "\'\'\'") ~?= True
              , "raw newline" ~: isLeft (M.runParser charLiteral' "" "\'a\n\'") ~?= True
              , "\'a" ~: isLeft (M.runParser charLiteral' "" "\'a") ~?= True
              , "\'\\x100\'" ~: isLeft (M.runParser charLiteral' "" "'\\x100'") ~?= True
              , "\'\\x0000001\'" ~: isLeft (M.runParser charLiteral' "" "'\\x0000001'") ~?= True
              , "oversized hex escape fails early" ~:
                    isLeft (M.runParser charLiteral' "" $ T.pack ("'\\x" <> replicate 1000 '1' <> "'")) ~?= True
              , "\'abcde\'" ~: isLeft (M.runParser charLiteral' "" "\'abcde\'") ~?= True
            ]
    ]
    where
        charLiteral' = charLiteral :: TestParser Int
        charSets =
            ['A'..'Z']
            <> ['a'..'z']
            <> ['0'..'9']
            <> "!\"#%&()*+,-./:;<=>?[]^_{|}~\a\b\f\t\v\0"
            <> [chr 27]

stringLiteralTest = TestLabel "Parser.Combinators.Core.stringLiteral" $
    TestList [
        TestLabel "Parser.Combinators.Core.stringLiteral success patterns" $
            TestList [
                "\"abc\" == abc" ~: M.runParser stringLiteral' "" "\"abc\"" ~?= Right (bytes "abc\0")
              , "\"012\" == 012" ~: M.runParser stringLiteral' "" "\"012\"" ~?= Right (bytes "012\0")
              , "\"012\"3 == 012" ~: M.runParser stringLiteral' "" "\"012\"3" ~?= Right (bytes "012\0")
              , "\"\\xff\" == 0xff" ~: M.runParser stringLiteral' "" "\"\\xff\"" ~?= Right (B.pack [0xff, 0])
              , "\"\\x000\" == 0" ~: M.runParser stringLiteral' "" "\"\\x000\"" ~?= Right (B.pack [0, 0])
              , "\"\\x0ff\" == 0xff" ~: M.runParser stringLiteral' "" "\"\\x0ff\"" ~?= Right (B.pack [0xff, 0])
              , "\"é\" keeps raw UTF-8 bytes" ~:
                    M.runParser stringLiteral' "" "\"é\"" ~?= Right (TE.encodeUtf8 "é" <> B.singleton 0)
              , "\"あ\" keeps raw UTF-8 bytes" ~:
                    M.runParser stringLiteral' "" "\"あ\"" ~?= Right (TE.encodeUtf8 "あ" <> B.singleton 0)
            ]
      , TestLabel "Parser.Combinators.Core.stringLiteral fail patterns" $
            TestList [
                "abc" ~: isLeft (M.runParser stringLiteral' "" "abc") ~?= True
              , "\"abc" ~: isLeft (M.runParser stringLiteral' "" "\"abc") ~?= True
              , "\"\\x100\"" ~: isLeft (M.runParser stringLiteral' "" "\"\\x100\"") ~?= True
              , "\"\\x0000001\"" ~: isLeft (M.runParser stringLiteral' "" "\"\\x0000001\"") ~?= True
              , "raw newline" ~: isLeft (M.runParser stringLiteral' "" "\"a\nb\"") ~?= True
              , "raw carriage return" ~: isLeft (M.runParser stringLiteral' "" "\"a\rb\"") ~?= True
              , "oversized hex escape fails early" ~:
                    isLeft (M.runParser stringLiteral' "" $ T.pack ("\"\\x" <> replicate 1000 '1' <> "\"")) ~?= True
            ]
    ]
    where
        stringLiteral' = stringLiteral :: TestParser B.ByteString
        bytes = B.pack . map (fromIntegral . ord)

errorBundlePrettyTest = TestLabel "Parser.Combinators.Core.errorBundlePretty" $
    TestList
        [ TestLabel "truncates source lines when the caret is near the beginning" $ TestCase $
            assertTruncatedSourceLine
                identifier'
                (T.cons '!' $ T.replicate 240 "a")
                $ \srcLn caretLn -> do
                    assertBool "source line should keep the beginning" $ "!" `isPrefixOf` srcLn
                    assertBool "source line should show truncation suffix" $ " ..." `isSuffixOf` srcLn
                    assertEqual "caret should stay at the first column" "^" caretLn
        , TestLabel "truncates source lines around a middle caret" $ TestCase $
            assertTruncatedSourceLine
                (identifier' <* M.eof)
                (T.replicate 180 "a" <> "!" <> T.replicate 200 "b")
                $ \srcLn caretLn -> do
                    assertBool "source line should show truncation prefix" $ "... " `isPrefixOf` srcLn
                    assertBool "source line should show truncation suffix" $ " ..." `isSuffixOf` srcLn
                    assertBool "caret should remain inside the shown source line" $
                        length (takeWhile (/= '^') caretLn) < length srcLn
        ]
    where
        identifier' = identifier :: TestParser T.Text
        assertTruncatedSourceLine parserUnderTest input assertSource = case M.runParser parserUnderTest "" input of
            Left err ->
                case lines $ show err of
                    _loc : srcLn : caretLn : _ -> do
                        assertBool "source line should be capped" $ length srcLn <= 160
                        assertBool "caret line should contain a caret" $ '^' `elem` caretLn
                        assertSource srcLn caretLn
                    _ ->
                        assertFailure "expected pretty error with source and caret lines"
            Right _ ->
                assertFailure "expected parse failure"

hexadecimalTest = TestLabel "Parser.Combinators.Core.hexadecimal" $
    TestList [
        TestLabel "Parser.Combinators.Core.hexadecimal success patterns" $
            TestList [
                "0x01 == 0x01" ~: M.runParser hexadecimal' "" "0x01" ~?= Right 0x01
              , "0xf == 0xf" ~: M.runParser hexadecimal' "" "0xf" ~?= Right 0xf
              , "0X0 == 0x0" ~: M.runParser hexadecimal' "" "0X0" ~?= Right 0x0
              , "0Xf == 0xf" ~: M.runParser hexadecimal' "" "0Xf" ~?= Right 0xf
              , "0xfz == 0xf" ~: M.runParser hexadecimal' "" "0xfz" ~?= Right 0xf
            ]
      , TestLabel "Parser.Combinators.Core.hexadecimal fail patterns" $
            TestList [
                "0x" ~: isLeft (M.runParser hexadecimal' "" "0x") ~?= True
              , "0xz" ~: isLeft (M.runParser hexadecimal' "" "0xz") ~?= True
              , "01" ~: isLeft (M.runParser hexadecimal' "" "01") ~?= True
              , "0" ~: isLeft (M.runParser hexadecimal' "" "0") ~?= True
            ]
    ]
    where
        hexadecimal' = hexadecimal :: TestParser Int

octalTest = TestLabel "Parser.Combinators.Core.octal" $
    TestList [
        TestLabel "Parser.Combinators.Core.octal success patterns" $
            TestList [
                "01 == 0o1" ~: M.runParser octal' "" "01" ~?= Right 0o1
              , "0010 == 0o10" ~: M.runParser octal' "" "0010" ~?= Right 0o10
              , "0010a == 0o10" ~: M.runParser octal' "" "0010a" ~?= Right 0o10
            ]
      , TestLabel "Parser.Combinators.Core.octal fail patterns" $
            TestList [
                "0x0" ~: isLeft (M.runParser octal' "" "0x0") ~?= True
              , "0" ~: isLeft (M.runParser octal' "" "0") ~?= True
            ]
    ]
    where
        octal' = octal :: TestParser Int

naturalTest = TestLabel "Parser.Combinators.Core.natural" $
    TestList [
        TestLabel "Parser.Combinators.Core.natural success patterns" $
            TestList [
                "10 == 10" ~: M.runParser natural' "" "10" ~?= Right 10
              , "0010 == 0o10" ~: M.runParser natural' "" "0010" ~?= Right 0o10
              , "0x1 == 0x1" ~: M.runParser natural' "" "0x1" ~?= Right 0x1
              , "0b101 == 5" ~: M.runParser natural' "" "0b101" ~?= Right 5
              , "0x == 0" ~: M.runParser natural' "" "0x" ~?= Right 0
              , "0xz == 0" ~: M.runParser natural' "" "0xz" ~?= Right 0
              , "00x0 == 0" ~: M.runParser natural' "" "00x0" ~?= Right 0
            ]
      , TestLabel "Parser.Combinators.Core.natural fail patterns" $
            TestList [
                "hoge" ~: isLeft (M.runParser natural' "" "hoge") ~?= True
              , "binary literal over digit limit" ~:
                    isLeft (M.runParser (natural' <* M.eof) "" ("0b" <> T.replicate 129 "1")) ~?= True
            ]
    ]
    where
        natural' = natural :: TestParser Int

integerTest = TestLabel "Parser.Combinators.Core.integer" $
    TestList [
        TestLabel "Parser.Combinators.Core.integer success patterns" $
            TestList [
                "10 == 10" ~: M.runParser integer' "" "10" ~?= Right 10
              , "0010 == 0o10" ~: M.runParser integer' "" "0010" ~?= Right 0o10
              , "0x1 == 0x1" ~: M.runParser integer' "" "0x1" ~?= Right 0x1
              , "0x == 0" ~: M.runParser integer' "" "0x" ~?= Right 0
              , "0xz == 0" ~: M.runParser integer' "" "0xz" ~?= Right 0
              , "00x0 == 0" ~: M.runParser integer' "" "00x0" ~?= Right 0
              , "+10 == 10" ~: M.runParser integer' "" "+10" ~?= Right 10
              , "+0010 == 0o10" ~: M.runParser integer' "" "+0010" ~?= Right 0o10
              , "+0x1 == 0x1" ~: M.runParser integer' "" "+0x1" ~?= Right 0x1
              , "+0b101 == 5" ~: M.runParser integer' "" "+0b101" ~?= Right 5
              , "+0x == 0" ~: M.runParser integer' "" "+0x" ~?= Right 0
              , "+0xz == 0" ~: M.runParser integer' "" "+0xz" ~?= Right 0
              , "+00x0 == 0" ~: M.runParser integer' "" "+00x0" ~?= Right 0
              , "-10 == -10" ~: M.runParser integer' "" "-10" ~?= Right (-10)
              , "-0010 == -0o10" ~: M.runParser integer' "" "-0010" ~?= Right (-0o10)
              , "-0x1 == -0x1" ~: M.runParser integer' "" "-0x1" ~?= Right (-0x1)
              , "-0b101 == -5" ~: M.runParser integer' "" "-0b101" ~?= Right (-5)
              , "-0x == 0" ~: M.runParser integer' "" "-0x" ~?= Right 0
              , "-0xz == 0" ~: M.runParser integer' "" "-0xz" ~?= Right 0
              , "-00x0 == 0" ~: M.runParser integer' "" "-00x0" ~?= Right 0
            ]
    ]
    where
        integer' = integer :: TestParser Int

identifierTest = TestLabel "Parser.Combinators.Core.identifier" $
    TestList [
        TestLabel "Parser.Combinators.Core.identifier success patterns" $
            TestList [
                "a" ~: M.runParser identifier' "" "a =" ~?= Right "a"
              , "abcde" ~: M.runParser identifier' "" "abcde" ~?= Right "abcde"
              , "_" ~: M.runParser identifier' "" "_" ~?= Right "_"
              , "a@" ~: M.runParser identifier' "" "a@" ~?= Right "a"
              , "a1a" ~: M.runParser identifier' "" "a1" ~?= Right "a1"
            ]
      , TestLabel "Parser.Combinators.Core.identifier fail patterns" $
            TestList [
                TestLabel "invalid characters eg" $ TestList [
                    "@" ~: isLeft (M.runParser identifier' "" "@") ~?= True
                  , "@a" ~: isLeft (M.runParser identifier' "" "@a") ~?= True
                  , "1a" ~: isLeft (M.runParser identifier' "" "1a") ~?= True
                ]
              , TestLabel "3 characters op" $
                    TestList [T.unpack op ~: isLeft (M.runParser identifier' "" op) ~?= True | op <- CR.strOps3]
              , TestLabel "2 characters op" $
                    TestList [T.unpack op ~: isLeft (M.runParser identifier' "" op) ~?= True | op <- CR.strOps2]
              , TestLabel "1 characters op" $
                    TestList [[op] ~: isLeft (M.runParser identifier' "" $ T.singleton op) ~?= True | op <- CR.charOps]
              , TestLabel "reserved keywords" $
                    TestList
                        [ "if" ~: isLeft (M.runParser identifier' "" "if") ~?= True
                        , "for" ~: isLeft (M.runParser identifier' "" "for") ~?= True
                        , "int" ~: isLeft (M.runParser identifier' "" "int") ~?= True
                        , "return" ~: isLeft (M.runParser identifier' "" "return") ~?= True
                        , "_Bool" ~: isLeft (M.runParser identifier' "" "_Bool") ~?= True
                        , "typedef" ~: isLeft (M.runParser identifier' "" "typedef") ~?= True
                        ]
            ]
    ]
    where
        identifier' = identifier :: TestParser T.Text

runInitializerParser :: Parser Integer a -> T.Text -> Either (M.ParseErrorBundle T.Text Void) a
runInitializerParser p input =
    fst $ runIdentity $ runStateT (M.runParserT p "" input) initConstructionData

runInitializerParserState
    :: Parser Integer a
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (ConstructionData Integer)
runInitializerParserState p input = case runIdentity $ runStateT (M.runParserT p "" input) initConstructionData of
    (Left err, _)    -> Left err
    (Right _, state) -> Right state

parseInitializerAST
    :: CT.StorageClass Integer
    -> [(T.Text, CT.StorageClass Integer)]
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (ATree Integer)
parseInitializerAST ty surroundingVars =
    runInitializerParser $ do
        spaceConsumer
        mapM_ (\(ident, ty') -> void $ registerLVar ty' ident) surroundingVars
        equal *> varInit assign ty "x" <* semi <* M.eof

parseInitializer :: CT.StorageClass Integer -> [(T.Text, CT.StorageClass Integer)] -> T.Text -> Either (M.ParseErrorBundle T.Text Void) ()
parseInitializer ty surroundingVars =
    runInitializerParser $ do
        spaceConsumer
        mapM_ (\(ident, ty') -> void $ registerLVar ty' ident) surroundingVars
        void $ equal *> varInit assign ty "x" <* semi
        M.eof

inferInitializerType
    :: CT.StorageClass Integer
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (CT.StorageClass Integer)
inferInitializerType =
    flip inferInitializerTypeWithVars []

inferInitializerTypeWithVars
    :: CT.StorageClass Integer
    -> [(T.Text, CT.StorageClass Integer)]
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (CT.StorageClass Integer)
inferInitializerTypeWithVars ty surroundingVars =
    (PV.lvtype . fromMaybe (error "missing variable x") . lookupLVar "x" <$>)
        . runInitializerParserState parser'
    where
        parser' = do
            spaceConsumer
            mapM_ (\(ident, ty') -> void $ registerLVar ty' ident) surroundingVars
            void $ equal *> varInit assign ty "x" <* semi
            M.eof

inferGlobalType
    :: T.Text
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (CT.StorageClass Integer)
inferGlobalType ident input =
    PV.gvtype
        . fromMaybe (error $ "missing global variable " <> T.unpack ident)
        . MP.lookup ident
        . (\(_, _, gvars, _, _) -> gvars)
        <$> runParser parser "" input

inferGlobalInitWith
    :: T.Text
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (PV.GVarInitWith Integer)
inferGlobalInitWith ident input =
    PV.initWith
        . fromMaybe (error $ "missing global variable " <> T.unpack ident)
        . MP.lookup ident
        . (\(_, _, gvars, _, _) -> gvars)
        <$> runParser parser "" input

inferFunctionType
    :: T.Text
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (CT.StorageClass Integer)
inferFunctionType ident input =
    PF.fntype
        . fromMaybe (error $ "missing function " <> T.unpack ident)
        . MP.lookup ident
        . (\(_, _, _, _, fns) -> fns)
        <$> runParser parser "" input

hasFunctionBinding
    :: T.Text
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) Bool
hasFunctionBinding ident input =
    MP.member ident . (\(_, _, _, _, fns) -> fns)
        <$> (runParser parser "" input :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer))

parseProgram :: T.Text -> Either (M.ParseErrorBundle T.Text Void) ()
parseProgram input =
    void
        (runParser parser "" input :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer))

parseProgramWarnings :: T.Text -> Either (M.ParseErrorBundle T.Text Void) Warnings
parseProgramWarnings input =
    (\(warnings, _, _, _, _) -> warnings)
        <$> (runParser parser "" input :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer))

parseProgramAsts :: T.Text -> Either (M.ParseErrorBundle T.Text Void) (ASTs Integer)
parseProgramAsts input =
    (\(_, asts, _, _, _) -> asts)
        <$> (runParser parser "" input :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer))

parseProgramLiterals :: T.Text -> Either (M.ParseErrorBundle T.Text Void) (PV.Literals Integer)
parseProgramLiterals input =
    (\(_, _, _, literals, _) -> literals)
        <$> (runParser parser "" input :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer))

parseAssignExpr :: T.Text -> Either (M.ParseErrorBundle T.Text Void) (ATree Integer)
parseAssignExpr =
    runInitializerParser . (spaceConsumer *>) . (<* M.eof) $ assign

parseProgramAllowSameInputExternalCollisions :: T.Text -> Either (M.ParseErrorBundle T.Text Void) ()
parseProgramAllowSameInputExternalCollisions input =
    void
        ( runParserAllowSameInputExternalCollisions parser "" input
            :: Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer)
        )

firstLocalDeclType
    :: T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (CT.StorageClass Integer)
firstLocalDeclType input =
    fromMaybe (error "missing local declaration")
        . listToMaybe
        . concatMap collectLocalDeclTypes
        <$> parseProgramAsts input

collectLocalDeclTypes :: ATree Integer -> [CT.StorageClass Integer]
collectLocalDeclTypes ATEmpty = []
collectLocalDeclTypes (ATNode kind _ lhs rhs) =
    kindLocalDeclTypes kind
        <> collectLocalDeclTypes lhs
        <> collectLocalDeclTypes rhs
    where
        kindLocalDeclTypes = \case
            ATNull (ATNode (ATLVar ty _) _ _ _) ->
                [ty]
            ATConditional cond tr fl ->
                collectLocalDeclTypes cond
                    <> collectLocalDeclTypes tr
                    <> collectLocalDeclTypes fl
            ATSwitch cond cases ->
                collectLocalDeclTypes cond
                    <> concatMap collectLocalDeclTypes cases
            ATFor kinds ->
                concatMap (collectLocalDeclTypes . fromATKindFor) kinds
            ATBlock ats ->
                concatMap collectLocalDeclTypes ats
            ATStmtExpr ats ->
                concatMap collectLocalDeclTypes ats
            ATDefFunc _ params ->
                maybe [] (concatMap collectLocalDeclTypes) params
            ATCallFunc _ params ->
                maybe [] (concatMap collectLocalDeclTypes) params
            ATCallPtr params ->
                maybe [] (concatMap collectLocalDeclTypes) params
            _ ->
                []

containsIncompleteStructTag :: T.Text -> CT.TypeKind Integer -> Bool
containsIncompleteStructTag tag = \case
    CT.CTPtr innerTy ->
        containsIncompleteStructTag tag innerTy
    CT.CTArray _ innerTy ->
        containsIncompleteStructTag tag innerTy
    CT.CTFunc retTy params ->
        containsIncompleteStructTag tag retTy
            || any (containsIncompleteStructTag tag . fst) params
    CT.CTEnum baseTy _ ->
        containsIncompleteStructTag tag baseTy
    CT.CTStruct members ->
        any (containsIncompleteStructTag tag . CT.smType) members
    CT.CTNamedStruct _ _ members ->
        any (containsIncompleteStructTag tag . CT.smType) members
    CT.CTIncomplete (CT.IncompleteArray innerTy) ->
        containsIncompleteStructTag tag innerTy
    CT.CTIncomplete (CT.IncompleteStruct foundTag _) ->
        foundTag == tag
    _ ->
        False

assertProgramErrorContains :: T.Text -> T.Text -> IO ()
assertProgramErrorContains errMsg input = case parseProgram input of
    Left err -> assertBool
        "unexpected error message"
        (errMsg `T.isInfixOf` T.pack (show err))
    Right _ -> assertFailure "expected parse failure"

errorBundleLoc :: M.ParseErrorBundle T.Text Void -> (Int, Int)
errorBundleLoc err =
    ( fromIntegral $ PP.sourceLine pos
    , fromIntegral $ PP.sourceColumn pos
    )
    where
        pos = M.pstateSourcePos $ M.bundlePosState err

pairMembers :: MP.Map T.Text (CT.StructMember Integer)
pairMembers = MP.fromList
    [ ("left", CT.StructMember CT.CTInt 0)
    , ("right", CT.StructMember CT.CTInt 8)
    ]

pairTy :: CT.StorageClass Integer
pairTy = CT.SCAuto $ CT.CTStruct pairMembers

boxTy :: CT.StorageClass Integer
boxTy = CT.SCAuto $ CT.CTStruct $ MP.fromList
    [ ("pair", CT.StructMember (CT.CTStruct pairMembers) 0)
    , ("value", CT.StructMember CT.CTInt (CT.sizeof (CT.CTStruct pairMembers)))
    ]

pairArrayTy :: CT.StorageClass Integer
pairArrayTy = CT.SCAuto $ CT.CTArray 2 (CT.CTStruct pairMembers)

charPtrTy :: CT.TypeKind Integer
charPtrTy = CT.CTPtr CT.CTChar

charPtrIncompleteArrayTy :: CT.StorageClass Integer
charPtrIncompleteArrayTy = CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray charPtrTy

charIncompleteArrayTy :: CT.StorageClass Integer
charIncompleteArrayTy = CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray CT.CTChar

fixedCharArrayTy :: CT.StorageClass Integer
fixedCharArrayTy = CT.SCAuto $ CT.CTArray 3 CT.CTChar

twoCharArrayTy :: CT.StorageClass Integer
twoCharArrayTy = CT.SCAuto $ CT.CTArray 2 CT.CTChar

oneCharArrayTy :: CT.StorageClass Integer
oneCharArrayTy = CT.SCAuto $ CT.CTArray 1 CT.CTChar

intArrayTy :: CT.TypeKind Integer
intArrayTy = CT.CTArray 2 CT.CTInt

intFunctionTy :: CT.TypeKind Integer
intFunctionTy = CT.CTFunc CT.CTInt [(CT.CTVoid, Nothing)]

intFunctionPtrTy :: CT.StorageClass Integer
intFunctionPtrTy = CT.SCAuto $ CT.CTPtr intFunctionTy

wideIntArrayTy :: CT.TypeKind Integer
wideIntArrayTy = CT.CTArray 3 CT.CTInt

wideNestedIntArrayTy :: CT.StorageClass Integer
wideNestedIntArrayTy = CT.SCAuto $ CT.makeCTArray [2, 3] CT.CTInt

shortCharRowTy :: CT.TypeKind Integer
shortCharRowTy = CT.CTArray 3 CT.CTChar

nestedIntArrayTy :: CT.StorageClass Integer
nestedIntArrayTy = CT.SCAuto $ CT.CTArray 2 intArrayTy

fixedOuterIncompleteInnerIntArrayTy :: CT.StorageClass Integer
fixedOuterIncompleteInnerIntArrayTy = CT.SCAuto $ CT.CTArray 2 $ CT.CTIncomplete $ CT.IncompleteArray CT.CTInt

wideOuterIncompleteInnerIntArrayTy :: CT.StorageClass Integer
wideOuterIncompleteInnerIntArrayTy = CT.SCAuto $ CT.CTArray 3 $ CT.CTIncomplete $ CT.IncompleteArray CT.CTInt

wideOuterIncompleteInnerCharArrayTy :: CT.StorageClass Integer
wideOuterIncompleteInnerCharArrayTy = CT.SCAuto $ CT.CTArray 3 $ CT.CTIncomplete $ CT.IncompleteArray CT.CTChar

charRowTy :: CT.TypeKind Integer
charRowTy = CT.CTArray 4 CT.CTChar

nestedCharArrayTy :: CT.StorageClass Integer
nestedCharArrayTy = CT.SCAuto $ CT.makeCTArray [2, 4] CT.CTChar

pointerMemberTy :: CT.StorageClass Integer
pointerMemberTy = CT.SCAuto $ CT.CTStruct $ MP.fromList
    [ ("ptr", CT.StructMember (CT.CTPtr CT.CTInt) 0)
    ]

arrayMemberTy :: CT.StorageClass Integer
arrayMemberTy = CT.SCAuto $ CT.CTStruct $ MP.fromList
    [ ("values", CT.StructMember intArrayTy 0)
    , ("value", CT.StructMember CT.CTInt (fromIntegral $ CT.sizeof intArrayTy))
    ]

charArrayMemberTy :: CT.StorageClass Integer
charArrayMemberTy = CT.SCAuto $ CT.CTStruct $ MP.fromList
    [ ("text", CT.StructMember charRowTy 0)
    , ("value", CT.StructMember CT.CTInt (fromIntegral $ CT.sizeof charRowTy))
    ]

paddedStructTy :: CT.StorageClass Integer
paddedStructTy = CT.SCAuto $ CT.CTStruct $ MP.fromList
    [ ("c", CT.StructMember CT.CTChar 0)
    , ("i", CT.StructMember CT.CTInt 4)
    ]

initializerFirstStmtZeroByteOffset :: ATree Integer -> Maybe Integer
initializerFirstStmtZeroByteOffset ast = case ast of
    ATNode (ATBlock (stmt:_)) _ _ _ -> initializerStmtZeroByteOffset stmt
    _                               -> Nothing

initializerZeroByteOffsets :: ATree Integer -> [Integer]
initializerZeroByteOffsets ast = case ast of
    ATNode (ATBlock stmts) _ _ _ -> mapMaybe initializerStmtZeroByteOffset stmts
    _                            -> []

initializerStmtZeroByteOffset :: ATree Integer -> Maybe Integer
initializerStmtZeroByteOffset (ATNode ATExprStmt _ expr _) = initializerExprZeroByteOffset expr
initializerStmtZeroByteOffset _                            = Nothing

initializerExprZeroByteOffset :: ATree Integer -> Maybe Integer
initializerExprZeroByteOffset (ATNode ATAssign _ lhs rhs)
    | initializerIsZeroLiteral rhs = initializerLhsZeroByteOffset lhs
    | otherwise = Nothing
initializerExprZeroByteOffset _ = Nothing

initializerLhsZeroByteOffset :: ATree Integer -> Maybe Integer
initializerLhsZeroByteOffset (ATNode ATDeref ty ptr _)
    | CT.toTypeKind ty == CT.CTChar = initializerPointerByteOffset ptr
    | otherwise = Nothing
initializerLhsZeroByteOffset _ = Nothing

initializerPointerByteOffset :: ATree Integer -> Maybe Integer
initializerPointerByteOffset (ATNode ATCast _ lhs _) = initializerPointerByteOffset lhs
initializerPointerByteOffset (ATNode ATAddPtr _ lhs (ATNode (ATNum offset) _ _ _)) =
    (+ offset) <$> initializerPointerByteOffset lhs
initializerPointerByteOffset (ATNode ATAddr _ _ _) = Just 0
initializerPointerByteOffset _                     = Nothing

initializerIsZeroLiteral :: ATree Integer -> Bool
initializerIsZeroLiteral (ATNode (ATNum 0) _ _ _) = True
initializerIsZeroLiteral _                        = False

structInitializerTest :: Test
structInitializerTest = TestLabel "Parser.Program.struct-initializer" $
    TestList
        [ "rejects struct copy initialization without braces" ~:
            isLeft (parseInitializer pairTy [("y", pairTy)] "= y;") ~?= True
        , "rejects braced struct copy initialization" ~:
            isLeft (parseInitializer pairTy [("y", pairTy)] "= { y };") ~?= True
        , "rejects brace-elided nested struct copy expressions" ~:
            isLeft (parseInitializer boxTy [("y", pairTy)] "= { y, 3 };") ~?= True
        , "rejects braced nested struct copy expressions" ~:
            isLeft (parseInitializer boxTy [("y", pairTy)] "= { { y }, 3 };") ~?= True
        , "accepts named nested struct copy expressions" ~:
            isRight (parseProgram "struct A { int a; }; struct B { int c; struct A b; }; int main(void) { struct A st = {42}; struct B st2 = {12, st}; return st2.b.a; }") ~?= True
        , "accepts omitted-bound arrays initialized from struct copy expressions" ~:
            isRight (parseProgram "struct A { int a; }; int main(void) { struct A st = {42}; struct A arr[] = { st }; return arr[0].a; }") ~?= True
        , "does not leak literals from failed speculative struct copy parsing" ~:
            length <$> parseProgramLiterals "struct A { char text[3]; int value; }; struct B { struct A a; int tail; }; int main(void) { struct B b = { \"hi\", 7, 9 }; return b.a.value; }" ~?= Right 0
        , "rejects brace-elided array member copy expressions" ~:
            isLeft (parseInitializer arrayMemberTy [("y", CT.SCAuto intArrayTy)] "= { y, 3 };") ~?= True
        , "accepts brace-elided nested struct initializers" ~:
            isRight (parseInitializer boxTy [] "= { 1, 2, 3 };") ~?= True
        , "accepts brace-elided array members inside struct initializers" ~:
            isRight (parseInitializer arrayMemberTy [] "= { 1, 2, 3 };") ~?= True
        , "rejects treating braced array members as part of a single brace-elided struct element" ~:
            isLeft
                ( parseInitializer
                    (CT.SCAuto $ CT.CTArray 1 $ CT.toTypeKind arrayMemberTy)
                    []
                    "= { { 1, 2 }, 3 };"
                )
                ~?= True
        , "accepts braced array-of-struct elements whose first member is braced" ~:
            isRight
                ( parseInitializer
                    (CT.SCAuto $ CT.CTArray 2 $ CT.toTypeKind arrayMemberTy)
                    []
                    "= { {{1, 2}, 3}, {{4, 5}, 6} };"
                )
                ~?= True
        , "accepts brace-elided array-of-struct initializers" ~:
            isRight (parseInitializer pairArrayTy [] "= { 1, 2, 3, 4 };") ~?= True
        , "accepts trailing commas after short brace-elided array-of-struct initializers" ~:
            isRight (parseInitializer pairArrayTy [] "= { 1, 2, 3, };") ~?= True
        , "accepts array-to-pointer decay for pointer members inside struct initializers" ~:
            isRight (parseInitializer pointerMemberTy [("a", CT.SCAuto intArrayTy)] "= { a };") ~?= True
        , "accepts string literals for leading char-array members in brace-elided struct initializers" ~:
            isRight
                ( parseInitializer
                    (CT.SCAuto $ CT.CTArray 1 $ CT.toTypeKind charArrayMemberTy)
                    []
                    "= { \"abc\", 1 };"
                )
                ~?= True
        , TestLabel "zero-fills only omitted struct storage after explicit initialization" $ TestCase $
            case parseInitializerAST paddedStructTy [] "= { 1 };" of
                Left err -> assertFailure $ show err
                Right ast -> do
                    assertEqual
                        "explicit member initialization should precede zero fill"
                        Nothing
                        (initializerFirstStmtZeroByteOffset ast)
                    assertEqual
                        "unexpected zero-filled byte offsets"
                        [1 .. fromIntegral (pred $ CT.sizeof $ CT.toTypeKind paddedStructTy)]
                        (initializerZeroByteOffsets ast)
        ]

incompleteArrayInitializerTest :: Test
incompleteArrayInitializerTest = TestLabel "Parser.Program.incomplete-array-initializer" $
    TestList
        [ "rejects empty brace initialization for incomplete arrays" ~:
            isLeft
                (inferInitializerType (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray CT.CTInt) "= {};")
                ~?= True
        , "rejects empty brace initialization for incomplete nested arrays" ~:
            isLeft
                (inferInitializerType (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray intArrayTy) "= {};")
                ~?= True
        , "accepts empty brace initialization for fixed arrays" ~:
            isRight (parseInitializer (CT.SCAuto intArrayTy) [] "= {};") ~?= True
        , "infers row count from brace-elided nested arrays" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTArray 2 $ CT.CTIncomplete $ CT.IncompleteArray CT.CTInt)
                "= { 1, 2, 3, 4 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 intArrayTy)
        , "accepts brace-elided nested arrays when only the inner bound is inferred" ~:
            isRight (parseInitializer fixedOuterIncompleteInnerIntArrayTy [] "= { 1, 2, 3, 4 };") ~?= True
        , "accepts brace-elided initializers when only the immediate inner array bound is inferred" ~:
            isRight (parseInitializer fixedOuterIncompleteInnerIntArrayTy [] "= { 1, 2, 3, 4, 5, 6 };") ~?= True
        , "infers only the immediate inner bound for braced nested arrays with fixed outer arrays" ~:
            inferInitializerType
                fixedOuterIncompleteInnerIntArrayTy
                "= {{1, 2}, {3, 4}};"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 intArrayTy)
        , "preserves the declared inner width when outer row count is inferred" ~:
            inferInitializerType
                wideOuterIncompleteInnerIntArrayTy
                "= {{1, 2}, {3, 4}, {5, 6}};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [3, 3] CT.CTInt)
        , "preserves the declared inner width even when explicit rows are narrower" ~:
            inferInitializerType
                wideOuterIncompleteInnerIntArrayTy
                "= {{1}, {2, 3}, {4}};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [3, 3] CT.CTInt)
        , "infers only the immediate inner array bound for fixed outer arrays" ~:
            inferInitializerType
                fixedOuterIncompleteInnerIntArrayTy
                "= { 1, 2, 3, 4, 5, 6 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 wideIntArrayTy)
        , "infers explicit string row width for fixed outer char arrays" ~:
            inferInitializerType
                wideOuterIncompleteInnerCharArrayTy
                "= {\"ab\", \"cd\", \"ef\"};"
                ~?= Right (CT.SCAuto $ CT.CTArray 3 shortCharRowTy)
        , "preserves the declared char[][N] row width for direct string rows" ~:
            inferInitializerType
                wideOuterIncompleteInnerCharArrayTy
                "= {\"a\", \"b\", \"c\"};"
                ~?= Right (CT.SCAuto $ CT.CTArray 3 shortCharRowTy)
        , "preserves the declared char[][N] row width for braced string rows" ~:
            inferInitializerType
                wideOuterIncompleteInnerCharArrayTy
                "= {{\"a\"}, {\"b\"}, {\"c\"}};"
                ~?= Right (CT.SCAuto $ CT.CTArray 3 shortCharRowTy)
        , "preserves the declared char[][N] row width for braced char-list rows" ~:
            inferInitializerType
                wideOuterIncompleteInnerCharArrayTy
                "= {{'a', 'b'}, {'c'}};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [2, 3] CT.CTChar)
        , "preserves the declared char[][N] row width for mixed string and braced char-list rows" ~:
            inferInitializerType
                wideOuterIncompleteInnerCharArrayTy
                "= {\"ab\", {'c'}};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [2, 3] CT.CTChar)
        , "infers only the omitted outer bound for char[][4] braced char-list rows" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTArray 4 $ CT.CTIncomplete $ CT.IncompleteArray CT.CTChar)
                "= {{'a'}};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 4] CT.CTChar)
        , "rejects overlong string rows when char[][N] only infers the outer bound" ~:
            isLeft
                (parseInitializer wideOuterIncompleteInnerCharArrayTy [] "= {\"abcd\"};")
                ~?= True
        , "infers pointer arrays when string literals decay to pointer elements" ~:
            inferInitializerType charPtrIncompleteArrayTy "= { \"x\" };"
                ~?= Right (CT.SCAuto $ CT.CTArray 1 charPtrTy)
        , "infers char array length from braced string initializers" ~:
            inferInitializerType charIncompleteArrayTy "= {\"abc\"};"
                ~?= Right (CT.SCAuto $ CT.CTArray 4 CT.CTChar)
        , "accepts braced string initializers for incomplete char arrays" ~:
            isRight (parseInitializer charIncompleteArrayTy [] "= {\"abc\"};") ~?= True
        , "infers char[][N] bounds from braced char-list rows" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray charRowTy)
                "= {{'a'}, \"bc\"};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [2, 4] CT.CTChar)
        , "accepts numeric braced char-list rows while inferring char[][N] bounds" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray charRowTy)
                "= {{1}, \"bc\"};"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [2, 4] CT.CTChar)
        , "rejects brace-elided nested array copy expressions while probing incomplete array length" ~:
            isLeft
                ( inferInitializerTypeWithVars
                    (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray intArrayTy)
                    [("b", CT.SCAuto intArrayTy)]
                    "= { b };"
                )
                ~?= True
        , "rejects brace-elided nested array copy expressions during initialization" ~:
            isLeft (parseInitializer nestedIntArrayTy [("b", CT.SCAuto intArrayTy)] "= { b };") ~?= True
        , "accepts braced scalar elements inside brace-elided nested arrays" ~:
            isRight (parseInitializer nestedIntArrayTy [] "= { 1, {2}, 3 };") ~?= True
        , "accepts braced rows in fixed nested arrays" ~:
            isRight (parseInitializer nestedIntArrayTy [] "= {{1, 2}, {3, 4}};") ~?= True
        , "accepts wide braced rows in fixed nested arrays" ~:
            isRight (parseInitializer wideNestedIntArrayTy [] "= {{1, 2, 3}, {4, 5, 6}};") ~?= True
        , "infers row count when brace-elided nested arrays contain braced scalar elements" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray intArrayTy)
                "= { 1, {2}, 3 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 intArrayTy)
        , "infers element count from brace-elided struct aggregates" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray (CT.CTStruct pairMembers))
                "= { 1, 2, 3, 4 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 (CT.CTStruct pairMembers))
        , "infers separate elements when struct element initialization starts with braces" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray $ CT.toTypeKind arrayMemberTy)
                "= { { 1, 2 }, 3 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 $ CT.toTypeKind arrayMemberTy)
        , "infers element count when braced struct elements start with braced array members" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray $ CT.toTypeKind arrayMemberTy)
                "= { {{1, 2}, 3}, {{4, 5}, 6} };"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 $ CT.toTypeKind arrayMemberTy)
        , "infers element count when brace-elided struct elements start with string literals" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray $ CT.toTypeKind charArrayMemberTy)
                "= { \"abc\", 1 };"
                ~?= Right (CT.SCAuto $ CT.CTArray 1 $ CT.toTypeKind charArrayMemberTy)
        , TestLabel "restores parser position after probing incomplete array length" $ TestCase $
            case parseInitializer
                    (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray intArrayTy)
                    []
                    "={1,2};@"
            of
                Left err -> assertEqual "unexpected error location" (1, 8) (errorBundleLoc err)
                Right _  -> assertFailure "expected parse failure"
        , TestLabel "restores parser position after probing direct string length for incomplete char arrays" $ TestCase $
            case parseInitializer charIncompleteArrayTy [] "=\"abc\";@" of
                Left err -> assertEqual "unexpected error location" (1, 8) (errorBundleLoc err)
                Right _  -> assertFailure "expected parse failure"
        , "rejects excess brace-elided rows in fixed nested arrays" ~:
            isLeft (parseInitializer nestedIntArrayTy [] "= { 1, 2, 3, 4, 5 };") ~?= True
        , "accepts trailing commas after short brace-elided nested arrays" ~:
            isRight (parseInitializer nestedIntArrayTy [] "= { 1, 2, 3, };") ~?= True
        , "accepts braced string rows in fixed nested char arrays" ~:
            isRight (parseInitializer nestedCharArrayTy [] "= {{\"abc\"}, {\"def\"}};") ~?= True
        , "accepts exact-fit fixed char arrays from string literals" ~:
            isRight (parseInitializer fixedCharArrayTy [] "= \"abc\";") ~?= True
        , "accepts exact-fit fixed char arrays from braced string literals" ~:
            isRight (parseInitializer fixedCharArrayTy [] "= {\"abc\"};") ~?= True
        , "accepts fixed char arrays that omit the trailing terminator when the bound is exact" ~:
            isRight (parseInitializer twoCharArrayTy [] "= \"ab\";") ~?= True
        , "accepts single-element fixed char arrays initialized from one-character strings" ~:
            isRight (parseInitializer oneCharArrayTy [] "= \"x\";") ~?= True
        , "accepts exact-fit string rows in brace-elided fixed nested char arrays" ~:
            isRight (parseInitializer (CT.SCAuto $ CT.makeCTArray [1, 3] CT.CTChar) [] "= {\"abc\"};") ~?= True
        , "accepts exact-fit string rows in braced fixed nested char arrays" ~:
            isRight (parseInitializer (CT.SCAuto $ CT.makeCTArray [1, 3] CT.CTChar) [] "= {{\"abc\"}};") ~?= True
        , "rejects overlong fixed char arrays from string literals" ~:
            isLeft (parseInitializer fixedCharArrayTy [] "= \"abcd\";") ~?= True
        , "rejects fixed char arrays that exceed the bound before the terminator" ~:
            isLeft (parseInitializer twoCharArrayTy [] "= \"abc\";") ~?= True
        , "rejects overlong fixed char arrays from braced string literals" ~:
            isLeft (parseInitializer fixedCharArrayTy [] "= {\"abcd\"};") ~?= True
        , "infers row count from braced string rows in incomplete nested char arrays" ~:
            inferInitializerType
                (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteArray charRowTy)
                "= {{\"abc\"}, {\"def\"}};"
                ~?= Right nestedCharArrayTy
        , TestLabel "reports excess fixed-array elements at the offending initializer" $ TestCase $
            case parseInitializer nestedIntArrayTy [] "= { 1, 2, 3, 4, 5 };" of
                Left err -> do
                    assertEqual "unexpected error location" (1, 17) (errorBundleLoc err)
                    assertBool
                        "unexpected error message"
                        (T.isInfixOf "excess elements in array initializer" $ T.pack $ show err)
                Right _ -> assertFailure "expected parse failure"
        ]

constantExpressionTest :: Test
constantExpressionTest = TestLabel "Parser.Program.constant-expression" $
    TestList
        [ "accepts '%' in array-bound constant expressions" ~:
            isRight (parseProgram "int a[5 % 2]; int main(void) { return sizeof a / sizeof a[0] == 1; }")
                ~?= True
        , "accepts '%' in case-label constant expressions" ~:
            isRight (parseProgram "int main(void) { switch (0) { case 5 % 2: return 1; default: return 0; } }")
                ~?= True
        , "rejects ',' in array-bound integer constant expressions" ~:
            isLeft (parseProgram "int a[(1, 2)]; int main(void) { return 0; }")
                ~?= True
        , TestLabel "rejects ',' in case-label integer constant expressions" $ TestCase $
            assertProgramErrorContains
                "The expression is not constant-expression"
                "int main(void) { switch (0) { case (1, 2): return 1; default: return 0; } }"
        , "short-circuits '||' in array-bound constant expressions" ~:
            isRight (parseProgram "int a[1 || 1 / 0]; int main(void) { return sizeof a / sizeof a[0] == 1; }")
                ~?= True
        , "short-circuits '&&' in case-label constant expressions" ~:
            isRight (parseProgram "int main(void) { switch (0) { case 0 && 1 % 0: return 1; default: return 0; } }")
                ~?= True
        , TestLabel "rejects sizeof of incomplete operands in case-label constant expressions" $ TestCase $
            assertProgramErrorContains
                "invalid application of 'sizeof' to incomplete type"
                "int (*p)[]; int main(void) { switch (0) { case sizeof(*p): return 1; default: return 0; } }"
        , TestLabel "rejects _Alignof of incomplete operands in case-label constant expressions" $ TestCase $
            assertProgramErrorContains
                "invalid application of '_Alignof' to incomplete type"
                "int (*p)[]; int main(void) { switch (0) { case _Alignof(*p): return 1; default: return 0; } }"
        ]

integerOperatorTypeTest :: Test
integerOperatorTypeTest = TestLabel "Parser.Program.integer-operator-type" $
    TestList
        [ "preserves int result types for bitwise operators on int operands" ~:
            (CT.toTypeKind . atype <$> parseAssignExpr "((int)1) & ((int)2)")
                ~?= Right CT.CTInt
        , "preserves promoted int result types for shift operators" ~:
            (CT.toTypeKind . atype <$> parseAssignExpr "((char)1) << 1")
                ~?= Right CT.CTInt
        , "treats enum types as integral for integer-only operators" ~:
            CT.isIntegral (CT.SCAuto $ CT.CTEnum CT.CTInt mempty)
                ~?= True
        , TestLabel "rejects shift operators on bare function designators even when they return _Bool" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "_Bool f(void); int main(void) { return f << 1; }"
        ]

globalInitializerTest :: Test
globalInitializerTest = TestLabel "Parser.Program.global-initializer" $
    TestList
        [ "accepts empty translation units" ~:
            isRight (parseProgram "")
                ~?= True
        , "accepts comment-only translation units" ~:
            isRight (parseProgram "/* no declarations */ // no declarations\n")
                ~?= True
        , "accepts file-scope declarations without declarators" ~:
            isRight (parseProgram "int; int main(void) { return 0; }")
                ~?= True
        , "accepts file-scope static declarations without declarators" ~:
            isRight (parseProgram "static int; int main(void) { return 0; }")
                ~?= True
        , TestLabel "rejects file-scope auto declarations without declarators" $ TestCase $
            assertProgramErrorContains
                "storage-class specifier is not allowed at file scope"
                "auto int; int main(void) { return 0; }"
        , TestLabel "rejects file-scope register declarations without declarators" $ TestCase $
            assertProgramErrorContains
                "storage-class specifier is not allowed at file scope"
                "register int; int main(void) { return 0; }"
        , "infers file-scope int array bounds from braced initializers" ~:
            inferGlobalType "a" "int a[] = {1, 2};"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 CT.CTInt)
        , "infers file-scope char array bounds from string initializers" ~:
            inferGlobalType "s" "char s[] = \"x\";"
                ~?= Right (CT.SCAuto $ CT.CTArray 2 CT.CTChar)
        , "merges same-file tentative incomplete arrays with later complete declarations" ~:
            inferGlobalType "x" "int x[]; int x[4];"
                ~?= Right (CT.SCAuto $ CT.CTArray 4 CT.CTInt)
        , "merges same-file tentative nested incomplete arrays when only the outermost bound is missing" ~:
            inferGlobalType "x" "int x[][4]; int x[2][4];"
                ~?= Right (CT.SCAuto $ CT.makeCTArray [2, 4] CT.CTInt)
        , "rejects same-file tentative arrays whose element type is an incomplete struct" ~:
            isLeft (parseProgram "struct S a[];")
                ~?= True
        , "rejects extern arrays whose fixed-bound element type is incomplete" ~:
            isLeft (parseProgram "extern struct S arr[1]; struct S { int a; }; int main(void) { return 0; }")
                ~?= True
        , "accepts extern arrays whose top-level bound is omitted" ~:
            isRight (parseProgram "extern int a[]; int main(void) { return 0; }")
                ~?= True
        , "rejects same-file tentative arrays whose omitted bound is not the only incompleteness" ~:
            isLeft (parseProgram "int a[][];")
                ~?= True
        , "rejects sizeof on a tentative array before a later completing declaration" ~:
            isLeft (parseProgram "int x[]; int main(void) { return sizeof x; } int x[4];")
                ~?= True
        , "rejects sizeof on a typedef-backed tentative array before completion" ~:
            isLeft
                (parseProgram "typedef int Row[2]; extern Row rows[]; int main(void) { return sizeof rows; }")
                ~?= True
        , "rejects _Alignof on a typedef-backed tentative array before completion" ~:
            isLeft
                (parseProgram "typedef int Row[2]; extern Row rows[]; int main(void) { return _Alignof rows; }")
                ~?= True
        , "rejects address arithmetic on a tentative array before a later completing declaration" ~:
            isLeft (parseProgram "int x[]; int main(void) { return ((char*)(&x + 1)) - ((char*)&x); } int x[4];")
                ~?= True
        , "rejects pointer arithmetic after a later function-return pointer-to-array redeclaration changes the pointee bound" ~:
            isLeft (parseProgram "int (*f(void))[]; int *g(void) { return *(f() + 1); } int (*f(void))[4];")
                ~?= True
        , "rejects sizeof after a later function-return pointer-to-array redeclaration changes the pointee bound" ~:
            isLeft (parseProgram "int (*f(void))[]; int main(void) { return sizeof *f(); } int (*f(void))[4];")
                ~?= True
        , "rejects _Alignof after a later function-return pointer-to-array redeclaration changes the pointee bound" ~:
            isLeft (parseProgram "int (*f(void))[]; int main(void) { return _Alignof *f(); } int (*f(void))[4];")
                ~?= True
        , "accepts same-file tentative nested incomplete arrays at use sites before finalization" ~:
            isRight (parseProgram "int x[][4]; int main(void) { return sizeof x[0]; }")
                ~?= True
        , "accepts dereferenced omitted-bound array pointers in ordinary expression contexts" ~:
            isRight
                (parseProgram "int main(void) { int x[4]; int (*p)[] = (int (*)[])&x; int *q = *p; q[1] = 7; return (*p)[1]; }")
                ~?= True
        , "rejects same-file tentative arrays whose omitted bound is not outermost" ~:
            isLeft (parseProgram "int x[2][]; int x[2][4];")
                ~?= True
        , TestLabel "rejects same-file tentative arrays whose later declaration changes rank" $ TestCase $
            case parseProgram "int x[]; int x[2][4];" of
                Left err -> assertBool
                    "unexpected error message"
                    (T.isInfixOf "redeclaration of 'x' with no linkage" $ T.pack $ show err)
                Right _ -> assertFailure "expected parse failure"
        , "accepts file-scope static tentative incomplete arrays" ~:
            isRight (parseProgram "static int x[];") ~?= True
        , "accepts file-scope static function definitions" ~:
            isRight (parseProgram "static int helper(void) { return 1; } int main(void) { return helper(); }") ~?= True
        , "accepts top-level braced scalar int initializers" ~:
            isRight (parseInitializer (CT.SCAuto CT.CTInt) [] "= {1};") ~?= True
        , "accepts top-level braced scalar pointer initializers" ~:
            isRight (parseInitializer (CT.SCAuto $ CT.CTPtr CT.CTChar) [] "= {\"x\"};") ~?= True
        , "accepts pointer casts of address constants in file-scope initializers" ~:
            isRight (parseProgram "int arr[1]; char* p = (char*)arr; int main() { return p == (char*)arr; }")
                ~?= True
        , "accepts casted null constants in pointer file-scope initializers" ~:
            isRight (parseProgram "char *p = (char*)0; int main(void) { return p == 0; }")
                ~?= True
        , "accepts nested pointer-cast null constants in pointer file-scope initializers" ~:
            isRight (parseProgram "int *p = (int*)(void*)0; int main(void) { return p == 0; }")
                ~?= True
        , "accepts plain address constants in pointer file-scope initializers" ~:
            isRight (parseProgram "int g; int* p = &g;")
                ~?= True
        , "accepts self-referential address constants in pointer file-scope initializers" ~:
            isRight (parseProgram "void *p = &p;")
                ~?= True
        , "accepts array-subobject decay reached through a dereference in file-scope initializers" ~:
            isRight (parseProgram "int x[2][4]; int *p = x[0];")
                ~?= True
        , "accepts addressed array-subobject elements in file-scope initializers" ~:
            isRight (parseProgram "int x[2][4]; int *p = &x[0][0];")
                ~?= True
        , "accepts address constants with byte addends in pointer file-scope initializers" ~:
            isRight (parseProgram "int a[2]; int *p = &a[1]; char *q = \"ab\" + 1;")
                ~?= True
        , "accepts address constants with non-trivial integer addends in pointer file-scope initializers" ~:
            isRight (parseProgram "int a[4]; int *p = a + (1 + 1);")
                ~?= True
        , "accepts short-circuited logical-and addends in pointer file-scope initializers" ~:
            isRight (parseProgram "int a[4]; int *p = a + (0 && 1/0);")
                ~?= True
        , "accepts short-circuited logical-or elements in aggregate file-scope initializers" ~:
            isRight (parseProgram "int g[] = {1 || 1/0};")
                ~?= True
        , "accepts file-scope nested omitted-bound char arrays with initializers" ~:
            isRight
                (parseProgram "char str[][4] = { \"abc\", \"def\" }; int main(void) { return str[1][2]; }")
                ~?= True
        , "accepts file-scope typedef-backed nested omitted-bound arrays once the element struct is complete" ~:
            isRight
                (parseProgram "typedef struct S T; struct S { int a; }; T rows[][1] = {{{1}}}; int main(void) { return sizeof rows[0][0]; }")
                ~?= True
        , "accepts file-scope outer omitted-bound arrays whose element type comes from an array typedef" ~:
            isRight
                (parseProgram "typedef int Row[2]; Row rows[] = {{1, 2}}; int main(void) { return rows[0][1]; }")
                ~?= True
        , TestLabel "preserves redeclaration compatibility for file-scope typedef-backed omitted-bound arrays" $
            TestList
                [ "earlier tentative declaration" ~:
                    inferGlobalType "rows" "typedef int Row[2]; Row rows[]; Row rows[] = {{1, 2}};"
                        ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 2] CT.CTInt)
                , "earlier extern declaration" ~:
                    inferGlobalType "rows" "typedef int Row[2]; extern Row rows[]; Row rows[] = {{1, 2}};"
                        ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 2] CT.CTInt)
                , "later extern declaration" ~:
                    inferGlobalType "rows" "typedef int Row[2]; Row rows[] = {{1, 2}}; extern Row rows[];"
                        ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 2] CT.CTInt)
                ]
        , TestLabel "preserves redeclaration compatibility for block-scope extern typedef-backed omitted-bound arrays" $
            TestList
                [ "definition after block-scope extern" ~:
                    inferGlobalType "rows" "typedef int Row[2]; int f(void) { extern Row rows[]; return 0; } Row rows[] = {{1, 2}};"
                        ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 2] CT.CTInt)
                , "definition before block-scope extern" ~:
                    inferGlobalType "rows" "typedef int Row[2]; Row rows[] = {{1, 2}}; int f(void) { extern Row rows[]; return 0; }"
                        ~?= Right (CT.SCAuto $ CT.makeCTArray [1, 2] CT.CTInt)
                ]
        , "accepts GNU omitted-middle conditionals in scalar file-scope initializers" ~:
            isRight (parseProgram "int g = 1 ?: 2; int h = 0 ?: 2; int main(void) { return g == 1 && h == 2; }")
                ~?= True
        , "accepts GNU omitted-middle conditionals in array bounds" ~:
            isRight (parseProgram "int a[42 ?: 7]; int main(void) { return sizeof(a) / sizeof(a[0]) == 42; }")
                ~?= True
        , "accepts bare function designators in file-scope pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int (*fp)(void) = foo;")
                ~?= True
        , "accepts address-of function designators in file-scope pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int (*fp)(void) = &foo;")
                ~?= True
        , "accepts typed null function-pointer casts in file-scope initializers" ~:
            isRight (parseProgram "int (*fp)(void) = (int (*)(void))0; int main(void) { return fp == 0; }")
                ~?= True
        , "accepts explicitly cast function designators in file-scope function-pointer initializers" ~:
            isRight (parseProgram "int foo(int x) { return x; } int (*fp)(void) = (int (*)(void))foo;")
                ~?= True
        , "accepts void-returning bare function designators in file-scope pointer initializers" ~:
            isRight (parseProgram "void helper(void) {} void (*fp)(void) = helper; int main(void) { return fp != 0; }")
                ~?= True
        , "accepts void-returning address-of function designators in file-scope pointer initializers" ~:
            isRight (parseProgram "void helper(void) {} void (*fp)(void) = &helper; int main(void) { return fp != 0; }")
                ~?= True
        , "accepts function designators in aggregate file-scope function-pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int (*fps[1])(void) = { foo }; int main(void) { return fps[0](); }")
                ~?= True
        , "accepts same-file redeclarations that refine empty parameter lists to void prototypes" ~:
            isRight (parseProgram "int foo(); int foo(void) { return 1; }")
                ~?= True
        , "accepts same-file function redeclarations that spell int as signed" ~:
            isRight (parseProgram "int foo(void); signed foo(void); int foo(void) { return 1; }")
                ~?= True
        , "accepts same-file function-pointer redeclarations that refine empty parameter lists to void prototypes" ~:
            isRight (parseProgram "int foo(void) { return 1; } int (*fp)(); int (*fp)(void) = foo;")
                ~?= True
        , "accepts function parameters that shadow file-scope objects" ~:
            isRight (parseProgram "int x; int f(int x) { return x; }")
                ~?= True
        , "accepts function parameters that shadow file-scope typedefs" ~:
            isRight (parseProgram "typedef int T; int f(int T) { return T; }")
                ~?= True
        , "drops function parameters from scope before later file-scope typedef uses" ~:
            isRight (parseProgram "typedef int T; int f(int T) { return T; } T g; int main(void) { return 0; }")
                ~?= True
        , "drops function parameters from scope before later file-scope enumerator uses" ~:
            isRight (parseProgram "enum { N = 1 }; int f(int N) { return N; } int a = N; int main(void) { return a - 1; }")
                ~?= True
        , "rejects prototypes where earlier parameter names hide typedefs" ~:
            isLeft (parseProgram "typedef int T; int f(int T, T x);")
                ~?= True
        , "rejects block-scope locals that redeclare parameters in the same function scope" ~:
            isLeft (parseProgram "int f(int x) { int x; return x; }")
                ~?= True
        , "rejects same-file pointer-to-array redeclarations that complete an omitted pointee bound" ~:
            isLeft (parseProgram "int (*p)[]; int (*p)[4]; int main(void) { return 0; }")
                ~?= True
        , "rejects same-file pointer-to-array redeclarations that disagree on pointee bounds" ~:
            isLeft (parseProgram "int (*p)[3]; int (*p)[4]; int main(void) { return 0; }")
                ~?= True
        , "rejects same-file function redeclarations that refine pointer-to-array parameter bounds" ~:
            isLeft (parseProgram "int f(int (*p)[]); int f(int (*p)[4]); int main(void) { return 0; }")
                ~?= True
        , TestLabel "rejects direct calls on same-file object identifiers" $ TestCase $
            assertProgramErrorContains
                "called object is not a function or function pointer"
                "int foo; int main(void) { return foo(); }"
        , TestLabel "rejects same-file function declarations that reuse a global identifier" $ TestCase $
            assertProgramErrorContains
                "conflicting types for 'bar'"
                "int bar; int bar(void) { return 0; }"
        , TestLabel "rejects same-file global declarations that reuse a function identifier" $ TestCase $
            assertProgramErrorContains
                "redeclaration of 'bar' with no linkage"
                "int bar(void) { return 0; } int bar;"
        , TestLabel "rejects same-file globals that reuse an implicitly declared function identifier" $ TestCase $
            assertProgramErrorContains
                "redeclaration of 'bar' with no linkage"
                "int foo(void) { return bar(); } static int bar;"
        , TestLabel "rejects same-file redeclarations that only match via function-return equality" $ TestCase $
            case parseProgram "int *p; int (*p)(void);" of
                Left err -> assertBool
                    "unexpected error message"
                    (T.isInfixOf "redeclaration of 'p' with no linkage" $ T.pack $ show err)
                Right _ -> assertFailure "expected parse failure"
        , TestLabel "rejects values returned from void function definitions after function-return equality changes" $ TestCase $
            assertProgramErrorContains
                "is void, but the statement returns a value"
                "void f(void) { return 1; }"
        , TestLabel "does not warn for empty returns in void function definitions after function-return equality changes" $ TestCase $
            case parseProgramWarnings "void f(void) { return; }" of
                Left err ->
                    assertFailure $ "unexpected parse error: " <> show err
                Right warnings ->
                    assertBool
                        "unexpected warnings"
                        (SQ.null warnings)
        , "accepts same-file tentative globals that spell int as signed" ~:
            isRight (parseProgram "int x; signed x; int main(void) { return x; }")
                ~?= True
        , "accepts void-pointer function parameters instead of treating them as empty void parameter lists" ~:
            isRight (parseProgram "void free(void*); void f(void *p) { } int main(void) { return 0; }")
                ~?= True
        , "keeps exact void parameter lists valid" ~:
            isRight (parseProgram "int f(void); int main(void) { return 0; }")
                ~?= True
        , "accepts typedef void as unnamed no-parameter prototype" ~:
            isRight (parseProgram "typedef void V; int f(V); int f(void) { return 0; } int main(void) { return f(); }")
                ~?= True
        , "rejects typedef void mixed with other function parameters" ~:
            isLeft (parseProgram "typedef void V; int f(V, int x); int main(void) { return 0; }")
                ~?= True
        , "rejects named typedef void function parameters" ~:
            isLeft (parseProgram "typedef void V; int f(V x); int main(void) { return 0; }")
                ~?= True
        , "rejects void mixed with other function parameters" ~:
            isLeft (parseProgram "int f(void, int x); int main(void) { return 0; }")
                ~?= True
        , "rejects named void function parameters" ~:
            isLeft (parseProgram "int f(void x); int main(void) { return 0; }")
                ~?= True
        , "rejects void array function parameters" ~:
            isLeft (parseProgram "int f(void a[]); int main(void) { return 0; }")
                ~?= True
        , "rejects fixed void array function parameters" ~:
            isLeft (parseProgram "int f(void a[1]); int main(void) { return 0; }")
                ~?= True
        , "rejects unnamed void array function parameters" ~:
            isLeft (parseProgram "int f(void []); int main(void) { return 0; }")
                ~?= True
        , "rejects unnamed fixed void array function parameters" ~:
            isLeft (parseProgram "int f(void [1]); int main(void) { return 0; }")
                ~?= True
        , "rejects unnamed multidimensional void array function parameters" ~:
            isLeft (parseProgram "int f(void [1][1]); int main(void) { return 0; }")
                ~?= True
        , "rejects unnamed incomplete multidimensional void array function parameters" ~:
            isLeft (parseProgram "int f(void [][1]); int main(void) { return 0; }")
                ~?= True
        , "rejects unnamed void array parameters in function definitions" ~:
            isLeft (parseProgram "int f(void [1]) { return 0; } int main(void) { return 0; }")
                ~?= True
        , "rejects multidimensional void array function parameters" ~:
            isLeft (parseProgram "int f(void a[1][1]); int main(void) { return 0; }")
                ~?= True
        , "rejects incomplete multidimensional void array function parameters" ~:
            isLeft (parseProgram "int f(void a[][1]); int main(void) { return 0; }")
                ~?= True
        , "rejects pointers to void array function parameters" ~:
            isLeft (parseProgram "int f(void (*a)[1]); int main(void) { return 0; }")
                ~?= True
        , "accepts array parameters of void pointers" ~:
            isRight (parseProgram "int f(void *a[1]); int main(void) { return 0; }")
                ~?= True
        , "accepts pointer-to-function parameters returning void" ~:
            isRight (parseProgram "int f(void (*cb)(void)); int main(void) { return 0; }")
                ~?= True
        , "accepts multi-word integer type specifiers" ~:
            isRight (parseProgram "long long x; signed int y; signed long z; int main(void) { return sizeof(signed long); }")
                ~?= True
        , "accepts signed long long type specifiers" ~:
            isRight (parseProgram "signed long long x; int long long y; int main(void) { return sizeof(signed long long int); }")
                ~?= True
        , "distinguishes long and long long type equality" ~:
            ((CT.CTLong CT.CTInt :: CT.TypeKind Integer) == CT.CTLong (CT.CTLong CT.CTInt))
                ~?= False
        , "distinguishes short and long long type equality" ~:
            ((CT.CTShort CT.CTInt :: CT.TypeKind Integer) == CT.CTLong (CT.CTLong CT.CTInt))
                ~?= False
        , "rejects incompatible long and long long object redeclarations" ~:
            isLeft (parseProgram "long x; long long x; int main(void) { return 0; }")
                ~?= True
        , "rejects incompatible long and long long function redeclarations" ~:
            isLeft (parseProgram "long f(void); long long f(void); int main(void) { return 0; }")
                ~?= True
        , "rejects incompatible short and long long object redeclarations" ~:
            isLeft (parseProgram "short x; long long x; int main(void) { return 0; }")
                ~?= True
        , "rejects incompatible short and long long function redeclarations" ~:
            isLeft (parseProgram "short f(void); long long f(void); int main(void) { return 0; }")
                ~?= True
        , "accepts equivalent signed long specifier orderings" ~:
            isRight (parseProgram "signed long x; long signed x; int main(void) { return 0; }")
                ~?= True
        , "accepts signed short specifier orderings" ~:
            isRight (parseProgram "signed short a; short signed b; signed short int c; int short signed d; int main(void) { return sizeof(short signed int); }")
                ~?= True
        , "accepts signed integer specifiers as equivalent redeclarations" ~:
            isRight (parseProgram "int i; signed int i; long l; signed long l; short s; signed short s; int main(void) { return 0; }")
                ~?= True
        , TestLabel "rejects invalid repeated type specifiers" $
            TestList
                [ "long long long" ~:
                    isLeft (parseProgram "long long long x; int main(void) { return 0; }")
                        ~?= True
                , "signed signed int" ~:
                    isLeft (parseProgram "signed signed int x; int main(void) { return 0; }")
                        ~?= True
                , "signed signed char" ~:
                    isLeft (parseProgram "signed signed char x; int main(void) { return 0; }")
                        ~?= True
                , "short short int" ~:
                    isLeft (parseProgram "short short int x; int main(void) { return 0; }")
                        ~?= True
                , "int int" ~:
                    isLeft (parseProgram "int int x; int main(void) { return 0; }")
                        ~?= True
                , "short long int" ~:
                    isLeft (parseProgram "short long int x; int main(void) { return 0; }")
                        ~?= True
                , "signed void" ~:
                    isLeft (parseProgram "signed void x; int main(void) { return 0; }")
                        ~?= True
                , "signed _Bool" ~:
                    isLeft (parseProgram "signed _Bool x; int main(void) { return 0; }")
                        ~?= True
                , "long char" ~:
                    isLeft (parseProgram "long char x; int main(void) { return 0; }")
                        ~?= True
                ]
        , TestLabel "rejects unsupported type specifiers" $
            TestList
                [ "unsigned" ~:
                    isLeft (parseProgram "unsigned x; int main(void) { return 0; }")
                        ~?= True
                , "float" ~:
                    isLeft (parseProgram "float x; int main(void) { return 0; }")
                        ~?= True
                , "double" ~:
                    isLeft (parseProgram "double x; int main(void) { return 0; }")
                        ~?= True
                , "_Complex int" ~:
                    isLeft (parseProgram "_Complex int x; int main(void) { return 0; }")
                        ~?= True
                , "_Imaginary int" ~:
                    isLeft (parseProgram "_Imaginary int x; int main(void) { return 0; }")
                        ~?= True
                ]
        , "rejects reserved keywords as declarator identifiers" ~:
            isLeft (parseProgram "int for(void) { return 1; }")
                ~?= True
        , TestLabel "preserves array declarators for function-pointer objects" $ TestCase $
            assertEqual
                "unexpected function-pointer array type"
                (Right $ CT.SCAuto $ CT.CTArray 2 $ CT.CTPtr intFunctionTy)
                (inferGlobalType "fps" "int (*fps[2])(void);")
        , TestLabel "preserves dimension order for parenthesized nested array declarators" $ TestCase $
            assertEqual
                "unexpected parenthesized nested array type"
                (Right $ CT.SCAuto $ CT.CTArray 4 $ CT.CTArray 3 CT.CTChar)
                (firstLocalDeclType "int main(void) { char (x[3])[4]; return sizeof x[0]; }")
        , TestLabel "preserves pointer binding while rebuilding nested array declarators" $ TestCase $
            assertEqual
                "unexpected pointer-to-array element type"
                (Right $ CT.SCAuto $ CT.CTArray 3 $ CT.CTPtr $ CT.CTArray 4 CT.CTInt)
                (inferGlobalType "ptrs" "int (*ptrs[3])[4];")
        , TestLabel "preserves function return declarators when rebuilding nested arrays" $ TestCase $
            assertEqual
                "unexpected function return type"
                (Right $ CT.CTFunc (CT.CTPtr intArrayTy) [(CT.CTVoid, Nothing)])
                (CT.toTypeKind <$> inferFunctionType "f" "int (*f(void))[2];")
        , "rejects bare function designators in non-function-pointer file-scope initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } char *p = foo;")
                ~?= True
        , "rejects address-of function designators in non-function-pointer file-scope initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } char *p = &foo;")
                ~?= True
        , TestLabel "rejects incompatible bare function designators in file-scope function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int (*fp)(void) = foo;"
        , TestLabel "rejects incompatible addressed function designators in file-scope function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int (*fp)(void) = &foo;"
        , TestLabel "rejects incompatible function-pointer values in file-scope function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int (*a)(int) = foo; int (*b)(void) = a;"
        , TestLabel "rejects object-pointer casts of function designators in file-scope function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int (*fp)(void) = (int*)foo;"
        , TestLabel "rejects intermediate object-pointer casts in file-scope function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int (*fp)(void) = (int (*)(void))(int*)foo;"
        , "rejects address constants whose folded addends raise constexpr evaluation errors" ~:
            isLeft (parseProgram "int a[4]; int *p = a + (1 / 0);")
                ~?= True
        , "rejects plain address constants in non-pointer file-scope initializers" ~:
            isLeft (parseProgram "int g; int x = &g;")
                ~?= True
        , "rejects non-pointer casts of address constants in file-scope initializers" ~:
            isLeft (parseProgram "int arr[1]; int x = (int)arr;") ~?= True
        , "rejects non-address pointer casts in pointer file-scope initializers" ~:
            isLeft (parseProgram "char *p = (char*)1;")
                ~?= True
        , "rejects non-zero integer constant expressions in pointer file-scope initializers" ~:
            isLeft (parseProgram "int *p = 1;")
                ~?= True
        , "rejects incompatible self-referential object-pointer file-scope initializers" ~:
            isLeft (parseProgram "int *p = &p;")
                ~?= True
        , "rejects non-zero integer constant expressions in function-pointer file-scope initializers" ~:
            isLeft (parseProgram "int (*fp)(void) = 1 + 1;")
                ~?= True
        , TestLabel "rejects comma expressions in scalar file-scope initializers" $ TestCase $
            assertProgramErrorContains
                "initializer element is not constant"
                "int x = (1, 2);"
        , TestLabel "rejects comma expressions in pointer file-scope initializers" $ TestCase $
            assertProgramErrorContains
                "initializer element is not constant"
                "int x; int *p = (0, &x);"
        , "rejects pointer-typed casts in non-pointer file-scope initializers" ~:
            isLeft (parseProgram "int x = (char*)0;")
                ~?= True
        , "rejects file-scope void objects without initializers" ~:
            isLeft (parseProgram "void x;")
                ~?= True
        , "rejects file-scope void objects with initializers" ~:
            isLeft (parseProgram "void x = 0;")
                ~?= True
        , "rejects file-scope arrays of void" ~:
            isLeft (parseProgram "void a[1];")
                ~?= True
        , "rejects file-scope omitted-bound arrays of void with initializers" ~:
            isLeft (parseProgram "void a[] = {0};")
                ~?= True
        , TestLabel "rejects file-scope initialized arrays whose omitted bound is not outermost" $
            TestList
                [ "fixed outer bound before omitted bound" ~:
                    isLeft (parseProgram "int a[2][][4] = {{{1}}, {{2}}};")
                        ~?= True
                , "additional omitted bound after an outer omitted bound" ~:
                    isLeft (parseProgram "int a[][2][][4] = {{{{1}}}};")
                        ~?= True
                , "typedef-hidden omitted bound in the element type" ~:
                    isLeft (parseProgram "typedef int Row[]; Row a[2] = {{1}, {2}};")
                        ~?= True
                ]
        , TestLabel "rejects file-scope omitted-bound arrays whose ultimate element type is a function" $ TestCase $
            assertProgramErrorContains
                "incomplete type"
                "typedef int F(void); F table[][1] = {{0}};"
        ]

scalarInitializerTest :: Test
scalarInitializerTest = TestLabel "Parser.Program.scalar-initializer" $
    TestList
        [ TestLabel "rejects plain void scalar initializers" $ TestCase $
            case parseProgram "void f(void) {} int main() { int x = f(); return 0; }" of
                Left err -> assertBool
                    "unexpected error message"
                    (T.isInfixOf "void value not ignored as it ought to be" $ T.pack $ show err)
                Right _ -> assertFailure "expected parse failure"
        , TestLabel "rejects braced void scalar initializers" $ TestCase $
            case parseProgram "void f(void) {} int main() { int x = { f() }; return 0; }" of
                Left err -> assertBool
                    "unexpected error message"
                    (T.isInfixOf "void value not ignored as it ought to be" $ T.pack $ show err)
                Right _ -> assertFailure "expected parse failure"
        , "rejects bare function designators in local scalar initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } int main(void) { int x = foo; return x; }")
                ~?= True
        , "rejects addressed function designators in local scalar initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } int main(void) { char *p = &foo; return p != 0; }")
                ~?= True
        , TestLabel "rejects incompatible object-pointer values in local scalar initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int main(void) { int a[2]; char *p = a; return 0; }"
        , TestLabel "rejects multidimensional array expressions in local object-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int main(void) { int a[2][3]; int *p = a; return 0; }"
        , TestLabel "rejects array expressions in local pointer-to-array initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int main(void) { char a[3]; char (*p)[3] = a; return 0; }"
        , TestLabel "rejects small struct lvalues in local scalar initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "struct S { int a; }; int main(void) { struct S s; int x = s; return x; }"
        , TestLabel "rejects small struct call results in local scalar initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "struct S { int a; }; struct S make(void); int main(void) { int x = make(); return x; }"
        , TestLabel "rejects multidimensional array expressions in object-pointer assignments" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int main(void) { int a[2][3]; int *p = 0; p = a; return 0; }"
        , TestLabel "rejects bare arrays in pointer-to-array assignments" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int main(void) { char a[3]; char (*p)[3] = 0; p = a; return 0; }"
        , TestLabel "rejects multidimensional array expressions in object-pointer function arguments" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int sink(int *p) { return p[0]; } int main(void) { int a[2][3]; return sink(a); }"
        , TestLabel "rejects bare arrays in pointer-to-array function arguments" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int sink(char (*p)[3]) { return (*p)[0]; } int main(void) { char a[3]; return sink(a); }"
        , TestLabel "rejects function designators in void-pointer local scalar initializers without a cast" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int helper(void) { return 1; } int main(void) { void *p = helper; return p != 0; }"
        , "accepts function designators cast to object pointers in local scalar initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { char *p = (char *)foo; return p != 0; }")
                ~?= True
        , "accepts function designators cast to void pointers in local scalar initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { void *p = (void *)foo; return p != 0; }")
                ~?= True
        , "accepts function designators cast to integers in local scalar initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { long x = (long)foo; return x != 0; }")
                ~?= True
        , "rejects block-scope void objects without initializers" ~:
            isLeft (parseProgram "int main(void) { void x; return 0; }")
                ~?= True
        , "rejects block-scope void objects with initializers" ~:
            isLeft (parseProgram "int main(void) { void x = 0; return 0; }")
                ~?= True
        , "rejects block-scope arrays of void" ~:
            isLeft (parseProgram "int main(void) { void a[1]; return 0; }")
                ~?= True
        , "rejects block-scope omitted-bound arrays of void with initializers" ~:
            isLeft (parseProgram "int main(void) { void a[] = {0}; return 0; }")
                ~?= True
        , TestLabel "rejects block-scope omitted-bound arrays whose ultimate element type is a function" $ TestCase $
            assertProgramErrorContains
                "incomplete type"
                "typedef int F(void); int main(void) { F table[][1] = {{0}}; return 0; }"
        , "rejects block-scope incomplete arrays without initializers" ~:
            isLeft (parseProgram "int main(void) { int a[]; return 0; }")
                ~?= True
        , "accepts block-scope omitted-bound arrays with initializers" ~:
            isRight (parseProgram "int main(void) { int a[] = {1, 2}; return a[1]; }")
                ~?= True
        , "accepts block-scope nested omitted-bound char arrays with initializers" ~:
            isRight
                (parseProgram "int main(void) { char str[][4] = { \"abc\", \"def\" }; return str[1][2]; }")
                ~?= True
        , "accepts block-scope typedef-backed nested omitted-bound arrays once the element struct is complete" ~:
            isRight
                (parseProgram "typedef struct S T; struct S { int a; }; int main(void) { T rows[][1] = {{{1}}}; return sizeof rows[0][0]; }")
                ~?= True
        , "accepts block-scope outer omitted-bound arrays whose element type comes from an array typedef" ~:
            isRight
                (parseProgram "typedef int Row[2]; int main(void) { Row rows[] = {{1, 2}}; return rows[0][1]; }")
                ~?= True
        , TestLabel "rejects block-scope initialized arrays whose omitted bound is not outermost" $
            TestList
                [ "fixed outer bound before omitted bound" ~:
                    isLeft (parseProgram "int main(void) { int a[2][][4] = {{{1}}, {{2}}}; return 0; }")
                        ~?= True
                , "additional omitted bound after an outer omitted bound" ~:
                    isLeft (parseProgram "int main(void) { int a[][2][][4] = {{{{1}}}}; return 0; }")
                        ~?= True
                , "typedef-hidden omitted bound in the element type" ~:
                    isLeft (parseProgram "typedef int Row[]; int main(void) { Row a[2] = {{1}, {2}}; return 0; }")
                        ~?= True
                ]
        , TestLabel "rejects conditional-wrapped function designators in local scalar initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int main(void) { int x = 1 ? foo : foo; return x; }"
        , "rejects comma-wrapped function designators in local scalar initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } int main(void) { char *p = (0, foo); return p != 0; }")
                ~?= True
        , TestLabel "rejects statement-expression-wrapped function designators in local scalar initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int main(void) { char *p = ({ foo; }); return p != 0; }"
        , "rejects bare function designators in brace-elided member scalar initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } struct S { int x; }; int main(void) { struct S s = { foo }; return s.x; }")
                ~?= True
        , "rejects addressed function designators in brace-elided member scalar initializers" ~:
            isLeft (parseProgram "int foo(void) { return 1; } struct S { char *p; }; int main(void) { struct S s = { &foo }; return s.p != 0; }")
                ~?= True
        , "accepts bare function designators in local function-pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { int (*fp)(void) = foo; return fp(); }")
                ~?= True
        , TestLabel "rejects incompatible bare function designators in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int main(void) { int (*fp)(void) = foo; return 0; }"
        , TestLabel "rejects incompatible addressed function designators in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int main(void) { int (*fp)(void) = &foo; return 0; }"
        , TestLabel "rejects integer constants in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int main(void) { int (*fp)(void) = 1; return 0; }"
        , TestLabel "rejects object pointers in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int main(void) { int x; int *p = &x; int (*fp)(void) = p; return 0; }"
        , TestLabel "rejects object-pointer casts of function designators in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int main(void) { int (*fp)(void) = (int*)foo; return 0; }"
        , TestLabel "rejects intermediate object-pointer casts in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(void) { return 1; } int main(void) { int (*fp)(void) = (int (*)(void))(int*)foo; return 0; }"
        , "accepts addressed function designators in local function-pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { int (*fp)(void) = &foo; return fp(); }")
                ~?= True
        , "accepts dereferenced function pointers in local function-pointer initializers" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { int (*fp)(void) = foo; int (*gp)(void) = *fp; return gp(); }")
                ~?= True
        , "accepts typed null function-pointer casts in local function-pointer initializers" ~:
            isRight (parseProgram "int main(void) { int (*fp)(void) = (int (*)(void))0; return fp == 0; }")
                ~?= True
        , TestLabel "rejects incompatible function-pointer values in local function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int main(void) { int (*a)(int) = foo; int (*b)(void) = a; return 0; }"
        , "accepts void-returning bare function designators in local function-pointer initializers" ~:
            isRight (parseProgram "void helper(void) {} int main(void) { void (*fp)(void) = helper; return fp != 0; }")
                ~?= True
        , "accepts void-returning function designators in aggregate function-pointer initializers" ~:
            isRight (parseProgram "void helper(void) {} int main(void) { void (*fps[1])(void) = { helper }; return fps[0] != 0; }")
                ~?= True
        , TestLabel "rejects incompatible function designators in aggregate function-pointer initializers" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(int x) { return x; } int main(void) { int (*fps[1])(void) = { foo }; return 0; }"
        , TestLabel "tracks indirect call expressions as their return type for sizeof" $ TestCase $
            case parseInitializerAST (CT.SCAuto $ CT.CTLong CT.CTInt) [("fp", intFunctionPtrTy)] "= sizeof fp();" of
                Right
                    ( ATNode
                        (ATBlock [ATNode ATExprStmt _ (ATNode ATAssign _ _ (ATNode ATSizeof _ (ATNode (ATCallPtr Nothing) ty _ _) _)) _])
                        _ _ _
                    ) ->
                    assertEqual "unexpected indirect call result type" (CT.SCAuto CT.CTInt) ty
                Right ast ->
                    assertFailure $ "unexpected AST: " <> show ast
                Left err ->
                    assertFailure $ "unexpected parse error: " <> show err
        , TestLabel "folds _Alignof over indirect calls using the return type" $ TestCase $
            case parseInitializerAST (CT.SCAuto $ CT.CTLong CT.CTInt) [("fp", intFunctionPtrTy)] "= _Alignof fp();" of
                Right
                    ( ATNode
                        (ATBlock [ATNode ATExprStmt _ (ATNode ATAssign _ _ (ATNode ATAlignof _ (ATNode (ATCallPtr Nothing) ty _ _) _)) _])
                        _ _ _
                    ) ->
                    assertEqual
                        "unexpected indirect call result type"
                        (CT.SCAuto CT.CTInt)
                        ty
                Right ast ->
                    assertFailure $ "unexpected AST: " <> show ast
                Left err ->
                    assertFailure $ "unexpected parse error: " <> show err
        ]

functionDesignatorContextTest :: Test
functionDesignatorContextTest = TestLabel "Parser.Program.function-designator-context" $
    TestList
        [ TestLabel "rejects sizeof on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid application of 'sizeof' to function type"
                "int f(void) { return 1; } int main(void) { return sizeof f; }"
        , TestLabel "rejects _Alignof on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid application of '_Alignof' to function type"
                "int f(void) { return 1; } int main(void) { return _Alignof f; }"
        , TestLabel "rejects incrementing bare function designators" $ TestCase $
            assertProgramErrorContains
                "lvalue required as increment operand"
                "int f(void) { return 1; } int main(void) { ++f; return 0; }"
        , TestLabel "rejects assigning to bare function designators" $ TestCase $
            assertProgramErrorContains
                "lvalue required as left operand of assignment"
                "int f(void) { return 1; } int main(void) { f = 0; return 0; }"
        , TestLabel "rejects unary plus on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid application of '+' to function type"
                "int f(void) { return 1; } int main(void) { return +f != 0; }"
        , TestLabel "rejects unary minus on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid application of '-' to function type"
                "int f(void) { return 1; } int main(void) { return -f != 0; }"
        , TestLabel "rejects bitwise not on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid application of '~' to function type"
                "int f(void) { return 1; } int main(void) { return ~f; }"
        , TestLabel "rejects multiplicative operators on bare function designators" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "int f(void) { return 1; } int main(void) { return f * 2; }"
        , TestLabel "rejects multiplicative operators on wrapped function designators" $
            TestList
                [ TestLabel "comma wrapper" $ TestCase $
                    assertProgramErrorContains
                        "invalid operands"
                        "int f(void) { return 1; } int main(void) { return (0, f) * 2; }"
                , TestLabel "conditional wrapper" $ TestCase $
                    assertProgramErrorContains
                        "invalid operands"
                        "int f(void) { return 1; } int main(void) { return (1 ? f : f) / 2; }"
                , TestLabel "statement expression wrapper" $ TestCase $
                    assertProgramErrorContains
                        "invalid operands"
                        "int f(void) { return 1; } int main(void) { return ({ f; }) % 2; }"
                ]
        , TestLabel "accepts multiplicative operators after explicit arithmetic casts of function designators" $
            TestList
                [ "direct integer cast" ~:
                    isRight
                        (parseProgram "int f(void) { return 1; } int main(void) { return ((long)f) * 2; }")
                        ~?= True
                , "boolean cast" ~:
                    isRight
                        (parseProgram "int f(void) { return 1; } int main(void) { return ((_Bool)f) % 2; }")
                        ~?= True
                , "wrapped cast result" ~:
                    isRight
                        (parseProgram "int f(void) { return 1; } int main(void) { return (0, (long)f) / 2; }")
                        ~?= True
                ]
        ]

functionCallTest :: Test
functionCallTest = TestLabel "Parser.Program.function-call" $
    TestList
        [ "accepts repeated dereference of function pointers in indirect calls" ~:
            isRight (parseProgram "int inc(int x) { return x + 1; } int main(void) { int (*fp)(int) = inc; return (**fp)(41) - 42; }")
                ~?= True
        , "accepts typed null function-pointer casts for function-pointer parameters" ~:
            isRight (parseProgram "int use(int (*fp)(void)) { return fp == 0; } int main(void) { return use((int (*)(void))0); }")
                ~?= True
        , TestLabel "rejects old-style function designators for typed function-pointer parameters when promotions change the type" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int use(int (*fp)(char)) { return 0; } int foo(); int main(void) { return use(foo); }"
        , TestLabel "rejects incompatible bare function designators for typed function-pointer parameters" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int use(int (*fp)(void)) { return fp(); } int f(int x) { return x; } int main(void) { return use(f); }"
        , TestLabel "rejects integer constants for typed function-pointer parameters" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int use(int (*fp)(void)) { return fp(); } int main(void) { return use(1); }"
        , TestLabel "rejects non-null integers for typed object-pointer parameters" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int use(int *p) { return p == 0; } int main(void) { return use(1); }"
        , TestLabel "rejects incompatible object pointers for typed pointer parameters" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "int use(char **p) { return p == 0; } int main(void) { int *x = 0; int **pp = &x; return use(pp); }"
        , TestLabel "rejects incompatible by-value struct arguments" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "struct A { int a; }; struct B { int b; }; int use(struct A); int main(void) { struct B b; return use(b); }"
        , TestLabel "rejects too few arguments through typed function pointers" $ TestCase $
            assertProgramErrorContains
                "too few arguments to function call"
                "int inc(int x) { return x + 1; } int main(void) { int (*fp)(int) = inc; return fp(); }"
        , TestLabel "rejects too many arguments through void function pointers" $ TestCase $
            assertProgramErrorContains
                "too many arguments to function call"
                "int zero(void) { return 0; } int main(void) { int (*fp)(void) = zero; return fp(1); }"
        , TestLabel "rejects too many arguments after refining empty parameter lists to void prototypes" $ TestCase $
            assertProgramErrorContains
                "too many arguments to function call"
                "int foo(); int foo(void); int main(void) { return foo(1); }"
        , TestLabel "rejects too many arguments after refining function-pointer redeclarations to void prototypes" $ TestCase $
            assertProgramErrorContains
                "too many arguments to function call"
                "int (*fp)(); int (*fp)(void); int main(void) { return fp(1); }"
        , TestLabel "rejects too few arguments through complete function prototypes" $ TestCase $
            assertProgramErrorContains
                "too few arguments to function call"
                "int inc(int x) { return x + 1; } int main(void) { return inc(); }"
        , TestLabel "rejects too many arguments through void function prototypes" $ TestCase $
            assertProgramErrorContains
                "too many arguments to function call"
                "int zero(void) { return 0; } int main(void) { return zero(1); }"
        ]

conditionalPointerTypeTest :: Test
conditionalPointerTypeTest = TestLabel "Parser.Program.conditional-pointer-type" $
    TestList
        [ "preserves function-pointer results for standard conditionals against null" ~:
            isRight (parseProgram "int foo(void) { return 7; } int main(void) { return (1 ? foo : 0)(); }")
                ~?= True
        , "preserves object-pointer results for standard conditionals against null" ~:
            isRight (parseProgram "int main(void) { int x = 7; int *p = &x; return *(1 ? p : 0); }")
                ~?= True
        , "preserves function-pointer results for GNU omitted conditionals against null" ~:
            isRight (parseProgram "int foo(void) { return 7; } int main(void) { int (*fp)(void) = foo; return (fp ?: 0)(); }")
                ~?= True
        , "preserves object-pointer results for GNU omitted conditionals against null" ~:
            isRight (parseProgram "int main(void) { int x = 7; int *p = &x; return *(p ?: 0); }")
                ~?= True
        , TestLabel "rejects dereferencing standard conditionals that merge void* with object pointers" $ TestCase $
            assertProgramErrorContains
                "void value not ignored as it ought to be"
                "int main(void) { void *vp; int *ip; return *(1 ? vp : ip); }"
        , TestLabel "rejects dereferencing GNU omitted conditionals that merge void* with object pointers" $ TestCase $
            assertProgramErrorContains
                "void value not ignored as it ought to be"
                "int main(void) { void *vp; int *ip; return *(vp ?: ip); }"
        , TestLabel "rejects standard conditionals with incompatible object-pointer operands" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "int main(void) { int x = 7; char y = 3; int *p = &x; char *q = &y; return *(1 ? p : q); }"
        , TestLabel "rejects GNU omitted conditionals with incompatible object-pointer operands" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "int main(void) { int x = 7; char y = 3; int *p = &x; char *q = &y; return *(p ?: q); }"
        , "preserves function-pointer results for standard conditionals against void* null casts" ~:
            isRight
                (parseProgram "int foo(void) { return 7; } int main(void) { int (*fp)(void) = foo; return (1 ? (void*)0 : fp) == 0; }")
                ~?= True
        , "preserves function-pointer results for GNU omitted conditionals against void* null casts" ~:
            isRight
                (parseProgram "int foo(void) { return 7; } int main(void) { int (*fp)(void) = foo; return (fp ?: (void*)0) != 0; }")
                ~?= True
        ]

conditionalAggregateTypeTest :: Test
conditionalAggregateTypeTest = TestLabel "Parser.Program.conditional-aggregate-type" $
    TestList
        [ "accepts compatible small-struct conditional branches" ~:
            isRight
                (parseProgram "struct S { int a; }; int main(void) { struct S a; struct S b; return (1 ? a : b).a; }")
                ~?= True
        , TestLabel "rejects incompatible small-struct conditional branches" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct A { int a; }; struct B { int b; }; int main(void) { struct A a; struct B b; int c = 1; return (c ? a : b).a; }"
        , TestLabel "rejects incomplete-struct and scalar conditional branches before later completion" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S f(); int main(void) { return sizeof(1 ? f() : 0); } struct S { int a; };"
        ]

aggregateConditionTest :: Test
aggregateConditionTest = TestLabel "Parser.Program.aggregate-condition" $
    TestList
        [ TestLabel "rejects small-struct if conditions" $ TestCase $
            assertProgramErrorContains
                "invalid condition type"
                "struct S { int a; }; int main(void) { struct S s; if (s) return 1; return 0; }"
        , TestLabel "rejects small-struct while conditions" $ TestCase $
            assertProgramErrorContains
                "invalid condition type"
                "struct S { int a; }; int main(void) { struct S s; while (s) return 1; return 0; }"
        , TestLabel "rejects small-struct for conditions" $ TestCase $
            assertProgramErrorContains
                "invalid condition type"
                "struct S { int a; }; int main(void) { struct S s; for (; s; ) return 1; return 0; }"
        , TestLabel "rejects small-struct switch conditions" $ TestCase $
            assertProgramErrorContains
                "invalid condition type"
                "struct S { int a; }; int main(void) { struct S s; switch (s) { default: return 0; } }"
        , TestLabel "rejects small-struct conditional-operator conditions" $ TestCase $
            assertProgramErrorContains
                "invalid condition type"
                "struct S { int a; }; int main(void) { struct S s; return s ? 1 : 2; }"
        ]

aggregateScalarOperatorTest :: Test
aggregateScalarOperatorTest = TestLabel "Parser.Program.aggregate-scalar-operator" $
    TestList
        [ TestLabel "rejects small-struct logical operands" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return s || 0; }"
        , TestLabel "rejects small-struct equality operands" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return s == s; }"
        , TestLabel "rejects small-struct relational operands" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return s < s; }"
        , TestLabel "rejects unary logical not on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return !s; }"
        , TestLabel "rejects unary plus on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return +s; }"
        , TestLabel "rejects bitwise not on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return ~s; }"
        , TestLabel "rejects multiplicative operators on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; return s * 1; }"
        , TestLabel "rejects pre-increment on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; ++s; return 0; }"
        , TestLabel "rejects post-increment on small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "struct S { int a; }; int main(void) { struct S s; s++; return 0; }"
        , TestLabel "rejects compound assignments to small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "struct S { int a; }; int main(void) { struct S s; s += 1; return 0; }"
        , TestLabel "rejects compound assignments from small structs" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "struct S { int a; }; int main(void) { int x; struct S s; x += s; return x; }"
        ]

explicitAggregateCastTest :: Test
explicitAggregateCastTest = TestLabel "Parser.Program.explicit-aggregate-cast" $
    TestList
        [ TestLabel "rejects scalar-to-struct casts before member access" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct S { int a; }; int main(void) { return ((struct S)42).a; }"
        , TestLabel "rejects struct casts that hide incompatible call arguments" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct A { int a; }; struct B { int b; }; int use(struct A); int main(void) { struct B b; return use((struct A)b); }"
        , TestLabel "rejects struct casts that hide incompatible return values" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct A { int a; }; struct B { int b; }; struct A make(void) { struct B b; return (struct A)b; } int main(void) { return 0; }"
        , TestLabel "rejects struct casts that hide incompatible assignments" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct A { int a; }; struct B { int b; }; int main(void) { struct A a; struct B b; a = (struct A)b; return 0; }"
        , TestLabel "rejects struct-to-scalar casts" $ TestCase $
            assertProgramErrorContains
                "invalid cast operand"
                "struct S { int a; }; int main(void) { struct S s; return (int)s; }"
        ]

functionPointerAssignmentTest :: Test
functionPointerAssignmentTest = TestLabel "Parser.Program.function-pointer-assignment" $
    TestList
        [ "accepts compatible function-pointer assignments from variables" ~:
            isRight (parseProgram "int foo(void) { return 1; } int main(void) { int (*a)(void) = foo; int (*b)(void) = 0; b = a; return b(); }")
                ~?= True
        , "accepts typed null function-pointer casts in function-pointer assignments" ~:
            isRight (parseProgram "int main(void) { int (*fp)(void) = 0; fp = (int (*)(void))0; return fp == 0; }")
                ~?= True
        , TestLabel "rejects old-style function declarations in typed function-pointer initializers when promotions change the type" $ TestCase $
            assertProgramErrorContains
                "invalid initializer for scalar object"
                "int foo(); int main(void) { int (*fp)(char) = foo; return 0; }"
        , TestLabel "rejects assigning bare function designators to ordinary scalars" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int helper(void) { return 1; } int main(void) { int x; x = helper; return 0; }"
        , TestLabel "rejects assigning incompatible object-pointer values" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int main(void) { char *p = 0; int a[2]; p = a; return 0; }"
        , TestLabel "rejects assigning function designators to object pointers without a cast" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int helper(void) { return 1; } int main(void) { void *p = 0; p = helper; return 0; }"
        , TestLabel "rejects incompatible bare function designators in function-pointer assignments" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int foo(int x) { return x; } int main(void) { int (*fp)(void) = 0; fp = foo; return 0; }"
        , TestLabel "rejects assigning old-style function pointers to typed function pointers when promotions change the type" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int foo(); int main(void) { int (*src)() = foo; int (*dst)(char) = 0; dst = src; return 0; }"
        , TestLabel "rejects incompatible function-pointer values in function-pointer assignments" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int foo(int x) { return x; } int main(void) { int (*a)(int) = foo; int (*b)(void) = 0; b = a; return 0; }"
        , TestLabel "rejects assigning object pointers to function pointers" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "int main(void) { int x; int *p = &x; int (*fp)(void) = 0; fp = p; return 0; }"
        ]

postfixMemberAccessTest :: Test
postfixMemberAccessTest = TestLabel "Parser.Program.postfix-member-access" $
    TestList
        [ "accepts direct struct member access" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; int b; } x; x.a = 1; x.b = 2; return x.a + x.b; }")
                ~?= True
        , "accepts pointer struct member access" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; } x; struct S *p = &x; p->a = 42; return x.a; }")
                ~?= True
        , "accepts pointer member access on pointer rvalues" ~:
            isRight
                (parseProgram "struct S { int a; }; struct S *makep(void); int main(void) { return makep()->a; }")
                ~?= True
        , "accepts chained pointer member access" ~:
            isRight
                (parseProgram "int main(void) { struct Y { int a; }; struct X { struct Y *py; }; struct X x; struct Y y; y.a = 42; x.py = &y; return x.py->a; }")
                ~?= True
        , "accepts pointer member access through addressable array members" ~:
            isRight
                (parseProgram "int main(void) { struct T { int a; }; struct S { struct T arr[1]; }; struct S x; x.arr[0].a = 42; return x.arr->a; }")
                ~?= True
        , "accepts direct member access through explicit dereference of large struct pointers" ~:
            isRight
                (parseProgram "struct S { int a; int b; int c; }; struct S *p; int main(void) { return (*p).c; }")
                ~?= True
        , "continues postfix parsing after direct member access" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a[2]; } x; x.a[1] = 42; return x.a[1]; }")
                ~?= True
        , "accepts direct member access on struct rvalues in unevaluated contexts" ~:
            isRight
                (parseProgram "struct S { int a; }; struct S make(void); int main(void) { return sizeof(make().a); }")
                ~?= True
        , "accepts direct member access on assignment rvalues in unevaluated contexts" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; } x; struct S y; return sizeof((x = y).a); }")
                ~?= True
        , "accepts direct member access on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a; }; struct S make(void); int main(void) { return make().a; }")
                ~?= True
        , "accepts direct member access on assignment struct rvalues" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; } x; struct S y; return (x = y).a; }")
                ~?= True
        , "accepts array member access on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a[1]; }")
                ~?= True
        , "accepts unary dereference of array member access on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return *make().a; }")
                ~?= True
        , "accepts unary dereference of array member pointer arithmetic on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return *(make().a + 1); }")
                ~?= True
        , "accepts unary dereference of array member pointer subtraction on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return *(make().a - 0); }")
                ~?= True
        , "accepts member access when a discarded comma operand reads a scalar array element on a struct rvalue" ~:
            isRight
                (parseProgram "struct S { int a[1]; int b; }; struct S make(void); int main(void) { return (make().a[0], make()).b; }")
                ~?= True
        , "accepts array member access on assignment struct rvalues" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a[2]; } x; struct S y; return (x = y).a[1]; }")
                ~?= True
        , "accepts nested aggregate member access on function-call struct rvalues" ~:
            isRight
                (parseProgram "struct T { char a; char b; char c; }; struct S { char pad; struct T t; }; struct S make(void); int main(void) { return make().t.a; }")
                ~?= True
        , "accepts sizeof nested array member access on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2][2]; }; struct S make(void); int main(void) { return sizeof(make().a[1][0]); }")
                ~?= True
        , "accepts sizeof aggregate member access on large struct rvalues" ~:
            isRight
                (parseProgram "struct T { int a; int b; int c; }; struct S { struct T t; }; struct S make(void); int main(void) { return sizeof(make().t); }")
                ~?= True
        , "accepts sizeof member access on large assignment rvalues" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; int b; int c; } x; struct S y; return sizeof((x = y).a); }")
                ~?= True
        , "accepts sizeof calls with large by-value struct arguments" ~:
            isRight
                (parseProgram "struct S { int a; int b; int c; }; int sink(struct S x); int main(void) { struct S x; return sizeof(sink(x)); }")
                ~?= True
        , TestLabel "rejects sizeof calls with incompatible forward-declared struct arguments" $ TestCase $
            assertProgramErrorContains
                "invalid argument type to function call"
                "struct A; struct B; extern struct B b; int sink(struct A); int main(void) { return sizeof(sink(b)); } struct A { int a; }; struct B { int b; };"
        , "accepts address-of large struct lvalues" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; int b; int c; } x; struct S *p = &x; return 0; }")
                ~?= True
        , "accepts sizeof large struct lvalues" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a; int b; int c; } x; return sizeof(x); }")
                ~?= True
        , "accepts address-of addressable array member elements" ~:
            isRight
                (parseProgram "int main(void) { struct S { int a[2]; } x; int *p = &x.a[1]; return 0; }")
                ~?= True
        , "accepts sizeof address-of array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return sizeof(&make().a[1]); }")
                ~?= True
        , "accepts _Alignof address-of array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return _Alignof(&make().a[1]); }")
                ~?= True
        , "accepts sizeof assignment through array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return sizeof(make().a[1] = 1); }")
                ~?= True
        , "accepts _Alignof assignment through array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return _Alignof(make().a[1] = 1); }")
                ~?= True
        , "accepts sizeof post-increment through array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return sizeof(make().a[1]++); }")
                ~?= True
        , "accepts _Alignof pre-decrement through array member elements on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return _Alignof(--make().a[1]); }")
                ~?= True
        , "accepts sizeof post-increment through chained array member pointer arithmetic on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return sizeof(((make().a + 1)[0])++); }")
                ~?= True
        , "accepts _Alignof compound assignment through chained array member pointer arithmetic on struct rvalues" ~:
            isRight
                (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return _Alignof(((make().a + 1)[0]) += 1); }")
                ~?= True
        , TestLabel "rejects sizeof assignment to array-typed rvalue array elements" $ TestCase $
            assertProgramErrorContains
                "lvalue required as left operand of assignment"
                "struct S { int a[2][2]; }; struct S make(void); int main(void) { return sizeof(make().a[1] = make().a[0]); }"
        , TestLabel "rejects _Alignof assignment to array-typed rvalue array elements" $ TestCase $
            assertProgramErrorContains
                "lvalue required as left operand of assignment"
                "struct S { int a[2][2]; }; struct S make(void); int main(void) { return _Alignof(make().a[1] = make().a[0]); }"
        , TestLabel "rejects sizeof increment of array-typed rvalue array elements" $ TestCase $
            assertProgramErrorContains
                "lvalue required as increment operand"
                "struct S { int a[2][2]; }; struct S make(void); int main(void) { return sizeof(make().a[1]++); }"
        , TestLabel "rejects _Alignof decrement of array-typed rvalue array elements" $ TestCase $
            assertProgramErrorContains
                "lvalue required as decrement operand"
                "struct S { int a[2][2]; }; struct S make(void); int main(void) { return _Alignof(--make().a[1]); }"
        , "rejects sizeof address-of scalar rvalues" ~:
            isLeft (parseProgram "int main(void) { return sizeof(&1); }")
                ~?= True
        , "accepts address-of function designators" ~:
            isRight
                (parseProgram "int foo(void); int main(void) { int (*p)(void) = &foo; return 0; }")
                ~?= True
        , "accepts compatible small struct assignments and returns" ~:
            isRight
                (parseProgram "struct S { int a; }; struct S make(void) { struct S x; return x; } int main(void) { struct S x; struct S y; x = y; x = make(); return x.a; }")
                ~?= True
        , "rejects decaying array member access from struct rvalues in comma subscripts" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return (0, make().a)[1]; }")
                ~?= True
        , "rejects assigning array member decay from struct rvalues to pointers" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p; return (p = make().a, p[1]); }")
                ~?= True
        , "rejects initializing pointers from array member decay on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p = make().a; return 0; }")
                ~?= True
        , "rejects pointer arithmetic on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p = make().a + 1; return p[0]; }")
                ~?= True
        , "rejects nested array member subscripts on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2][2]; }; struct S make(void); int main(void) { return make().a[1][0]; }")
                ~?= True
        , "rejects member access through aggregate array elements on struct rvalues" ~:
            isLeft (parseProgram "struct T { int x; }; struct S { struct T a[2]; }; struct S make(void); int main(void) { return make().a[0].x; }")
                ~?= True
        , "rejects pointer member access through array member decay on struct rvalues" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported non-addressable array member decay"
                    "struct T { int a; }; struct S { struct T arr[1]; }; struct S make(void); int main(void) { return make().arr->a; }"
        , TestLabel "rejects pointer member bases read from large struct rvalues" $ TestCase $
            assertProgramErrorContains
                "unsupported non-addressable array member decay"
                "struct T { int a; }; struct S { int pad0; int pad1; struct T *p; }; struct S make(void); int main(void) { return make().p[0].a; }"
        , TestLabel "rejects address-of paths requiring pointer reads from large struct rvalues" $ TestCase $
            assertProgramErrorContains
                "unsupported non-addressable array member decay"
                "struct T { int a; }; struct S { int pad0; int pad1; struct T *p; }; struct S make(void); int main(void) { int *q = &make().p[0].a; return 0; }"
        , TestLabel "rejects assignments through paths requiring pointer reads from large struct rvalues" $ TestCase $
            assertProgramErrorContains
                "unsupported non-addressable array member decay"
                "struct T { int a; }; struct S { int pad0; int pad1; struct T *p; }; struct S make(void); int main(void) { make().p[0].a = 1; return 0; }"
        , "rejects aggregate member values on struct rvalues" ~:
            isLeft (parseProgram "struct T { int a; int b; int c; }; struct S { struct T t; }; struct S make(void); int main(void) { return make().t; }")
                ~?= True
        , "rejects scalar member access on large struct rvalues" ~:
            isLeft (parseProgram "struct S { int a; int b; int c; }; struct S make(void); int main(void) { return make().c; }")
                ~?= True
        , "rejects expression statements with large struct values" ~:
            isLeft (parseProgram "int main(void) { struct S { int a; int b; int c; } x; x; return 0; }")
                ~?= True
        , "rejects assigning large struct values" ~:
            isLeft (parseProgram "int main(void) { struct S { int a; int b; int c; } x; struct S y; x = y; return 0; }")
                ~?= True
        , TestLabel "rejects incompatible small struct assignments" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "struct A { int a; }; struct B { int b; }; int main(void) { struct A a; struct B b; a = b; return 0; }"
        , TestLabel "rejects assigning small structs to scalars" $ TestCase $
            assertProgramErrorContains
                "invalid operands to assignment"
                "struct S { int a; }; int main(void) { int x; struct S s; x = s; return x; }"
        , "rejects assigning large function-returned struct values" ~:
            isLeft (parseProgram "struct S { int a; int b; int c; }; struct S make(void); int main(void) { struct S x; x = make(); return 0; }")
                ~?= True
        , "rejects returning large struct values" ~:
            isLeft (parseProgram "struct S { int a; int b; int c; }; struct S make(void) { struct S x; return x; }")
                ~?= True
        , TestLabel "rejects large struct return definitions with empty returns" $ TestCase $
            assertProgramErrorContains
                "unsupported by-value function return type"
                "struct S { int a; int b; int c; }; struct S make(void) { return; }"
        , TestLabel "rejects delayed large struct return definitions with empty returns" $ TestCase $
            assertProgramErrorContains
                "unsupported by-value function return type"
                "struct S make(void) { return; } struct S { int a; int b; int c; };"
        , TestLabel "rejects large struct return definitions without returns" $ TestCase $
            assertProgramErrorContains
                "unsupported by-value function return type"
                "struct S { int a; int b; int c; }; struct S make(void) {}"
        , TestLabel "rejects cast-produced large struct return values" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct S { int a; int b; int c; }; struct S make(void) { return (struct S)0; }"
        , TestLabel "rejects incompatible small struct return values" $ TestCase $
            assertProgramErrorContains
                "invalid return type"
                "struct A { int a; }; struct B { int b; }; struct A make(void) { struct B b; return b; } int main(void) { return 0; }"
        , TestLabel "rejects returning small structs from scalar functions" $ TestCase $
            assertProgramErrorContains
                "invalid return type"
                "struct S { int a; }; int make(void) { struct S s; return s; } int main(void) { return 0; }"
        , TestLabel "rejects nested aggregate returns inside returned statement expressions" $ TestCase $
            assertProgramErrorContains
                "invalid return type"
                "struct S { int a; }; int main(void) { return ({ struct S s; return s; 0; }); }"
        , "accepts aggregate returns inside sizeof statement expressions" ~:
            isRight
                (parseProgram "struct S { int a; }; int main(void) { return sizeof(({ struct S s; return s; 0; })); }")
                ~?= True
        , "accepts aggregate returns inside _Alignof statement expressions" ~:
            isRight
                (parseProgram "struct S { int a; }; int main(void) { return _Alignof(({ struct S s; return s; 0; })); }")
                ~?= True
        , "rejects function call arguments with large struct values" ~:
            isLeft (parseProgram "struct S { int a; int b; int c; }; int sink(struct S x); int main(void) { struct S x; return sink(x); }")
                ~?= True
        , TestLabel "rejects cast-produced large struct call arguments" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct S { int a; int b; int c; }; int sink(struct S x); int main(void) { return sink((struct S)0); }"
        , TestLabel "rejects cast-produced large struct old-style call arguments" $ TestCase $
            assertProgramErrorContains
                "invalid cast type"
                "struct S { int a; int b; int c; }; int sink(); int main(void) { return sink((struct S)0); }"
        , "rejects direct call arguments with escaping statement-expression control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "struct S { int a[2]; }; struct S make(void); int sink(int x); int main(void) { for (;;) { sink(make().a[({ continue; 0; })]); } return 0; }"
        , "rejects indirect call arguments with escaping statement-expression control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "struct S { int a[2]; }; struct S make(void); int sink(int x); int main(void) { int (*fp)(int) = sink; for (;;) { fp(make().a[({ continue; 0; })]); } return 0; }"
        , "rejects direct call arguments with statement-expression return control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "int sink(int x); int main(void) { return sink(({ return 2; 0; })); }"
        , "rejects indirect call arguments with statement-expression return control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "int sink(int x); int main(void) { int (*fp)(int) = sink; return fp(({ return 2; 0; })); }"
        , "rejects indirect call callees with escaping statement-expression control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call callee"
                    "int sink(int x); int main(void) { int (*fp)(int) = sink; for (;;) { ({ continue; fp; })(1); } return 0; }"
        , "rejects indirect call callees with statement-expression return control flow" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call callee"
                    "int sink(int x); int main(void) { int (*fp)(int) = sink; ({ return 2; fp; })(1); return 0; }"
        , "accepts no-argument indirect callees with statement-expression return control flow" ~:
            isRight (parseProgram "int sink(void); int main(void) { int (*fp)(void) = sink; ({ return 2; fp; })(); return 0; }")
                ~?= True
        , "rejects direct call arguments that jump into nested call argument labels" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "int sink(int x); int main(void) { return sink(({ goto L; sink(({ L: 2; })); 3; })); }"
        , "rejects indirect call arguments that jump into nested call argument labels" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in function call argument"
                    "int sink(int x); int main(void) { int (*fp)(int) = sink; return fp(({ goto L; fp(({ L: 2; })); 3; })); }"
        , "rejects rvalue array member bases with escaping statement-expression control flow" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { for (;;) { ({ continue; make(); }).a[0]; } return 0; }")
                ~?= True
        , "accepts rvalue array member bases with statement-expression return control flow" ~:
            isRight (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return ({ return 5; make(); }).a[0]; }")
                ~?= True
        , "accepts call arguments with statement-expression-local loop control flow" ~:
            isRight (parseProgram "int sink(int x); int main(void) { return sink(({ while (1) { break; } 1; })); }")
                ~?= True
        , "accepts statement-expression return control flow" ~:
            isRight (parseProgram "int main(void) { ({ 1; return 2; 3; }); return 4; }")
                ~?= True
        , "accepts direct call arguments with unevaluated statement-expression control flow" ~:
            isRight (parseProgram "int sink(int x); int main(void) { for (;;) { return sink(sizeof(({ continue; 0; }))); } return 0; }")
                ~?= True
        , "accepts indirect call arguments with unevaluated statement-expression control flow" ~:
            isRight (parseProgram "int sink(int x); int main(void) { int (*fp)(int) = sink; for (;;) { return fp(_Alignof(({ continue; 0; }))); } return 0; }")
                ~?= True
        , "accepts statement-expression call arguments with nested unevaluated control flow" ~:
            isRight (parseProgram "int sink(int x); int main(void) { for (;;) { return sink(({ sizeof(({ continue; 0; })); 1; })); } return 0; }")
                ~?= True
        , "accepts unevaluated statement-expression-local cross-boundary goto" ~:
            isRight (parseProgram "int main(void) { return sizeof(({ goto L; ({ L: 1; }); 0; })); }")
                ~?= True
        , "rejects goto into statement-expression labels" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in statement expression"
                    "int main(void) { goto L; int x; x = ({ L: 1; }); return x; }"
        , "rejects goto into unevaluated statement-expression labels" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in statement expression"
                    "int main(void) { goto L; return sizeof(({ L: 3; })); }"
        , "rejects case labels inside statement expressions entered by outer switches" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in statement expression"
                    "int main(void) { switch (1) { ({ case 1: 0; 1; }); } return 0; }"
        , "rejects default labels inside statement expressions entered by outer switches" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in statement expression"
                    "int main(void) { switch (0) { ({ default: 0; 1; }); } return 0; }"
        , "accepts unevaluated case labels inside statement expressions under outer switches" ~:
            isRight (parseProgram "int main(void) { switch (1) { default: return sizeof(({ case 1: 0; 0; })); } return 0; }")
                ~?= True
        , "accepts unevaluated default labels inside statement expressions under outer switches" ~:
            isRight (parseProgram "int main(void) { switch (0) { case 0: return _Alignof(({ default: 0; 0; })); } return 0; }")
                ~?= True
        , "accepts switch labels inside statement-expression-local switches" ~:
            isRight (parseProgram "int main(void) { return ({ switch (1) { case 1: 2; default: 3; } 4; }); }")
                ~?= True
        , "accepts postfix subscript after array member pointer arithmetic on struct rvalues" ~:
            isRight (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return (make().a + 1)[0]; }")
                ~?= True
        , TestLabel "rejects chained array member pointer arithmetic on large struct rvalues" $ TestCase $
            assertProgramErrorContains
                "unsupported non-addressable array member decay"
                "struct S { int pad; int a[2]; }; struct S make(void); int main(void) { return (make().a + 1)[0]; }"
        , TestLabel "rejects nested array member pointer arithmetic on large struct rvalues" $ TestCase $
            assertProgramErrorContains
                "unsupported non-addressable array member decay"
                "struct S { int pad; int a[2]; }; struct S make(void); int main(void) { return *((make().a + 1) + 0); }"
        , "rejects escaping array member pointer arithmetic on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p = make().a + 1; return 0; }")
                ~?= True
        , "rejects escaping statement-expression control flow in assignment operands" ~:
            TestCase $
                assertProgramErrorContains
                    "unsupported control flow in statement expression"
                    "int main(void) { int x; for (;;) { x = ({ continue; 1; }); } return 0; }"
        , TestLabel "rejects evaluated statement-expression statements under unary dereference" $ TestCase $
            assertProgramErrorContains
                "lvalue required as left operand of assignment"
                "struct S { int a[2]; }; struct S make(void); int g; int main(void) { return *({ make().a[1] = 1; &g; }); }"
        , "rejects conditionals on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a ? 1 : 2; }")
                ~?= True
        , "rejects logical operators on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a || 0; }")
                ~?= True
        , "rejects equality operators on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a == 0; }")
                ~?= True
        , "rejects unary scalar conversion on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return !make().a; }")
                ~?= True
        , "rejects multiplicative operators on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a * 1; }")
                ~?= True
        , "rejects shifts on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { return make().a << 1; }")
                ~?= True
        , "rejects if conditions on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { if (make().a) return 1; return 0; }")
                ~?= True
        , "rejects while conditions on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { while (make().a) return 1; return 0; }")
                ~?= True
        , "rejects switch conditions on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { switch (make().a) { default: return 0; } }")
                ~?= True
        , "rejects for conditions on array member decay from struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { for (; make().a; ) return 1; return 0; }")
                ~?= True
        , "rejects address-of array members on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p = &make().a; return 0; }")
                ~?= True
        , "rejects address-of array member elements on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { int *p = &make().a[1]; return 0; }")
                ~?= True
        , "rejects address-of members through aggregate array elements on struct rvalues" ~:
            isLeft (parseProgram "struct T { int x; }; struct S { struct T a[2]; }; struct S make(void); int main(void) { int *p = &make().a[0].x; return 0; }")
                ~?= True
        , "rejects direct member access on non-struct expressions" ~:
            isLeft (parseProgram "int main(void) { int x; return x.a; }")
                ~?= True
        , "rejects pointer member access on non-pointer expressions" ~:
            isLeft (parseProgram "int main(void) { struct S { int a; } x; return x->a; }")
                ~?= True
        , "rejects assigning through direct member access on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a; }; struct S make(void); int main(void) { make().a = 1; return 0; }")
                ~?= True
        , "rejects assigning through direct member access on assignment rvalues" ~:
            isLeft (parseProgram "int main(void) { struct S { int a; } x; (x = x).a = 1; return 0; }")
                ~?= True
        , "rejects assigning through array member access on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { make().a[1] = 1; return 0; }")
                ~?= True
        , "rejects incrementing through array member access on struct rvalues" ~:
            isLeft (parseProgram "struct S { int a[2]; }; struct S make(void); int main(void) { make().a[1]++; return 0; }")
                ~?= True
        , TestLabel "rejects incrementing through chained array member pointer arithmetic on struct rvalues" $ TestCase $
            assertProgramErrorContains
                "lvalue required as increment operand"
                "struct S { int a[2]; }; struct S make(void); int main(void) { ((make().a + 1)[0])++; return 0; }"
        , TestLabel "rejects compound assignment through chained array member pointer arithmetic on struct rvalues" $ TestCase $
            assertProgramErrorContains
                "lvalue required as left operand of assignment"
                "struct S { int a[2]; }; struct S make(void); int main(void) { ((make().a + 1)[0]) += 1; return 0; }"
        ]

declarationSpecifierTest :: Test
declarationSpecifierTest = TestLabel "Parser.Program.declaration-specifier" $
    TestList
        [ "accepts typedef declarations and typedef-names in later declarations" ~:
            isRight
                (parseProgram "typedef int myint; myint x; int main(void) { myint y; return 0; }")
                ~?= True
        , "accepts typedef names for incomplete struct tags" ~:
            isRight
                (parseProgram "struct Node; typedef struct Node Node; Node *next; int main(void) { return 0; }")
                ~?= True
        , "rejects typedef declarations combined with static" ~:
            isLeft
                (parseProgram "typedef static int T; int main(void) { return 0; }")
                ~?= True
        , "rejects typedef declarations combined with register" ~:
            isLeft
                (parseProgram "typedef register int T; int main(void) { return 0; }")
                ~?= True
        , "rejects typedef declarations combined with auto" ~:
            isLeft
                (parseProgram "typedef auto int T; int main(void) { return 0; }")
                ~?= True
        , "rejects multiple ordinary storage-class specifiers" ~:
            isLeft
                (parseProgram "static register int x; int main(void) { return 0; }")
                ~?= True
        , "accepts extern function declarations" ~:
            isRight
                (parseProgram "extern int puts(); int main(void) { return 0; }")
                ~?= True
        , "accepts ignored include directives" ~:
            isRight
                (parseProgram "#include <stdio.h>\nint main(void) { return 0; }")
                ~?= True
        , "accepts long ignored include directives" ~:
            isRight
                (parseProgram $ "#include <" <> T.replicate 1000 "a" <> ">\nint main(void) { return 0; }")
                ~?= True
        , "accepts whitespace-prefixed ignored include directives" ~:
            isRight
                (parseProgram "  #include <stdio.h>\nint main(void) { return 0; }")
                ~?= True
        , "accepts whitespace-prefixed ignored include directives after code" ~:
            isRight
                (parseProgram "int x;\n  #include <stdio.h>\nint main(void) { return 0; }")
                ~?= True
        , "rejects include directives outside preprocessing-line position" ~:
            isLeft
                (parseProgram "int main(void) { return 0; #include <stdio.h>\n}")
                ~?= True
        , "accepts extern declarations when the storage-class follows the type specifier" ~:
            isRight
                (parseProgram "int extern x; int main(void) { return x; }")
                ~?= True
        , "accepts typedef declarations when the storage-class follows the type specifier" ~:
            isRight
                (parseProgram "int typedef U; U x; int main(void) { return 0; }")
                ~?= True
        , "accepts signed typedef declarations when the storage-class follows the type specifier" ~:
            isRight
                (parseProgram "signed typedef U; U x; int main(void) { return 0; }")
                ~?= True
        , "rejects unsupported unsigned typedef declarations without crashing" ~:
            isLeft
                (parseProgram "unsigned typedef U; U x; int main(void) { return 0; }")
                ~?= True
        , "does not leak block-scope extern function declarations" ~:
            hasFunctionBinding "foo" "int main(void) { extern int foo(void); return 0; }"
                ~?= Right False
        , "accepts block-scope function prototypes without registering local function objects" ~:
            isRight
                (parseProgram "int main(void) { int foo(void); return foo(); }")
                ~?= True
        , "does not leak block-scope ordinary function declarations" ~:
            hasFunctionBinding "foo" "int main(void) { int foo(void); return foo(); }"
                ~?= Right False
        , "accepts block-scope auto object declarations" ~:
            isRight
                (parseProgram "int main(void) { auto int x; x = 0; return x; }")
                ~?= True
        , "rejects block-scope auto function declarations" ~:
            isLeft
                (parseProgram "int main(void) { auto int foo(void); return 0; }")
                ~?= True
        , "rejects block-scope static function declarations" ~:
            isLeft
                (parseProgram "int main(void) { static int foo(void); return 0; }")
                ~?= True
        , "rejects block-scope register function declarations" ~:
            isLeft
                (parseProgram "int main(void) { register int foo(void); return 0; }")
                ~?= True
        , "keeps extern object declarations as declaration-only globals" ~:
            inferGlobalInitWith "x" "extern int x; int main(void) { return 0; }"
                ~?= Right PV.GVarInitWithExternDecl
        , "re-resolves completed struct tags for extern object uses" ~:
            isRight
                (parseProgram "extern struct S x; struct S { int a; }; int main(void) { return sizeof(x); }")
                ~?= True
        , "re-resolves completed nested struct tags for extern object uses" ~:
            isRight
                (parseProgram "extern struct S (*p)[1]; struct S { int a; }; int main(void) { return sizeof(**p); }")
                ~?= True
        , "preserves deferred extern object struct bindings across inner forward declarations" ~:
            isRight
                (parseProgram "extern struct S x; struct S { int a; }; int main(void) { { struct S; return sizeof(x); } }")
                ~?= True
        , "normalizes completed typedef-backed function declarations before registration" ~:
            isRight
                (parseProgram "typedef struct S T; struct S { int a; }; T foo(void); int main(void) { return sizeof(foo()); }")
                ~?= True
        , "preserves deferred typedef-backed struct bindings across inner forward declarations" ~:
            isRight
                (parseProgram "typedef struct S T; struct S { int a; }; int main(void) { { struct S; T x; return sizeof(x); } }")
                ~?= True
        , "preserves deferred function return struct bindings across inner forward declarations" ~:
            isRight
                (parseProgram "struct S foo(void); struct S { int a; }; int main(void) { { struct S; return sizeof(foo()); } }")
                ~?= True
        , "re-resolves completed typedef-backed local object types before storing them" ~:
            isRight
                (parseProgram "int main(void) { typedef struct S T; struct S { int a; }; T x; return sizeof(x); }")
                ~?= True
        , "preserves function-definition parameter-scope struct tags for the body" ~:
            isRight
                (parseProgram "int f(struct S { int x; } a) { struct S b; return sizeof(b); } int main(void) { return 0; }")
                ~?= True
        , "preserves outer parameter-scope struct tags when nested function-pointer parameters add prototype scopes" ~:
            isRight
                (parseProgram "int f(int (*g)(struct Inner { int y; } inner), struct Outer { int x; } outer) { struct Outer b; return sizeof(b); } int main(void) { return 0; }")
                ~?= True
        , "preserves outer parameter-scope struct tags for nested declarator function definitions" ~:
            isRight
                (parseProgram "int (*f(struct S { int x; } a))(void) { struct S b; return 0; } int main(void) { return 0; }")
                ~?= True
        , "rejects nested function-pointer parameter struct tags leaking into function bodies" ~:
            isLeft
                (parseProgram "int f(struct Outer { int x; } outer, int (*g)(struct Inner { int y; } inner)) { struct Inner i; return 0; } int main(void) { return 0; }")
                ~?= True
        , "preserves function-definition parameter-scope enums for the body" ~:
            isRight
                (parseProgram "int f(enum E { A = 3 } e) { return A; } int main(void) { return 0; }")
                ~?= True
        , "preserves outer parameter-scope enums when nested function-pointer parameters add prototype scopes" ~:
            isRight
                (parseProgram "int f(int (*g)(enum Inner { I = 1 } inner), enum Outer { O = 2 } outer) { return O; } int main(void) { return 0; }")
                ~?= True
        , "preserves outer parameter-scope enums for nested declarator function definitions" ~:
            isRight
                (parseProgram "int (*f(enum E { A = 3 } e))(void) { if (A) return 0; return 0; } int main(void) { return 0; }")
                ~?= True
        , "rejects nested function-pointer parameter enums leaking into function bodies" ~:
            isLeft
                (parseProgram "int f(enum Outer { O = 1 } outer, int (*g)(enum Inner { I = 2 } inner)) { return I; } int main(void) { return 0; }")
                ~?= True
        , "rejects redefinition of parameter-scope struct tags inside the function body" ~:
            isLeft
                (parseProgram "int f(struct S { int x; } a) { struct S { int y; } b; return 0; } int main(void) { return 0; }")
                ~?= True
        , "re-resolves completed struct tags for function parameters when locals complete them later" ~:
            isRight
                (parseProgram "int f(struct S *p) { struct S { int x; }; return sizeof(*p); } int main(void) { return 0; }")
                ~?= True
        , "re-resolves completed struct tags for earlier parameters when later parameters complete them" ~:
            isRight
                (parseProgram "int f(struct S *p, struct S { int x; } q) { return sizeof(*p); } int main(void) { return 0; }")
                ~?= True
        , "normalizes completed typedef-backed local pointer declarators before storing them" ~:
            fmap (containsIncompleteStructTag "S" . CT.toTypeKind) (firstLocalDeclType "int main(void) { typedef struct S T; struct S { int a; }; T *p; return 0; }")
                ~?= Right False
        , "accepts block-scope extern arrays after typedef-backed element types complete" ~:
            isRight
                (parseProgram "typedef struct S T; struct S { int a; }; int main(void) { extern T arr[1]; return sizeof arr[0]; }")
                ~?= True
        , "treats visible typedefs as shadowing ordinary identifiers in expressions" ~:
            isLeft
                (parseProgram "int foo(void) { return 1; } int main(void) { typedef int foo; return foo(); }")
                ~?= True
        , TestLabel "block-scope forward struct declarations shadow outer tags" $ TestCase $
            assertProgramErrorContains
                "invalid application of 'sizeof' to incomplete type"
                "struct S { int a; }; int f(void) { struct S; return sizeof(struct S); }"
        , "does not shadow struct tags for typedef-backed null declarations" ~:
            isRight
                (parseProgram "struct S { int a; }; typedef struct S T; int main(void) { T; return sizeof(struct S); }")
                ~?= True
        , TestLabel "rejects incomplete element types in local object declarations" $ TestCase $
            assertProgramErrorContains
                "declaration of variable with incomplete type"
                "int main(void) { struct S; struct S a[1]; return sizeof(a); }"
        , TestLabel "rejects block-scope extern arrays with incomplete element types" $ TestCase $
            assertProgramErrorContains
                "declaration of variable with incomplete type"
                "int main(void) { extern struct S arr[1]; struct S { int a; }; return 0; }"
        , TestLabel "rejects incomplete by-value function definition parameters" $ TestCase $
            assertProgramErrorContains
                "declaration of variable with incomplete type"
                "int f(struct S s) { return 0; } int main(void) { return 0; }"
        , TestLabel "rejects incomplete unnamed by-value function definition parameters" $ TestCase $
            assertProgramErrorContains
                "declaration of variable with incomplete type"
                "int f(struct S) { return 0; } int main(void) { return 0; }"
        , TestLabel "rejects unsupported large by-value function definition parameters" $ TestCase $
            assertProgramErrorContains
                "unsupported by-value function parameter type"
                "struct S { int a; int b; int c; }; int f(struct S s) { return 0; } int main(void) { return 0; }"
        , TestLabel "rejects unsupported large unnamed by-value function definition parameters" $ TestCase $
            assertProgramErrorContains
                "unsupported by-value function parameter type"
                "struct S { int a; int b; int c; }; int f(struct S, int x) { return x; } int main(void) { return 0; }"
        , "rejects incomplete element types in struct members" ~:
            isLeft
                (parseProgram "int main(void) { struct S; struct T { struct S a[1]; }; return 0; }")
                ~?= True
        , "rejects file-scope typedef arrays whose element type stays incomplete" ~:
            isLeft
                (parseProgram "typedef struct S A[1]; struct S { int a; }; int main(void) { return 0; }")
                ~?= True
        , "rejects block-scope typedef arrays whose element type is void" ~:
            isLeft
                (parseProgram "int main(void) { typedef void V[1]; return 0; }")
                ~?= True
        , TestLabel "rejects storage-class specifiers in struct members" $ TestCase $
            assertProgramErrorContains
                "invalid storage-class specifier"
                "struct S { auto int x; }; int main(void) { return 0; }"
        , "rejects later file-scope object declarations incompatible with block-scope extern objects" ~:
            isLeft
                (parseProgram "int main(void) { extern int x; return 0; } char x;")
                ~?= True
        , "rejects local objects that reuse a same-block extern object name" ~:
            isLeft
                (parseProgram "int main(void) { extern int x; int x; return 0; }")
                ~?= True
        , "rejects block-scope extern objects that collide with visible outer functions" ~:
            isLeft
                (parseProgram "int foo(void); int main(void) { extern int foo; return 0; }")
                ~?= True
        , "rejects later file-scope function declarations incompatible with block-scope extern prototypes" ~:
            isLeft
                (parseProgram "int main(void) { extern int foo(void); return 0; } char foo(void) { return 0; }")
                ~?= True
        , "rejects block-scope extern prototypes that collide with visible outer objects" ~:
            isLeft
                (parseProgram "int foo; int main(void) { extern int foo(void); return 0; }")
                ~?= True
        , "rejects block-scope enum constants that collide with same-scope extern functions" ~:
            isLeft
                (parseProgram "int main(void) { extern int foo(void); enum E { foo = 1 }; return 0; }")
                ~?= True
        , "treats block-scope extern objects as ordinary identifiers inside the same block" ~:
            isLeft
                (parseProgram "typedef int T; int main(void) { extern int T; T x; return 0; }")
                ~?= True
        , TestLabel "rejects calling block-scope extern objects as functions" $ TestCase $
            assertProgramErrorContains
                "called object is not a function or function pointer"
                "int main(void) { extern int foo; return foo(); }"
        , "accepts extern redeclarations of visible static functions" ~:
            isRight
                (parseProgram "static int foo(void); extern int foo(void); int main(void) { return 0; }")
                ~?= True
        , "accepts block-scope extern redeclarations of visible static functions" ~:
            isRight
                (parseProgram "static int foo(void); int main(void) { extern int foo(void); return 0; }")
                ~?= True
        , "accepts extern redeclarations of visible static objects" ~:
            isRight
                (parseProgram "static int x; extern int x; int main(void) { return 0; }")
                ~?= True
        , "rejects plain file-scope tentative redeclarations after visible static objects" ~:
            isLeft
                (parseProgram "static int x; int x; int main(void) { return 0; }")
                ~?= True
        , "rejects plain file-scope initialized redeclarations after visible static objects" ~:
            isLeft
                (parseProgram "static int x; int x = 1; int main(void) { return x; }")
                ~?= True
        , "accepts block-scope extern redeclarations of visible static objects" ~:
            isRight
                (parseProgram "static int x; int main(void) { extern int x; return x; }")
                ~?= True
        , "rejects file-scope extern void objects" ~:
            isLeft
                (parseProgram "extern void x; int main(void) { return 0; }")
                ~?= True
        , "accepts struct definitions and tagged uses" ~:
            isRight
                (parseProgram "struct X { int v; }; int main(void) { struct X x; return 0; }")
                ~?= True
        , "rejects duplicate struct member names" ~:
            isLeft
                (parseProgram "struct S { int x; char x; }; int main(void) { return 0; }")
                ~?= True
        , "rejects function-typed struct members" ~:
            isLeft
                (parseProgram "struct S { int f(void); }; int main(void) { return 0; }")
                ~?= True
        , "rejects redeclarations that use distinct anonymous struct definitions" ~:
            isLeft
                (parseProgram "struct { int a; } x; struct { int a; } x; int main(void) { return 0; }")
                ~?= True
        , "rejects struct tag uses that resolve to enum tags" ~:
            isLeft
                (parseProgram "enum E { A }; int main(void) { struct E *p; return p == 0; }")
                ~?= True
        , "accepts enum definitions and tagged uses" ~:
            isRight
                (parseProgram "enum E { A }; int main(void) { enum E e; e = A; return e; }")
                ~?= True
        , "rejects empty enums" ~:
            isLeft
                (parseProgram "enum E { }; int main(void) { return 0; }")
                ~?= True
        , "rejects reusing a struct tag as an enum tag in the same scope" ~:
            isLeft
                (parseProgram "struct S; enum S { A }; int main(void) { return 0; }")
                ~?= True
        , "rejects globals that collide with typedef names" ~:
            isLeft
                (parseProgram "typedef int T; int T;")
                ~?= True
        , "rejects globals that collide with enum constants" ~:
            isLeft
                (parseProgram "enum E { A }; int A;")
                ~?= True
        , "rejects typedef names shadowed by local ordinary identifiers" ~:
            isLeft
                (parseProgram "typedef int T; int main(void) { int T = 0; T x; return x; }")
                ~?= True
        ]

functionPointerArithmeticTest :: Test
functionPointerArithmeticTest = TestLabel "Parser.Program.function-pointer-arithmetic" $
    TestList
        [ TestLabel "rejects adding to function pointers" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "int helper(void) { return 0; } int main(void) { int (*fp)(void) = helper; fp + 1; return 0; }"
        , TestLabel "rejects subtracting function pointers" $ TestCase $
            assertProgramErrorContains
                "invalid operands"
                "int helper(void) { return 0; } int main(void) { int (*fp)(void) = helper; int (*gp)(void) = helper; return fp - gp; }"
        ]

emptyForBodyPreservationTest :: Test
emptyForBodyPreservationTest = TestLabel "Parser.Program.empty-for-body-preservation" $ TestCase $
    case parseProgramAsts "int main(void) { for(;;); }" of
        Left err -> assertFailure $ show err
        Right
            [ ATNode
                (ATDefFunc "main" _)
                _
                (ATNode (ATBlock [ATNode (ATFor clauses) _ _ _]) _ _ _)
                _
            ] ->
                assertEqual
                    "parsed empty-body for-loops should preserve init, condition, increment, and body placeholders"
                    [ ATForInit ATEmpty
                    , ATForCond ATEmpty
                    , ATForIncr ATEmpty
                    , ATForStmt ATEmpty
                    ]
                    clauses
        Right asts ->
            assertFailure $ "unexpected AST shape: " <> show asts

sameInputExternalCollisionTest :: Test
sameInputExternalCollisionTest = TestLabel "Parser.Program.same-input-external-collision" $
    TestList
        [ "accepts prototype-before-global collisions in allowSameInputExternalCollisions mode" ~:
            isRight
                ( parseProgramAllowSameInputExternalCollisions
                    "int foo(void); int foo; int main(void) { return 0; }"
                )
                ~?= True
        , "accepts global-before-prototype collisions in allowSameInputExternalCollisions mode" ~:
            isRight
                ( parseProgramAllowSameInputExternalCollisions
                    "int foo; int foo(void); int main(void) { return 0; }"
                )
                ~?= True
        , "rejects sibling block-scope tagged declarations that only share a nesting depth" ~:
            isLeft
                ( parseProgramAllowSameInputExternalCollisions
                    "int a(void) { struct S { int x; }; extern struct S *f(void); return 0; } int b(void) { struct S { char y; }; extern struct S *f(void); return 0; } int main(void) { return 0; }"
                )
                ~?= True
        ]

test :: Test
test = TestLabel "Parser.Combinators.Core" $
    TestList [
        charLiteralTest
      , errorBundlePrettyTest
      , stringLiteralTest
      , hexadecimalTest
      , octalTest
      , naturalTest
      , integerTest
      , identifierTest
      , structInitializerTest
      , incompleteArrayInitializerTest
      , constantExpressionTest
      , integerOperatorTypeTest
      , globalInitializerTest
      , scalarInitializerTest
      , sameInputExternalCollisionTest
      , functionDesignatorContextTest
      , functionCallTest
      , conditionalPointerTypeTest
      , conditionalAggregateTypeTest
      , aggregateConditionTest
      , aggregateScalarOperatorTest
      , explicitAggregateCastTest
      , functionPointerAssignmentTest
      , postfixMemberAccessTest
      , declarationSpecifierTest
      , functionPointerArithmeticTest
      , emptyForBodyPreservationTest
      ]
