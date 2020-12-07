{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Tests.ComponentsTests.Parser.Combinators (
    test
) where
import           Data.Char                    (chr)
import           Data.Either                  (isLeft)
import qualified Data.Text                    as T
import           Data.Void                    (Void)
import qualified Htcc.CRules                  as CR
import           Htcc.Parser.Combinators.Core
import qualified Htcc.Tokenizer.Token         as HTT 
import           Htcc.Utils                   (tshow)
import           Test.HUnit                   (Test (..), (~:), (~?=))
import           Text.Megaparsec

type TestParser = Parsec Void T.Text

charLiteralTest, stringLiteralTest, hexadecimalTest, octalTest, naturalTest, integerTest, identifierTest, operatorTest :: Test

charLiteralTest = TestLabel "Parser.Combinators.Core.charLiteral" $
    TestList [
        TestLabel "Parser.Combinators.Core.charLiteral success patterns" $
            TestList [
                TestLabel "valid characters" $ TestList [
                    (show x <> " == " <> show x) ~:
                        runParser charLiteral' "" (T.singleton '\'' <> T.singleton x <> T.singleton '\'') ~?= Right x
                            | x <- charSets
                ]
              , TestLabel "partial characters" $ TestList [
                    "\'a\'b == a" ~:
                        runParser charLiteral' "" "\'a\'b" ~?= Right 'a'
                  , "\'!\'b == !" ~:
                        runParser charLiteral' "" "\'!\'b" ~?= Right '!'
                ]
            ]
      , TestLabel "Parser.Combinators.Core.charLiteral fail patterns" $
            TestList [
                "ab" ~: isLeft (runParser charLiteral' "" "ab") ~?= True
              , "123" ~: isLeft (runParser charLiteral' "" "123") ~?= True
              , "\'a" ~: isLeft (runParser charLiteral' "" "\'a") ~?= True
            ]
    ]
    where
        charLiteral' = charLiteral :: TestParser Char
        charSets =
            ['A'..'Z']
            <> ['a'..'z']
            <> ['0'..'9']
            <> "!\"#%&\'()*+,-./:;<=>?[]^_{|}~\a\b\n\r\f\t\v\0"
            <> [chr 27]

stringLiteralTest = TestLabel "Parser.Combinators.Core.stringLiteral" $
    TestList [
        TestLabel "Parser.Combinators.Core.stringLiteral success patterns" $
            TestList [
                "\"abc\" == abc" ~: runParser stringLiteral' "" "\"abc\"" ~?= Right "abc"
              , "\"012\" == 012" ~: runParser stringLiteral' "" "\"012\"" ~?= Right "012"
              , "\"012\"3 == 012" ~: runParser stringLiteral' "" "\"012\"3" ~?= Right "012"
            ]
      , TestLabel "Parser.Combinators.Core.stringLiteral fail patterns" $
            TestList [
                "abc" ~: isLeft (runParser stringLiteral' "" "abc") ~?= True
              , "\"abc" ~: isLeft (runParser stringLiteral' "" "\"abc") ~?= True
            ]
    ]
    where
        stringLiteral' = stringLiteral :: TestParser String

hexadecimalTest = TestLabel "Parser.Combinators.Core.hexadecimal" $
    TestList [
        TestLabel "Parser.Combinators.Core.hexadecimal success patterns" $
            TestList [
                "0x01 == 0x01" ~: runParser hexadecimal' "" "0x01" ~?= Right 0x01
              , "0xf == 0xf" ~: runParser hexadecimal' "" "0xf" ~?= Right 0xf
              , "0X0 == 0x0" ~: runParser hexadecimal' "" "0X0" ~?= Right 0x0
              , "0Xf == 0xf" ~: runParser hexadecimal' "" "0Xf" ~?= Right 0xf
              , "0xfz == 0xf" ~: runParser hexadecimal' "" "0xfz" ~?= Right 0xf
            ]
      , TestLabel "Parser.Combinators.Core.hexadecimal fail patterns" $
            TestList [
                "0x" ~: isLeft (runParser hexadecimal' "" "0x") ~?= True
              , "0xz" ~: isLeft (runParser hexadecimal' "" "0xz") ~?= True
              , "01" ~: isLeft (runParser hexadecimal' "" "01") ~?= True
              , "0" ~: isLeft (runParser hexadecimal' "" "0") ~?= True
            ]
    ]
    where
        hexadecimal' = hexadecimal :: TestParser Int

octalTest = TestLabel "Parser.Combinators.Core.octal" $
    TestList [
        TestLabel "Parser.Combinators.Core.octal success patterns" $
            TestList [
                "01 == 0o1" ~: runParser octal' "" "01" ~?= Right 0o1
              , "0010 == 0o10" ~: runParser octal' "" "0010" ~?= Right 0o10
              , "0010a == 0o10" ~: runParser octal' "" "0010a" ~?= Right 0o10
            ]
      , TestLabel "Parser.Combinators.Core.octal fail patterns" $
            TestList [
                "0x0" ~: isLeft (runParser octal' "" "0x0") ~?= True
              , "0" ~: isLeft (runParser octal' "" "0") ~?= True
            ]
    ]
    where
        octal' = octal :: TestParser Int

naturalTest = TestLabel "Parser.Combinators.Core.natural" $
    TestList [
        TestLabel "Parser.Combinators.Core.natural success patterns" $
            TestList [
                "10 == 10" ~: runParser natural' "" "10" ~?= Right 10
              , "0010 == 0o10" ~: runParser natural' "" "0010" ~?= Right 0o10
              , "0x1 == 0x1" ~: runParser natural' "" "0x1" ~?= Right 0x1
              , "0x == 0" ~: runParser natural' "" "0x" ~?= Right 0
              , "0xz == 0" ~: runParser natural' "" "0xz" ~?= Right 0
              , "00x0 == 0" ~: runParser natural' "" "00x0" ~?= Right 0
            ]
      , TestLabel "Parser.Combinators.Core.natural fail patterns" $
            TestList [
                "hoge" ~: isLeft (runParser natural' "" "hoge") ~?= True
            ]
    ]
    where
        natural' = natural :: TestParser Int

integerTest = TestLabel "Parser.Combinators.Core.integer" $
    TestList [
        TestLabel "Parser.Combinators.Core.integer success patterns" $
            TestList [
                "10 == 10" ~: runParser integer' "" "10" ~?= Right 10
              , "0010 == 0o10" ~: runParser integer' "" "0010" ~?= Right 0o10
              , "0x1 == 0x1" ~: runParser integer' "" "0x1" ~?= Right 0x1
              , "0x == 0" ~: runParser integer' "" "0x" ~?= Right 0
              , "0xz == 0" ~: runParser integer' "" "0xz" ~?= Right 0
              , "00x0 == 0" ~: runParser integer' "" "00x0" ~?= Right 0
              , "+10 == 10" ~: runParser integer' "" "+10" ~?= Right 10
              , "+0010 == 0o10" ~: runParser integer' "" "+0010" ~?= Right 0o10
              , "+0x1 == 0x1" ~: runParser integer' "" "+0x1" ~?= Right 0x1
              , "+0x == 0" ~: runParser integer' "" "+0x" ~?= Right 0
              , "+0xz == 0" ~: runParser integer' "" "+0xz" ~?= Right 0
              , "+00x0 == 0" ~: runParser integer' "" "+00x0" ~?= Right 0
              , "-10 == -10" ~: runParser integer' "" "-10" ~?= Right (-10)
              , "-0010 == -0o10" ~: runParser integer' "" "-0010" ~?= Right (-0o10)
              , "-0x1 == -0x1" ~: runParser integer' "" "-0x1" ~?= Right (-0x1)
              , "-0x == 0" ~: runParser integer' "" "-0x" ~?= Right 0
              , "-0xz == 0" ~: runParser integer' "" "-0xz" ~?= Right 0
              , "-00x0 == 0" ~: runParser integer' "" "-00x0" ~?= Right 0
            ]
    ]
    where
        integer' = integer :: TestParser Int

identifierTest = TestLabel "Parser.Combinators.Core.identifier" $
    TestList [
        TestLabel "Parser.Combinators.Core.identifier success patterns" $
            TestList [
                "abcde" ~: runParser identifier' "" "abcde" ~?= Right "abcde"
              , "_" ~: runParser identifier' "" "_" ~?= Right "_"
              , "a@" ~: runParser identifier' "" "a@" ~?= Right "a"
              , "a1a" ~: runParser identifier' "" "a1" ~?= Right "a1"
            ]
      , TestLabel "Parser.Combinators.Core.identifier fail patterns" $
            TestList [
                TestLabel "invalid characters eg" $ TestList [
                    "@" ~: isLeft (runParser identifier' "" "@") ~?= True
                  , "@a" ~: isLeft (runParser identifier' "" "@a") ~?= True
                  , "1a" ~: isLeft (runParser identifier' "" "1a") ~?= True
                ]
              , TestLabel "3 characters op" $
                    TestList [T.unpack op ~: isLeft (runParser identifier' "" op) ~?= True | op <- CR.strOps3]
              , TestLabel "2 characters op" $
                    TestList [T.unpack op ~: isLeft (runParser identifier' "" op) ~?= True | op <- CR.strOps2]
              , TestLabel "1 characters op" $
                    TestList [[op] ~: isLeft (runParser identifier' "" $ T.singleton op) ~?= True | op <- CR.charOps]
            ]
    ]
    where
        identifier' = identifier :: TestParser T.Text

operatorTest = TestLabel "Parser.Combinators.Core.operator" $
    TestList [
        TestLabel "Parser.Combinators.Core.operator success patterns" $
            TestList [
                TestLabel "3 characters" $ TestList [T.unpack op ~: runParser operator' "" op ~?= Right op | op <- CR.strOps3]
              , TestLabel "2 characters" $ TestList [T.unpack op ~: runParser operator' "" op ~?= Right op | op <- CR.strOps2]
              , TestLabel "1 character" $
                    TestList [[op] ~: runParser operator' "" (T.singleton op) ~?= Right (T.singleton op) | op <- CR.charOps]
            ]
      , TestLabel "Parser.Combinators.Core.operator fail patterns" $
            TestList [
                [c] ~: isLeft (runParser operator' "" (T.singleton c)) ~?= True 
                    | c <- ['a'..'z'] <> ['A'..'Z']
            ]
    ]
    where
        operator' = operator :: TestParser T.Text

test :: Test
test = TestLabel "Parser.Combinators.Core" $
    TestList [
        charLiteralTest
      , stringLiteralTest
      , hexadecimalTest
      , octalTest
      , naturalTest
      , integerTest
      , identifierTest
      , operatorTest
    ]
