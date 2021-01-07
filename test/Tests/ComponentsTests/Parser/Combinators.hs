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
import qualified Text.Megaparsec              as M

type TestParser = M.Parsec Void T.Text

charLiteralTest, 
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
                        M.runParser charLiteral' "" (T.singleton '\'' <> T.singleton x <> T.singleton '\'') ~?= Right x
                            | x <- charSets
                ]
              , TestLabel "partial characters" $ TestList [
                    "\'a\'b == a" ~:
                        M.runParser charLiteral' "" "\'a\'b" ~?= Right 'a'
                  , "\'!\'b == !" ~:
                        M.runParser charLiteral' "" "\'!\'b" ~?= Right '!'
                ]
            ]
      , TestLabel "Parser.Combinators.Core.charLiteral fail patterns" $
            TestList [
                "ab" ~: isLeft (M.runParser charLiteral' "" "ab") ~?= True
              , "123" ~: isLeft (M.runParser charLiteral' "" "123") ~?= True
              , "\'a" ~: isLeft (M.runParser charLiteral' "" "\'a") ~?= True
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
                "\"abc\" == abc" ~: M.runParser stringLiteral' "" "\"abc\"" ~?= Right "abc\0"
              , "\"012\" == 012" ~: M.runParser stringLiteral' "" "\"012\"" ~?= Right "012\0"
              , "\"012\"3 == 012" ~: M.runParser stringLiteral' "" "\"012\"3" ~?= Right "012\0"
            ]
      , TestLabel "Parser.Combinators.Core.stringLiteral fail patterns" $
            TestList [
                "abc" ~: isLeft (M.runParser stringLiteral' "" "abc") ~?= True
              , "\"abc" ~: isLeft (M.runParser stringLiteral' "" "\"abc") ~?= True
            ]
    ]
    where
        stringLiteral' = stringLiteral :: TestParser String

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
              , "0x == 0" ~: M.runParser natural' "" "0x" ~?= Right 0
              , "0xz == 0" ~: M.runParser natural' "" "0xz" ~?= Right 0
              , "00x0 == 0" ~: M.runParser natural' "" "00x0" ~?= Right 0
            ]
      , TestLabel "Parser.Combinators.Core.natural fail patterns" $
            TestList [
                "hoge" ~: isLeft (M.runParser natural' "" "hoge") ~?= True
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
              , "+0x == 0" ~: M.runParser integer' "" "+0x" ~?= Right 0
              , "+0xz == 0" ~: M.runParser integer' "" "+0xz" ~?= Right 0
              , "+00x0 == 0" ~: M.runParser integer' "" "+00x0" ~?= Right 0
              , "-10 == -10" ~: M.runParser integer' "" "-10" ~?= Right (-10)
              , "-0010 == -0o10" ~: M.runParser integer' "" "-0010" ~?= Right (-0o10)
              , "-0x1 == -0x1" ~: M.runParser integer' "" "-0x1" ~?= Right (-0x1)
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
            ]
    ]
    where
        identifier' = identifier :: TestParser T.Text

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
    ]
