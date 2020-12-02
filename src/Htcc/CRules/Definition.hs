{-|
Module      : Htcc.CRules.Definition
Description : C language definition
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language definition
-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Htcc.CRules.Definition (
    cDef
) where

import           Text.Parsec           (Stream, (<|>))
import           Text.Parsec.Char      (alphaNum, char, letter, oneOf)
import qualified Text.Parsec.Token     as P

charOps :: String
charOps = "+-*/()<>=;{},&|^%!~[].?:"

strOps2 :: [String]
strOps2 = [
    "<="
  , ">="
  , "=="
  , "!="
  , "<<"
  , ">>"
  , "->"
  , "++"
  , "--"
  , "+="
  , "-="
  , "*="
  , "/="
  , "&&"
  , "||"
  , "&="
  , "|="
  , "^="
  ]

strOps3 :: [String]
strOps3 = [
    "<<="
  , ">>="
  ]

keywords :: [String]
keywords = [
    "return"
  , "if"
  , "switch"
  , "case"
  , "default"
  , "else"
  , "while"
  , "for"
  , "break"
  , "continue"
  , "enum"
  , "struct"
  , "sizeof"
  , "goto"
  , "_Alignof"
  , "typedef"
  ]

cDef :: Stream s m Char => P.GenLanguageDef s st m
cDef = P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = True
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> char '_'
    , P.opStart = P.opLetter cDef
    , P.opLetter = oneOf charOps
    , P.reservedOpNames = ((:[]) <$> charOps)
        <> strOps2
        <> strOps3
    , P.reservedNames = keywords
    , P.caseSensitive = True
    }

