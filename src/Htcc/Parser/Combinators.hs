{-|
Module      : Htcc.Parser.Combinators
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE FlexibleContexts #-}
module Htcc.Parser.Combinators (
    natural
  , lexer
  , operator
  , identifier
  , reserved
  , reservedOp
  , integer
  , whiteSpace
  , float
  , charLiteral
  , stringLiteral
  , parens
  , braces
  , angles
  , semi
  , comma
  , colon
  , dot
  , commaSep
  , commaSep1
) where

import           Htcc.CRules.Definition
import           Text.Parsec              (ParsecT, Stream)
import qualified Text.Parsec.Token        as P

{-# INLINE lexer #-}
lexer :: Stream s m Char => P.GenTokenParser s u m
lexer = P.makeTokenParser cDef

reserved, reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

natural, integer :: Stream s m Char => ParsecT s u m Integer
natural = P.natural lexer
integer = P.integer lexer

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = P.whiteSpace lexer

float :: Stream s m Char => ParsecT s u m Double
float = P.float lexer

parens, braces, angles :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer

charLiteral :: Stream s m Char => ParsecT s u m Char
charLiteral = P.charLiteral lexer

identifier, operator, semi, comma, colon, dot, stringLiteral :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer
operator = P.operator lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer
stringLiteral = P.stringLiteral lexer

commaSep, commaSep1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
