{-|
Module      : Htcc.Parser.Combinators.Core
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Htcc.Parser.Combinators.Core (
    runParser
  , ConstructionDataState
  , Parser
  , spaceConsumer
  , lexeme
  , symbol
  , charLiteral
  , stringLiteral
  , decimal
  , hexadecimal
  , octal
  , natural
  , integer
  , angles
  , parens
  , braces
  , brackets
  , identifier
  , semi
  , comma
  , colon
  , lnot
  , sharp
  , ampersand
  , lparen
  , rparen
  , lbrace
  , rbrace
  , langle
  , rangle
  , lbracket
  , rbracket
  , star
  , period
  , slash
  , equal
  , question
  , hat
  , tilda
  , vertical
  , percent
  , notFollowedBy
) where

import           Htcc.Parser.Combinators.ParserType

import           Control.Applicative                (Alternative (..))
import           Control.Monad.Combinators          (between)
import           Data.Char                          (isAlpha)
import           Data.Functor                       (($>))
import qualified Data.Text                          as T
import qualified Htcc.CRules                        as CR
import           Htcc.Utils                         (lor)
import qualified Text.Megaparsec                    as M
import qualified Text.Megaparsec.Char               as MC
import qualified Text.Megaparsec.Char.Lexer         as ML
import qualified Text.Parsec                        as P

spaceConsumer :: (Monad m, Ord e) => M.ParsecT e T.Text m ()
spaceConsumer = ML.space MC.space1 (ML.skipLineComment "//") (ML.skipBlockComment "/*" "*/")

lexeme :: (Monad m, Ord e) => M.ParsecT e T.Text m a -> M.ParsecT e T.Text m a
lexeme = ML.lexeme spaceConsumer

symbol :: (Monad m, Ord e) => T.Text -> M.ParsecT e T.Text m T.Text
symbol = ML.symbol spaceConsumer

charLiteral :: (Monad m, Ord e) => M.ParsecT e T.Text m Char
charLiteral = M.between (MC.char '\'') (MC.char '\'') charBody <* spaceConsumer
    where
        -- Keep the historical single-quoted character behavior that the existing
        -- component tests exercise, while `ML.charLiteral` remains strict enough
        -- for string-literal parsing.
        charBody = ML.charLiteral <|> M.ParsecT (P.noneOf ['\\'])

stringLiteral :: (Monad m, Ord e) => M.ParsecT e T.Text m String
stringLiteral = MC.char '\"' *> ((<> "\0") <$> M.manyTill ML.charLiteral (MC.char '\"')) <* spaceConsumer

hexadecimal, octal, decimal, natural, integer :: (Monad m, Ord e, Num i) => M.ParsecT e T.Text m i
hexadecimal = MC.char '0' >> MC.char' 'x' >> ML.hexadecimal
octal = MC.char '0' >> ML.octal
decimal = ML.decimal
natural = M.try (lexeme hexadecimal) <|> M.try (lexeme octal) <|> lexeme decimal
integer = ML.signed spaceConsumer natural <|> natural

parens, braces, angles, brackets :: (Monad m, Ord e) => M.ParsecT e T.Text m a -> M.ParsecT e T.Text m a
parens = between lparen rparen
braces = between lbrace rbrace
angles = between langle rangle
brackets = between lbracket rbracket

identifier,
    semi,
    comma,
    colon,
    lnot,
    sharp,
    ampersand,
    lparen,
    rparen,
    lbrace,
    rbrace,
    langle,
    rangle,
    lbracket,
    rbracket,
    star,
    period,
    slash,
    equal,
    question,
    hat,
    tilda,
    vertical,
    percent :: (Monad m, Ord e) => M.ParsecT e T.Text m T.Text
identifier = M.try $ do
    ident <-
        mappend
            <$> M.takeWhile1P (Just "valid identifier") (lor [isAlpha, (=='_')])
            <*> M.takeWhileP (Just "valid identifier") CR.isValidChar
    if ident `elem` reservedKeywords
        then fail $ "reserved keyword '" <> T.unpack ident <> "' cannot be used as identifier"
        else spaceConsumer $> ident

reservedKeywords :: [T.Text]
reservedKeywords =
    [ "auto"
    , "break"
    , "case"
    , "char"
    , "const"
    , "continue"
    , "default"
    , "do"
    , "double"
    , "else"
    , "enum"
    , "extern"
    , "float"
    , "for"
    , "goto"
    , "if"
    , "inline"
    , "int"
    , "long"
    , "register"
    , "restrict"
    , "return"
    , "short"
    , "signed"
    , "sizeof"
    , "static"
    , "struct"
    , "switch"
    , "typedef"
    , "union"
    , "unsigned"
    , "void"
    , "volatile"
    , "while"
    , "_Alignas"
    , "_Alignof"
    , "_Atomic"
    , "_Bool"
    , "_Complex"
    , "_Generic"
    , "_Imaginary"
    , "_Noreturn"
    , "_Static_assert"
    , "_Thread_local"
    ]
semi = symbol ";"
comma = symbol ","
colon = symbol ":"
lnot = symbol "!"
sharp = symbol "#"
ampersand = symbol "&"
lparen = symbol "("
rparen = symbol ")"
lbrace = symbol "{"
rbrace = symbol "}"
langle = symbol "<"
rangle = symbol ">"
lbracket = symbol "["
rbracket = symbol "]"
star = symbol "*"
period = symbol "."
slash = symbol "/"
equal = symbol "="
question = symbol "?"
hat = symbol "^"
tilda = symbol "~"
vertical = symbol "|"
percent = symbol "%"

notFollowedBy :: (Monad m, Ord e)
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m b
    -> M.ParsecT e T.Text m a
notFollowedBy k p = lexeme (k <* M.notFollowedBy p)
