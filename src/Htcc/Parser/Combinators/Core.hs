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
import           Control.Monad                      (void)
import           Control.Monad.Combinators          (between)
import qualified Data.ByteString                    as B
import           Data.Char                          (chr, digitToInt, isAlpha,
                                                     isHexDigit, isOctDigit,
                                                     ord)
import           Data.Functor                       (($>))
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Htcc.CRules                        as CR
import           Htcc.Utils                         (lor)
import qualified Text.Megaparsec                    as M
import qualified Text.Megaparsec.Char               as MC
import qualified Text.Megaparsec.Char.Lexer         as ML
import qualified Text.Parsec                        as P
import qualified Text.Parsec.Pos                    as PP

spaceConsumer :: (Monad m, Ord e) => M.ParsecT e T.Text m ()
spaceConsumer = ML.space spaceOrInclude lineComment (ML.skipBlockComment "/*" "*/")
    where
        spaceOrInclude = skipIncludeLine <|> whiteSpaceChar
        lineComment = ML.skipLineComment "//"
        skipIncludeLine = do
            pos <- M.getSourcePos
            if PP.sourceColumn pos == 1
                then M.ParsecT $ P.try includeLine
                else M.empty
        includeLine = do
            horizontalSpace
            void $ P.char '#'
            horizontalSpace
            void $ P.string "include"
            P.notFollowedBy $ P.satisfy CR.isValidChar
            P.skipMany $ P.noneOf "\n"
        horizontalSpace = void $ P.many $ P.oneOf (" \t\r\f\v" :: String)
        whiteSpaceChar = M.ParsecT $ void $ P.oneOf (" \t\r\f\v\n" :: String)

lexeme :: (Monad m, Ord e) => M.ParsecT e T.Text m a -> M.ParsecT e T.Text m a
lexeme = ML.lexeme spaceConsumer

symbol :: (Monad m, Ord e) => T.Text -> M.ParsecT e T.Text m T.Text
symbol = ML.symbol spaceConsumer

maxHexEscapeDigits :: Int
maxHexEscapeDigits = 4

maxBinaryLiteralDigits :: Int
maxBinaryLiteralDigits = 128

charLiteral :: (Monad m, Ord e, Num i) => M.ParsecT e T.Text m i
charLiteral = do
    void $ MC.char '\''
    first <- byteCharBody
    rest <- charLiteralTail 3
    charConstantValue (first : rest) <$ spaceConsumer
    where
        charBody = M.ParsecT $ escapedByte <|> P.noneOf ['\\', '\'', '\n', '\r']
        escapedByte =
            P.char '\\'
                *> (hexEscape
                    <|> octalEscape
                    <|> simpleEscape
                    <|> invalidEscape
                   )
        simpleEscape = P.choice
            [ '\a' <$ P.char 'a'
            , '\b' <$ P.char 'b'
            , '\t' <$ P.char 't'
            , '\n' <$ P.char 'n'
            , '\v' <$ P.char 'v'
            , '\f' <$ P.char 'f'
            , '\r' <$ P.char 'r'
            , '\ESC' <$ P.char 'e'
            , '\\' <$ P.char '\\'
            , '\'' <$ P.char '\''
            , '"' <$ P.char '"'
            , '?' <$ P.char '?'
            ]
        hexEscape = do
            void $ P.char 'x'
            digits <- hexDigits
            byteFromDigits 16 digits
        hexDigits = do
            first <- P.satisfy isHexDigit
            collectHexDigits 1 [first]
        collectHexDigits count revDigits =
            P.optionMaybe (P.lookAhead $ P.satisfy isHexDigit) >>= \next ->
                case next of
                    Nothing -> pure $ reverse revDigits
                    Just _
                        | count >= maxHexEscapeDigits ->
                            fail "character constant escape exceeds byte width"
                        | otherwise -> do
                            c <- P.satisfy isHexDigit
                            let count' = succ count
                                revDigits' = c : revDigits
                            count' `seq` revDigits' `seq` collectHexDigits count' revDigits'
        octalEscape = do
            first <- P.satisfy isOctDigit
            second <- P.optionMaybe $ P.satisfy isOctDigit
            third <- P.optionMaybe $ P.satisfy isOctDigit
            byteFromDigits 8 $ first : maybe [] (\d -> d : maybe [] pure third) second
        byteFromDigits base digits =
            let n = foldl (\acc c -> acc * base + digitToInt c) 0 digits
             in if n <= 0xff
                    then pure $ chr n
                    else fail "character constant escape exceeds byte width"
        invalidEscape =
            P.anyChar >>= \c -> fail ("invalid escape sequence \\" <> [c])
        byteCharBody = do
            c <- charBody
            if ord c <= 0xff
                then pure c
                else fail "character constant escape exceeds byte width"
        charLiteralTail remaining =
            M.choice
                [ [] <$ MC.char '\''
                , if remaining == 0
                    then charBody *> fail "multi-character constant is too long"
                    else (:) <$> byteCharBody <*> charLiteralTail (pred remaining)
                ]
        charConstantValue = foldl (\acc c -> acc * 256 + fromIntegral (ord c)) 0

stringLiteral :: (Monad m, Ord e) => M.ParsecT e T.Text m B.ByteString
stringLiteral = do
    void $ MC.char '\"'
    chunks <- M.manyTill stringByteChunk (MC.char '\"')
    spaceConsumer
    pure $ B.concat chunks `B.snoc` 0
    where
        stringByteChunk = M.ParsecT $ escapedByte <|> rawCharBytes
        rawCharBytes = TE.encodeUtf8 . T.singleton <$> P.noneOf ['\\', '"', '\n', '\r']
        escapedByte =
            P.char '\\'
                *> (hexEscape
                    <|> octalEscape
                    <|> simpleEscape
                    <|> invalidEscape
                   )
        simpleEscape = P.choice
            [ byte '\a' <$ P.char 'a'
            , byte '\b' <$ P.char 'b'
            , byte '\t' <$ P.char 't'
            , byte '\n' <$ P.char 'n'
            , byte '\v' <$ P.char 'v'
            , byte '\f' <$ P.char 'f'
            , byte '\r' <$ P.char 'r'
            , byte '\ESC' <$ P.char 'e'
            , byte '\\' <$ P.char '\\'
            , byte '\'' <$ P.char '\''
            , byte '"' <$ P.char '"'
            , byte '?' <$ P.char '?'
            ]
        hexEscape = do
            void $ P.char 'x'
            digits <- hexDigits
            byteFromDigits "character code point out of range" 16 digits
        octalEscape = do
            first <- P.satisfy isOctDigit
            second <- P.optionMaybe $ P.satisfy isOctDigit
            third <- P.optionMaybe $ P.satisfy isOctDigit
            byteFromDigits "character code point out of range" 8 $ first : maybe [] (\d -> d : maybe [] pure third) second
        hexDigits = do
            first <- P.satisfy isHexDigit
            collectHexDigits 1 [first]
        collectHexDigits count revDigits =
            P.optionMaybe (P.lookAhead $ P.satisfy isHexDigit) >>= \next ->
                case next of
                    Nothing -> pure $ reverse revDigits
                    Just _
                        | count >= maxHexEscapeDigits ->
                            fail "character code point out of range"
                        | otherwise -> do
                            c <- P.satisfy isHexDigit
                            let count' = succ count
                                revDigits' = c : revDigits
                            count' `seq` revDigits' `seq` collectHexDigits count' revDigits'
        byteFromDigits errMsg base digits =
            let n = foldl (\acc c -> acc * base + digitToInt c) 0 digits
             in if n <= 0xff
                    then pure $ B.singleton $ fromIntegral n
                    else fail errMsg
        byte = B.singleton . fromIntegral . ord
        invalidEscape =
            P.anyChar >>= \c -> fail ("invalid escape sequence \\" <> [c])

hexadecimal, binary, octal, decimal, natural, integer :: (Monad m, Ord e, Num i) => M.ParsecT e T.Text m i
hexadecimal = MC.char '0' >> MC.char' 'x' >> ML.hexadecimal
binary = do
    void $ MC.char '0'
    void $ MC.char' 'b'
    first <- binaryDigit
    consumeBinaryDigits 1 $ binaryDigitValue first
    where
        binaryDigit = MC.char '0' <|> MC.char '1'
        binaryDigitValue = fromIntegral . digitToInt
        consumeBinaryDigits count acc =
            M.option Nothing (Just <$> M.lookAhead binaryDigit) >>= \next ->
                case next of
                    Nothing -> pure acc
                    Just _
                        | count >= maxBinaryLiteralDigits ->
                            fail "binary integer literal is too long"
                        | otherwise -> do
                            c <- binaryDigit
                            let count' = succ count
                                acc' = acc * 2 + binaryDigitValue c
                            count' `seq` acc' `seq` consumeBinaryDigits count' acc'
octal = MC.char '0' >> ML.octal
decimal = ML.decimal
natural = M.try (lexeme hexadecimal) <|> M.try (lexeme binary) <|> M.try (lexeme octal) <|> lexeme decimal
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
