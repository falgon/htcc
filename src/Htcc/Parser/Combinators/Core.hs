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
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}
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

import           Control.Applicative                (Alternative (..), optional)
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
import qualified Text.Megaparsec.Pos                as MP

spaceConsumer :: (Monad m, Ord e) => M.ParsecT e T.Text m ()
spaceConsumer = ML.space spaceOrInclude lineComment (ML.skipBlockComment "/*" "*/")
    where
        spaceOrInclude = skipIncludeLine <|> whiteSpaceChar
        lineComment = ML.skipLineComment "//"
        skipIncludeLine = do
            pos <- M.getSourcePos
            if MP.unPos (MP.sourceColumn pos) == 1
                then M.try includeLine
                else M.empty
        includeLine = do
            horizontalSpace
            void $ MC.char '#'
            horizontalSpace
            void $ MC.string "include"
            M.notFollowedBy $ M.satisfy CR.isValidChar
            void $ M.takeWhileP Nothing (/= '\n')
        horizontalSpace = void $ M.takeWhileP Nothing (`elem` (" \t\r\f\v" :: String))
        whiteSpaceChar = void $ M.satisfy (`elem` (" \t\r\f\v\n" :: String))

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
        charBody = escapedByte <|> M.noneOf ['\\', '\'', '\n', '\r']
        escapedByte =
            MC.char '\\'
                *> (hexEscape
                    <|> octalEscape
                    <|> simpleEscape
                    <|> invalidEscape
                   )
        simpleEscape = M.choice
            [ '\a' <$ MC.char 'a'
            , '\b' <$ MC.char 'b'
            , '\t' <$ MC.char 't'
            , '\n' <$ MC.char 'n'
            , '\v' <$ MC.char 'v'
            , '\f' <$ MC.char 'f'
            , '\r' <$ MC.char 'r'
            , '\ESC' <$ MC.char 'e'
            , '\\' <$ MC.char '\\'
            , '\'' <$ MC.char '\''
            , '"' <$ MC.char '"'
            , '?' <$ MC.char '?'
            ]
        hexEscape = do
            void $ MC.char 'x'
            digits <- hexDigits
            byteFromDigits 16 digits
        hexDigits = do
            first <- M.satisfy isHexDigit
            collectHexDigits 1 [first]
        collectHexDigits count revDigits =
            optional (M.lookAhead $ M.satisfy isHexDigit) >>= \case
                Nothing -> pure $ reverse revDigits
                Just _
                    | count >= maxHexEscapeDigits ->
                        fail "character constant escape exceeds byte width"
                    | otherwise -> do
                        c <- M.satisfy isHexDigit
                        let count' = succ count
                            revDigits' = c : revDigits
                        count' `seq` revDigits' `seq` collectHexDigits count' revDigits'
        octalEscape = do
            first <- M.satisfy isOctDigit
            second <- optional $ M.satisfy isOctDigit
            third <- optional $ M.satisfy isOctDigit
            byteFromDigits 8 $ first : maybe [] (\d -> d : maybe [] pure third) second
        byteFromDigits base digits =
            let n = foldl (\acc c -> acc * base + digitToInt c) 0 digits
             in if n <= 0xff
                    then pure $ chr n
                    else fail "character constant escape exceeds byte width"
        invalidEscape =
            M.anySingle >>= \c -> fail ("invalid escape sequence \\" <> [c])
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
        stringByteChunk = escapedByte <|> rawCharBytes
        rawCharBytes = TE.encodeUtf8 . T.singleton <$> M.noneOf ['\\', '"', '\n', '\r']
        escapedByte =
            MC.char '\\'
                *> (hexEscape
                    <|> octalEscape
                    <|> simpleEscape
                    <|> invalidEscape
                   )
        simpleEscape = M.choice
            [ byte '\a' <$ MC.char 'a'
            , byte '\b' <$ MC.char 'b'
            , byte '\t' <$ MC.char 't'
            , byte '\n' <$ MC.char 'n'
            , byte '\v' <$ MC.char 'v'
            , byte '\f' <$ MC.char 'f'
            , byte '\r' <$ MC.char 'r'
            , byte '\ESC' <$ MC.char 'e'
            , byte '\\' <$ MC.char '\\'
            , byte '\'' <$ MC.char '\''
            , byte '"' <$ MC.char '"'
            , byte '?' <$ MC.char '?'
            ]
        hexEscape = do
            void $ MC.char 'x'
            digits <- hexDigits
            byteFromDigits "character code point out of range" 16 digits
        octalEscape = do
            first <- M.satisfy isOctDigit
            second <- optional $ M.satisfy isOctDigit
            third <- optional $ M.satisfy isOctDigit
            byteFromDigits "character code point out of range" 8 $ first : maybe [] (\d -> d : maybe [] pure third) second
        hexDigits = do
            first <- M.satisfy isHexDigit
            collectHexDigits 1 [first]
        collectHexDigits count revDigits =
            optional (M.lookAhead $ M.satisfy isHexDigit) >>= \case
                Nothing -> pure $ reverse revDigits
                Just _
                    | count >= maxHexEscapeDigits ->
                        fail "character code point out of range"
                    | otherwise -> do
                        c <- M.satisfy isHexDigit
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
            M.anySingle >>= \c -> fail ("invalid escape sequence \\" <> [c])

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
            M.option Nothing (Just <$> M.lookAhead binaryDigit) >>= \case
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
