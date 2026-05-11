module Text.Megaparsec.Char.Lexer (
    space,
    skipLineComment,
    skipBlockComment,
    lexeme,
    symbol,
    charLiteral,
    decimal,
    hexadecimal,
    octal,
    signed
) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Char            (chr, digitToInt, isHexDigit, isOctDigit)
import qualified Data.Text            as T
import           Numeric              (readHex, readOct)
import qualified Text.Parsec          as P

import           Text.Megaparsec      (ParsecT (..))
import qualified Text.Megaparsec.Char as MC

space :: Monad m => ParsecT e T.Text m () -> ParsecT e T.Text m () -> ParsecT e T.Text m () -> ParsecT e T.Text m ()
space sp lineComment blockComment = ParsecT $
    P.skipMany $ P.try (unParsecT sp) <|> P.try (unParsecT lineComment) <|> unParsecT blockComment

skipLineComment :: Monad m => T.Text -> ParsecT e T.Text m ()
skipLineComment prefix = ParsecT $
    P.try (unParsecT $ MC.string prefix) *> P.skipMany (P.noneOf "\n")

skipBlockComment :: Monad m => T.Text -> T.Text -> ParsecT e T.Text m ()
skipBlockComment start end = ParsecT $ do
    void $ P.try $ unParsecT $ MC.string start
    P.skipMany $ P.try $ P.notFollowedBy (P.try endParser) *> P.anyChar
    void $ P.try endParser
    where
        endParser = unParsecT $ MC.string end

lexeme :: Monad m => ParsecT e T.Text m () -> ParsecT e T.Text m a -> ParsecT e T.Text m a
lexeme sc parser = ParsecT $ unParsecT parser <* unParsecT sc

symbol :: Monad m => ParsecT e T.Text m () -> T.Text -> ParsecT e T.Text m T.Text
symbol sc = lexeme sc . MC.string

charLiteral :: Monad m => ParsecT e T.Text m Char
charLiteral = ParsecT $ escaped <|> P.noneOf ['\\']
    where
        escaped =
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

        octalEscape = do
            digits <- octalDigits
            byteFromDigits 8 digits

        hexDigits = do
            first <- P.satisfy isHexDigit
            if first == '0'
                then do
                    P.skipMany $ P.char '0'
                    P.option "0" $ significantHexDigits =<< P.satisfy isHexDigit
                else significantHexDigits first

        significantHexDigits first = do
            second <- P.option [] ((: []) <$> P.satisfy isHexDigit)
            P.optionMaybe (P.lookAhead $ P.satisfy isHexDigit) >>= \next ->
                case next of
                    Just _  -> invalidCodePoint
                    Nothing -> pure $ first : second

        octalDigits =
            (:) <$> P.satisfy isOctDigit <*> P.option [] (P.try $ P.count 2 (P.satisfy isOctDigit) <|> P.count 1 (P.satisfy isOctDigit))

        byteFromDigits base digits =
            let n = foldl (\acc c -> acc * base + digitToInt c) 0 digits
             in if n <= 0xff
                    then pure $ chr n
                    else invalidCodePoint

        invalidEscape =
            P.anyChar >>= \c -> fail ("invalid escape sequence \\" <> [c])

        invalidCodePoint = fail "character code point out of range"

decimal :: (Monad m, Num i) => ParsecT e T.Text m i
decimal = ParsecT $ fromInteger . read <$> P.many1 P.digit

hexadecimal :: (Monad m, Num i) => ParsecT e T.Text m i
hexadecimal = ParsecT $
    fromInteger . fst . head . (readHex :: String -> [(Integer, String)]) <$> P.many1 (P.satisfy isHexDigit)

octal :: (Monad m, Num i) => ParsecT e T.Text m i
octal = ParsecT $
    fromInteger . fst . head . (readOct :: String -> [(Integer, String)]) <$> P.many1 (P.satisfy isOctDigit)

signed :: (Monad m, Num i) => ParsecT e T.Text m () -> ParsecT e T.Text m i -> ParsecT e T.Text m i
signed sc parser = ParsecT $ do
    signFn <- P.option id $
        P.try ((negate <$ P.char '-') <* unParsecT sc)
            <|> P.try ((id <$ P.char '+') <* unParsecT sc)
    signFn <$> unParsecT parser
