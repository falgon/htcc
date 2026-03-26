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
import           Data.Char            (chr, isHexDigit, isOctDigit)
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
    void $ P.try (unParsecT $ MC.string prefix) *> P.many (P.noneOf "\n")

skipBlockComment :: Monad m => T.Text -> T.Text -> ParsecT e T.Text m ()
skipBlockComment start end = ParsecT $
    void $ P.try (unParsecT $ MC.string start) *> P.manyTill P.anyChar (P.try $ unParsecT $ MC.string end)

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
            digits <- P.char 'x' *> P.many1 (P.satisfy isHexDigit)
            maybe invalidCodePoint pure $ decode readHex digits

        octalEscape = do
            digits <- octalDigits
            maybe invalidCodePoint pure $ decode readOct digits

        octalDigits =
            (:) <$> P.satisfy isOctDigit <*> P.option [] (P.try $ P.count 2 (P.satisfy isOctDigit) <|> P.count 1 (P.satisfy isOctDigit))

        decode :: (String -> [(Integer, String)]) -> String -> Maybe Char
        decode reader digits = safeChr . fst =<< listToMaybe (reader digits)

        listToMaybe []      = Nothing
        listToMaybe (x : _) = Just x

        safeChr n
            | n < 0 = Nothing
            | n > fromIntegral (fromEnum (maxBound :: Char)) = Nothing
            | 0xD800 <= n && n <= 0xDFFF = Nothing
            | otherwise = Just (chr $ fromIntegral n)

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
