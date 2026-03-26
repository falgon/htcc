module Text.Megaparsec.Char (
    char,
    char',
    string,
    space1
) where

import           Control.Monad   (void)
import           Data.Char       (isSpace, toLower)
import qualified Data.Text       as T
import qualified Text.Parsec     as P

import           Text.Megaparsec (ParsecT (..))

char :: Monad m => Char -> ParsecT e T.Text m Char
char = ParsecT . P.char

char' :: Monad m => Char -> ParsecT e T.Text m Char
char' c = ParsecT $ P.satisfy ((== toLower c) . toLower)

string :: Monad m => T.Text -> ParsecT e T.Text m T.Text
string = ParsecT . fmap T.pack . P.try . P.string . T.unpack

space1 :: Monad m => ParsecT e T.Text m ()
space1 = ParsecT $ void $ P.many1 $ P.satisfy isSpace
