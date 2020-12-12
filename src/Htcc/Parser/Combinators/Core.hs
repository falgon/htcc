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
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, TupleSections #-}
module Htcc.Parser.Combinators.Core (
    runParser
  , Parser
  , spaceConsumer
  , lexme
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
  , commaSep
  , commaSep1
) where

import           Control.Applicative                    (Alternative (..))
import           Control.Monad.Combinators              (between)
import           Control.Monad.Trans.State.Lazy
import           Data.Char                              (isAlpha)
import           Data.Functor.Identity
import qualified Data.Text                              as T
import           Data.Void
import qualified Htcc.CRules                            as CR
import           Htcc.Parser.AST.Type                   (ASTs)
import           Htcc.Parser.ConstructionData           (ConstructionData (..),
                                                         Warnings,
                                                         initConstructionData)
import qualified Htcc.Parser.ConstructionData.Scope     as PS
import qualified Htcc.Parser.ConstructionData.Scope.Var as PSV
import           Htcc.Utils                             (lor)
import qualified Text.Megaparsec                        as M
import qualified Text.Megaparsec.Char                   as MC
import qualified Text.Megaparsec.Char.Lexer             as ML

type ConstructionDataState i = StateT (ConstructionData i) Identity
type Parser i = M.ParsecT Void T.Text (ConstructionDataState i)

runParser ::
    Parser i (ASTs i) ->
    FilePath ->
    T.Text ->
    Either (M.ParseErrorBundle T.Text Void) (Warnings i, ASTs i, PSV.GlobalVars i, PSV.Literals i)
runParser p fp input =
    (warns (snd result),, PSV.globals $ PS.vars $ scope $ snd result, PSV.literals $ PS.vars $ scope $ snd result)
        <$> fst result
    where
        result = runIdentity $ runStateT (M.runParserT p fp input) initConstructionData

spaceConsumer :: Ord e => M.ParsecT e T.Text m ()
spaceConsumer = ML.space MC.space1 (ML.skipLineComment "//") (ML.skipBlockComment "/*" "*/")

lexme :: Ord e => M.ParsecT e T.Text m a -> M.ParsecT e T.Text m a
lexme = ML.lexeme spaceConsumer

symbol :: Ord e => T.Text -> M.ParsecT e T.Text m T.Text
symbol = ML.symbol spaceConsumer

charLiteral :: Ord e => M.ParsecT e T.Text m Char
charLiteral = M.between (MC.char '\'') (MC.char '\'') ML.charLiteral

stringLiteral :: Ord e => M.ParsecT e T.Text m String
stringLiteral = MC.char '\"' *> M.manyTill ML.charLiteral (MC.char '\"')

hexadecimal, octal, decimal, natural, integer :: (Ord e, Num i) => M.ParsecT e T.Text m i
hexadecimal = MC.char '0' >> MC.char' 'x' >> ML.hexadecimal
octal = MC.char '0' >> ML.octal
decimal = ML.decimal
natural = M.try (lexme hexadecimal) <|> M.try (lexme octal) <|> lexme decimal
integer = ML.signed spaceConsumer natural <|> natural

parens, braces, angles, brackets :: Ord e => M.ParsecT e T.Text m a -> M.ParsecT e T.Text m a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
angles = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")

identifier, semi, comma, colon :: Ord e => M.ParsecT e T.Text m T.Text
identifier =
    mappend
        <$> M.takeWhile1P (Just "valid identifier") (lor [isAlpha, (=='_')])
        <*> M.takeWhileP (Just "valid identifier") CR.isValidChar
        <* spaceConsumer

semi = symbol ";"
comma = symbol ","
colon = symbol "."

commaSep, commaSep1 :: Ord e => M.ParsecT e T.Text m T.Text -> M.ParsecT e T.Text m [T.Text]
commaSep = flip M.sepBy comma
commaSep1 = flip M.sepBy1 comma
