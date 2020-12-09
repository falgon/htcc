{-|
Module      : Htcc.Parser.Combinators.Program
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Htcc.Parser.Combinators.Program (
    parser
) where

import           Control.Applicative            hiding (many)
import           Control.Monad.Combinators      (between, choice)
import           Control.Monad.Combinators.Expr
import           Control.Monad.Fix              (fix)
import qualified Data.Text                      as T
import           Htcc.CRules.Types              as CT
import           Htcc.Parser.AST.Core           (ATKind (..), ATree (..),
                                                 atNumLit)
import           Htcc.Parser.AST.Type           (ASTs)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Development        (defMainFn)
import qualified Text.Megaparsec                as M
import qualified Text.Megaparsec.Char           as MC

parser :: Num i => Parser i (ASTs i)
parser = (:[]) . defMainFn <$> (spaceConsumer >> expr)

expr :: Num i => Parser i (ATree i)
expr = makeExprParser term operatorTable M.<?> "expression"

term :: Num i => Parser i (ATree i)
term = choice [parens expr, atNumLit <$> natural] M.<?> "term"

operatorTable :: Num i => [[Operator (Parser i) (ATree i)]]
operatorTable =
    [ [ binary "==" (ATNode ATEQ (CT.SCAuto CT.CTInt))
      , binary "!=" (ATNode ATNEQ (CT.SCAuto CT.CTInt))
      ]
    , [ binary "<=" (ATNode ATLEQ (CT.SCAuto CT.CTInt))
      , binary "<" (ATNode ATLT (CT.SCAuto CT.CTInt))
      , binary ">=" (ATNode ATGEQ (CT.SCAuto CT.CTInt))
      , binary ">" (ATNode ATGT (CT.SCAuto CT.CTInt))
      ]
    , [ prefix "-" (ATNode ATSub (CT.SCAuto CT.CTInt) (atNumLit 0))
      , prefix "+" id
      ]
    , [ binary "*" (ATNode ATMul (CT.SCAuto CT.CTInt))
      , binary "/" (ATNode ATDiv (CT.SCAuto CT.CTInt))
      ]
    , [ binary "+" (ATNode ATAdd (CT.SCAuto CT.CTInt))
      , binary "-" (ATNode ATSub (CT.SCAuto CT.CTInt))
      ]
    ]
    where
        prefix name f = Prefix (f <$ symbol name)
        binary name f = InfixL (f <$ symbol name)

{-
expr :: Num i => Parser i (ATree i)
expr = do
    m <- term
    ($ m) . fix $ \f nd ->
        ((ATNode ATAdd (CT.SCAuto CT.CTInt) nd <$> (symbol "+" >> term)) >>= f)
            <|> ((ATNode ATSub (CT.SCAuto CT.CTInt) nd <$> (symbol "-" >> term)) >>= f)
            <|> return nd

term :: Num i => Parser i (ATree i)
term = do
    fac <- factor
    ($ fac) . fix $ \f nd ->
        ((ATNode ATMul (CT.SCAuto CT.CTInt) nd <$> (symbol "*" >> factor)) >>= f)
            <|> ((ATNode ATDiv (CT.SCAuto CT.CTInt) nd <$> (symbol "/" >> factor)) >>= f)
            <|> return nd

factor :: Num i => Parser i (ATree i)
factor = atNumLit <$> integer <|> between (symbol "(") (symbol ")") expr
-}
