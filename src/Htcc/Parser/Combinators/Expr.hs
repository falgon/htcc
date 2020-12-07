{-|
Module      : Htcc.Parser.Combinators.Expr
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Htcc.Parser.Combinators.Expr (
    parser
) where

import           Control.Applicative          hiding (many)
import           Control.Monad.Combinators    (between)
import           Control.Monad.Fix            (fix)
import           Htcc.CRules.Types            as CT
import           Htcc.Parser.AST.Core         (ATKind (..), ATree (..),
                                               atNumLit)
import           Htcc.Parser.AST.Type         (ASTs)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Development      (defMainFn)

parser :: Num i => Parser i (ASTs i)
parser = (:[]) . defMainFn <$> expr

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

