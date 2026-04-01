{-|
Module      : Htcc.Parser.Combinators.GNUExtensions
Description : Combinators of GNU extensions
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Combinators of GNU extensions
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.GNUExtensions (
    condOmitted
  , stmtExpr
) where

import                          Data.Bits                       (Bits)
import                          Htcc.Parser.AST.Core            (ATKind (..),
                                                                 ATree (..),
                                                                 atConditional,
                                                                 atNoLeaf)
import                          Htcc.Parser.Combinators.Core
import {-# SOURCE #-}           Htcc.Parser.Combinators.Program (compoundStmt,
                                                                 conditional)
import                          Htcc.Parser.Combinators.Utils   (conditionalResultType,
                                                                 decayExprType,
                                                                 maybeToParser)
import                qualified Text.Megaparsec                 as M


-- Conditionals with Omitted Operands, see also: https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html
condOmitted :: (Ord i, Bits i, Read i, Show i, Integral i) => ATree i -> Parser i (ATree i)
condOmitted nd = M.try (symbol "?" *> symbol ":") *> do
    el <- conditional
    ty <- maybeToParser "invalid operands" $ conditionalResultType nd el
    pure $ atConditional ty nd ATEmpty el

-- Statements and Declarations in Expressions, see also: https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html
stmtExpr :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)
stmtExpr = do
    k <- parens compoundStmt
    if null k then fail "void value not ignored as it ought to be" else case last k of
        (ATNode ATExprStmt _ n _) -> pure $ atNoLeaf (ATStmtExpr $ init k <> [n]) (decayExprType $ atype n)
        _ -> fail "void value not ignored as it ought to be"
