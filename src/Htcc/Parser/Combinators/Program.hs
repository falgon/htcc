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

import           Control.Monad.Combinators              (choice, some)
import           Control.Monad.Trans                    (MonadTrans (..))
import           Control.Monad.Trans.State              (get, put)
import           Data.Bits                              (Bits (..))
import           Htcc.CRules.Types                      as CT
import           Htcc.Parser.AST                        (Treealizable (..))
import           Htcc.Parser.AST.Core                   (ATKind (..),
                                                         ATree (..), atBlock,
                                                         atGVar, atNumLit,
                                                         atReturn)
import           Htcc.Parser.AST.Type                   (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.ConstructionData           (addLVar, lookupVar)
import           Htcc.Parser.ConstructionData.Scope     (LookupVarResult (..))
import qualified Htcc.Parser.ConstructionData.Scope.Var as PV
import           Htcc.Parser.Development                (defMainFn)
import qualified Htcc.Tokenizer.Token                   as HT
import qualified Text.Megaparsec                        as M

parser :: (Integral i, Ord i, Bits i, Show i) => Parser i (ASTs i)
parser = (:[]) . defMainFn . atBlock <$> (spaceConsumer >> program) <* M.eof

program :: (Integral i, Ord i, Bits i, Show i) => Parser i (ASTs i)
program = some stmt

stmt,
    expr,
    assign,
    logicalOr,
    logicalAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseAnd,
    equality,
    relational,
    add,
    term,
    unary,
    factor,
    identifier' :: (Ord i, Bits i, Show i, Integral i) => Parser i (ATree i)

stmt = choice
    [ (atReturn (CT.SCUndef CT.CTUndef) <$> (kReturn >> expr)) <* semi
    , expr <* semi
    ]

expr = assign

assign = do
    nd <- logicalOr
    choice
        [ symbol "=" >> (ATNode ATAssign (atype nd) nd <$> assign)
        , return nd
        ]

logicalOr = binaryOperator logicalAnd [(symbol "||", binOpBool ATLOr)]
logicalAnd = binaryOperator bitwiseOr [(symbol "&&", binOpBool ATLAnd)]
bitwiseOr = binaryOperator bitwiseXor [(symbol "|", binOpIntOnly ATOr)]
bitwiseXor = binaryOperator bitwiseAnd [(symbol "^", binOpIntOnly ATXor)]
bitwiseAnd = binaryOperator equality [(symbol "&", binOpIntOnly ATAnd)]

equality = binaryOperator relational
    [ (symbol "==", binOpBool ATEQ)
    , (symbol "!=", binOpBool ATNEQ)
    ]

relational = binaryOperator add
    [ (symbol "<=", binOpBool ATLEQ)
    , (symbol "<",  binOpBool ATLT)
    , (symbol ">=", binOpBool ATGEQ)
    , (symbol ">",  binOpBool ATGT)
    ]

add = binaryOperator term
    [ (symbol "+", binOpCon ATAdd)
    , (symbol "-", binOpCon ATSub)
    ]

term = binaryOperator unary
    [ (symbol "*", binOpCon ATMul)
    , (symbol "/", binOpCon ATDiv)
    ]

unary = choice
    [ symbol "+" >> factor
    , (\n -> ATNode ATSub (atype n) (atNumLit 0) n) <$> (symbol "-" >> factor)
    , factor
    ]

factor = choice
    [ atNumLit <$> natural
    , identifier'
    , parens expr
    , ATEmpty <$ M.eof
    ]

identifier' = do
    ident <- identifier
    lift $ do
        scp <- get
        case lookupVar ident scp of
            FoundGVar (PV.GVar t _) -> return $ atGVar t ident
            FoundLVar sct -> return $ treealize sct
            FoundEnum sct -> return $ treealize sct
            NotFound -> let Right (lat, scp') = addLVar (CT.SCAuto CT.CTInt) (HT.TokenLCNums 1 1, HT.TKIdent ident) scp in do
                put scp'
                return lat
