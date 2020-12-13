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
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}
module Htcc.Parser.Combinators.Program (
    parser
) where

import           Control.Monad                               (forM, void, (>=>))
import           Control.Monad.Combinators                   (choice, some)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Control.Monad.Trans.State                   (get, gets, modify,
                                                              put)
import           Data.Bits                                   (Bits (..))
import           Data.Functor                                ((<&>))
import           Data.Maybe                                  (fromJust,
                                                              fromMaybe)
import qualified Data.Text                                   as T
import           Htcc.CRules.Types                           as CT
import           Htcc.Parser.AST                             (Treealizable (..))
import           Htcc.Parser.AST.Core                        (ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              atBlock,
                                                              atDefFunc, atElse,
                                                              atExprStmt, atFor,
                                                              atGVar, atIf,
                                                              atNoLeaf,
                                                              atNumLit,
                                                              atReturn, atUnary,
                                                              atWhile)
import           Htcc.Parser.AST.Type                        (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.ConstructionData                (addFunction,
                                                              addLVar,
                                                              incomplete,
                                                              lookupFunction,
                                                              lookupVar,
                                                              resetLocal)
import           Htcc.Parser.ConstructionData.Scope          (LookupVarResult (..))
import qualified Htcc.Parser.ConstructionData.Scope.Function as PSF
import qualified Htcc.Parser.ConstructionData.Scope.Var      as PV
import qualified Htcc.Tokenizer.Token                        as HT
import           Htcc.Utils                                  (maybe')
import qualified Text.Megaparsec                             as M
import qualified Text.Megaparsec.Char                        as MC

import           Text.Megaparsec.Debug                       (dbg)

parser, program :: (Integral i, Ord i, Bits i, Show i) => Parser i (ASTs i)
parser = (spaceConsumer >> program) <* M.eof
program = some global

global,
    function,
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

global = choice
    [ function
    ]

function = do
    ident <- identifier
    params <- symbol "(" >> M.manyTill (M.try (identifier <* comma) M.<|> identifier) (symbol ")")
    lift $ modify resetLocal
    choice
        [ declaration ident
        , definition ident params
        ]
    where
        declaration ident = do
            void semi
            scp <- lift get
            case addFunction False (CT.SCAuto CT.CTInt) (HT.TokenLCNums 1 1, HT.TKIdent ident) scp of
                Right scp' -> ATEmpty <$ lift (put scp')
                Left y     -> fail $ T.unpack $ fst y

        definition ident params = do
            void $ M.lookAhead (symbol "{")
            params' <- forM params $ \p -> do
                scp <- lift get
                case addLVar (CT.SCAuto CT.CTInt) (HT.TokenLCNums 1 1, HT.TKIdent p) scp of
                    Right (lat, scp') -> lat <$ lift (put scp')
                    Left x            -> fail $ T.unpack $ fst x
            atDefFunc ident (if null params' then Nothing else Just params') (CT.SCAuto CT.CTInt) <$> stmt

stmt = choice
    [ returnStmt
    , ifStmt
    , whileStmt
    , forStmt
    , compoundStmt
    , expr <* semi
    , ATEmpty <$ semi
    ]
    where
        returnStmt = choice
            [ atReturn (CT.SCUndef CT.CTUndef) <$> (M.try kReturn >> expr) <* semi
            --, atReturn (CT.SCUndef CT.CTUndef) ATEmpty <$ (kReturn >> semi)
            ]

        ifStmt = do
            r <- atIf <$> (M.try kIf >> parens expr) <*> stmt
            M.option ATEmpty (M.try kElse >> stmt) <&> \case
                ATEmpty -> r
                nd -> atElse r nd

        whileStmt = atWhile <$> (M.try kWhile >> parens expr) <*> stmt

        forStmt = do
            es <- (>>) (M.try kFor) $ parens $ do
                initSect <- ATForInit . atExprStmt
                    <$> choice [ATEmpty <$ semi,  expr <* semi]
                condSect <- ATForCond
                    <$> choice [atNumLit 1 <$ semi, expr <* semi]
                incrSect <- ATForIncr . atExprStmt
                    <$> M.option ATEmpty expr
                pure [initSect, condSect, incrSect]
            atFor . (es <>) . (:[]) . ATForStmt <$> stmt

        compoundStmt = atBlock <$> braces (M.many stmt)

expr = assign

assign = do
    nd <- logicalOr
    choice
        [ symbol "=" >> (ATNode ATAssign (atype nd) nd <$> assign)
        , pure nd
        ]

logicalOr = binaryOperator logicalAnd [(symbol "||", binOpBool ATLOr)]
logicalAnd = binaryOperator bitwiseOr [(symbol "&&", binOpBool ATLAnd)]
bitwiseOr = binaryOperator bitwiseXor [(symbol "|", binOpIntOnly ATOr)]
bitwiseXor = binaryOperator bitwiseAnd [(symbol "^", binOpIntOnly ATXor)]
bitwiseAnd = binaryOperator equality [(MC.char '&' `notFollowedOp` MC.char '&', binOpIntOnly ATAnd)]

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
    , (symbol "%", binOpCon ATMod)
    ]

unary = choice
    [ symbol "+" >> factor
    , symbol "-" >> factor <&> \n -> ATNode ATSub (atype n) (atNumLit 0) n
    , MC.char '&' `notFollowedOp` MC.char '&' >> unary <&> \n ->
        let ty = if CT.isArray (atype n) then fromJust $ CT.deref $ atype n else atype n in
            atUnary ATAddr (CT.mapTypeKind CT.CTPtr ty) n
    , symbol "*" >> unary >>= deref'
    , factor
    ]
    where
        deref' :: Ord i => ATree i -> Parser i (ATree i)
        deref' = runMaybeT . deref'' >=> maybe M.empty pure

        deref'' :: Ord i => ATree i -> MaybeT (Parser i) (ATree i)
        deref'' n = lift $ pure $ atUnary ATDeref (CT.SCAuto CT.CTInt) n

        {- After implementing the type, use:
        deref'' n = do
            ty <- MaybeT $ pure (CT.deref $ atype n)
            case CT.toTypeKind ty of
                CT.CTVoid -> lift $ fail "void value not ignored as it ought to be"
                _ -> do
                    scp <- lift $ lift get
                    ty' <- MaybeT $ pure (incomplete ty scp)
                    lift $ pure $ atUnary ATDeref ty' n
        -}

factor = choice
    [ atNumLit <$> natural
    , identifier'
    , parens expr
    , ATEmpty <$ M.eof
    ]

identifier' = do
    ident <- identifier
    choice
        [ fnCall ident
        , variable ident
        ]
    where
        variable ident = lift $ do
            scp <- get
            case lookupVar ident scp of
                FoundGVar (PV.GVar t _) -> return $ atGVar t ident
                FoundLVar sct -> return $ treealize sct
                FoundEnum sct -> return $ treealize sct
                NotFound -> let Right (lat, scp') = addLVar (CT.SCAuto CT.CTInt) (HT.TokenLCNums 1 1, HT.TKIdent ident) scp in do
                    put scp'
                    return lat

        fnCall ident = do
            params <- symbol "(" >> M.manyTill (M.try (expr <* comma) M.<|> expr) (symbol ")")
            let params' = if null params then Nothing else Just params
            lift $ do
                scp <- get
                return $ case lookupFunction ident scp of
                    -- TODO: set warning message
                    -- TODO: Infer the return type of a function
                    Nothing -> atNoLeaf (ATCallFunc ident params') (CT.SCAuto CT.CTInt)
                    Just fn -> atNoLeaf (ATCallFunc ident params') (PSF.fntype fn)
