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
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TupleSections #-}
module Htcc.Parser.Combinators.Program (
    parser
  , logicalOr
) where

import           Control.Monad                               (forM, void, (>=>))
import           Control.Monad.Combinators                   (choice, some)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Control.Monad.Trans.State                   (get, gets, modify,
                                                              put)
import           Data.Bits                                   (Bits (..))
import           Data.Either                                 (rights)
import           Data.Functor                                ((<&>))
import           Data.Maybe                                  (fromJust,
                                                              fromMaybe)
import qualified Data.Text                                   as T
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser.AST                             (Treealizable (..),
                                                              addKind, subKind)
import           Htcc.Parser.AST.Core                        (ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              atBlock,
                                                              atDefFunc, atElse,
                                                              atExprStmt, atFor,
                                                              atGVar, atIf,
                                                              atNoLeaf, atNull,
                                                              atNumLit,
                                                              atReturn, atUnary,
                                                              atWhile)
import           Htcc.Parser.AST.Type                        (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.Combinators.Type                (arraySuffix,
                                                              cType)
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
import qualified Text.Megaparsec                             as M
import qualified Text.Megaparsec.Char                        as MC

import           Text.Megaparsec.Debug                       (dbg)

declIdent :: (Show i, Read i, Bits i, Integral i) => Parser i (CT.StorageClass i, T.Text)
declIdent = do
    ty <- cType
    ident <- identifier
    (,ident) <$> M.option ty (arraySuffix ty)

declIdentFuncArg :: (Show i, Read i, Bits i, Integral i)
    => Parser i a
    -> Parser i (Either (CT.StorageClass i) (CT.StorageClass i, T.Text))
declIdentFuncArg sep = do
    ty <- cType
    anonymousArg ty M.<|> namedArg ty
    where
        anonymousArg ty = Left <$> M.option ty (arraySuffix ty) <* sep
        namedArg ty = do
            ident <- identifier
            Right . (,ident) <$> M.option ty (narrowPtr <$> arraySuffix ty) <* sep
        
        narrowPtr ty
            | CT.isCTArray ty = fromMaybe ty $ CT.mapTypeKind CT.CTPtr <$> CT.deref ty
            | CT.isIncompleteArray ty =
                CT.mapTypeKind (\(CT.CTIncomplete (CT.IncompleteArray t')) -> CT.CTPtr t') ty
            | otherwise = ty

registerLVar :: (Bits i, Integral i) => CT.StorageClass i -> T.Text -> Parser i (ATree i)
registerLVar ty ident =
    lift (gets $ addLVar ty (HT.TokenLCNums 1 1, HT.TKIdent ident))
        >>= \case
            Right (lat, scp') -> lift (lat <$ put scp')
            Left err          -> fail $ T.unpack $ fst err

parser, program :: (Integral i, Bits i, Read i, Show i) => Parser i (ASTs i)
parser = (spaceConsumer >> program) <* M.eof
program = some global

global,
    function,
    stmt,
    lvarStmt,
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
    sizeof,
    identifier' :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)

global = choice
    [ function
    ]

function = do
    (ty, ident) <- declIdent <* symbol "("
    params <- takeParameters
    lift $ modify resetLocal
    choice
        [ declaration ty ident
        , definition ty ident params
        ]
    where
        takeParameters =
            M.manyTill (M.try (declIdentFuncArg comma) M.<|> (declIdentFuncArg $ M.lookAhead (symbol ")"))) (symbol ")")

        declaration ty ident =
            void semi
                >> lift (gets $ addFunction False ty (HT.TokenLCNums 1 1, HT.TKIdent ident))
                >>= \case
                    Right scp' -> ATEmpty <$ lift (put scp')
                    Left err   -> fail $ T.unpack $ fst err

        definition ty ident params =
            void (M.lookAhead $ symbol "{")
                >> lift (gets $ addFunction True ty (HT.TokenLCNums 1 1, HT.TKIdent ident))
                >>= \case
                    Right scp' -> do
                        lift $ put scp'
                        params' <- forM (rights params) $ uncurry registerLVar
                        atDefFunc ident (if null params' then Nothing else Just params') ty <$> stmt
                    Left err -> fail $ T.unpack $ fst err

lvarStmt = choice
    [ ATEmpty <$ M.try (cType <* semi)
    , declIdent <* semi >>= fmap atNull . uncurry registerLVar
    ]

stmt = choice
    [ returnStmt
    , ifStmt
    , whileStmt
    , forStmt
    , compoundStmt
    , lvarStmt
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
    [ (symbol "+", \l r -> maybe (fail "invalid operands") pure $ addKind l r)
    , (symbol "-", \l r -> maybe (fail "invalid operands") pure $ subKind l r)
    ]

term = binaryOperator unary
    [ (symbol "*", binOpCon ATMul)
    , (symbol "/", binOpCon ATDiv)
    , (symbol "%", binOpCon ATMod)
    ]

unary = choice
    [ symbol "+" >> factor
    , symbol "-" >> factor <&> \n -> ATNode ATSub (atype n) (atNumLit 0) n
    , addr
    , symbol "*" >> unary >>= deref'
    , factor'
    ]
    where
        addr = MC.char '&' `notFollowedOp` MC.char '&' >> unary <&> \n ->
            let ty = if CT.isArray (atype n) then fromJust $ CT.deref $ atype n else atype n in
                atUnary ATAddr (CT.mapTypeKind CT.CTPtr ty) n

        factor' = factor >>= allAcc
            where
                allAcc fac = M.option fac $ choice
                    [ idxAcc fac
                    ]

                idxAcc fac = do
                    idx <- brackets expr
                    kt <- maybe (fail "invalid operands") pure (addKind fac idx)
                    ty <- maybe (fail "subscripted value is neither array nor pointer nor vector") pure 
                        $ CT.deref $ atype kt
                    ty' <- maybe (fail "incomplete value dereference") pure =<< lift (gets $ incomplete ty)
                    allAcc $ atUnary ATDeref ty' kt

        deref' = runMaybeT . deref'' >=> maybe M.empty pure
            where
                deref'' n = do
                    ty <- MaybeT $ pure (CT.deref $ atype n)
                    case CT.toTypeKind ty of
                        CT.CTVoid -> lift $ fail "void value not ignored as it ought to be"
                        _ -> MaybeT (lift $ gets $ incomplete ty)
                            >>= lift . pure . flip (atUnary ATDeref) n

factor = choice
    [ atNumLit <$> natural
    , sizeof
    , identifier'
    , parens expr
    , ATEmpty <$ M.eof
    ]

sizeof = kSizeof >> choice
    [ incomplete <$> M.try (parens cType) <*> lift get
        >>= maybe (fail "invalid application of 'sizeof' to incomplete type")
            (pure . atNumLit . fromIntegral . CT.sizeof)
    , atNumLit . fromIntegral . CT.sizeof . atype <$> unary
    ]

identifier' = do
    ident <- identifier
    choice
        [ fnCall ident
        , variable ident
        ]
    where
        variable ident = do
            lift (gets $ lookupVar ident)
                >>= \case
                    FoundGVar (PV.GVar t _) -> return $ atGVar t ident
                    FoundLVar sct -> return $ treealize sct
                    FoundEnum sct -> return $ treealize sct
                    NotFound -> fail $ "The '" <> T.unpack ident <> "' is not defined identifier"

        fnCall ident = do
            params <- symbol "(" >> M.manyTill (M.try (expr <* comma) M.<|> expr) (symbol ")")
            let params' = if null params then Nothing else Just params
            lift (gets $ lookupFunction ident) <&> \case
                -- TODO: set warning message
                -- TODO: Infer the return type of a function
                Nothing -> atNoLeaf (ATCallFunc ident params') (CT.SCAuto CT.CTInt)
                Just fn -> atNoLeaf (ATCallFunc ident params') (PSF.fntype fn)
