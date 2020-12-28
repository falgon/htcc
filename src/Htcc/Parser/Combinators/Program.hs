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
  , conditional
) where

import           Control.Monad                               (forM, void, (>=>))
import           Control.Monad.Combinators                   (choice, some)
import           Control.Monad.Extra                         (ifM)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Control.Monad.Trans.State                   (get, gets, modify,
                                                              put)
import           Data.Bits                                   (Bits)
import qualified Data.ByteString.UTF8                        as BSU
import           Data.Char                                   (ord)
import           Data.Either                                 (rights)
import           Data.Functor                                ((<&>))
import           Data.Maybe                                  (fromJust,
                                                              fromMaybe)
import qualified Data.Text                                   as T
import           Data.Tuple.Extra                            (dupe, first)
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser.AST                             (Treealizable (..),
                                                              addKind, subKind)
import           Htcc.Parser.AST.Core                        (ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              atBlock, atBreak,
                                                              atCase, atComma,
                                                              atConditional,
                                                              atContinue,
                                                              atDefFunc,
                                                              atDefault, atElse,
                                                              atExprStmt, atFor,
                                                              atGVar, atIf,
                                                              atNoLeaf, atNull,
                                                              atNumLit, atLabel,
                                                              atReturn,
                                                              atSwitch, atUnary,
                                                              atWhile,
                                                              fromATKindFor, atGoto,
                                                              isEmptyExprStmt)
import           Htcc.Parser.AST.Type                        (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.Combinators.Type                (arraySuffix,
                                                              cType,
                                                              constantExp)
import           Htcc.Parser.Combinators.Utils               (bracket,
                                                              maybeToParser,
                                                              registerLVar)
import           Htcc.Parser.Combinators.Var                 (varInit)
import           Htcc.Parser.ConstructionData                (addFunction,
                                                              addGVar,
                                                              addGVarWith,
                                                              addLiteral,
                                                              fallBack,
                                                              incomplete,
                                                              isSwitchStmt,
                                                              lookupFunction,
                                                              lookupVar,
                                                              resetLocal,
                                                              succNest)
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

parser, program :: (Integral i, Bits i, Read i, Show i) => Parser i (ASTs i)
parser = (spaceConsumer >> program) <* M.eof
program = some global

global,
    function,
    gvar,
    stmt,
    expr,
    assign,
    conditional,
    logicalOr,
    logicalAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseAnd,
    equality,
    relational,
    shift,
    add,
    term,
    unary,
    factor :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)

global = choice
    [ ATEmpty <$ M.try (cType >> semi)
    , function
    , gvar
    ]

function = do
    (ty, ident) <- M.try (declIdent <* symbol "(")
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

gvar = do
    (ty, ident) <- declIdent
    choice
        [ nonInit ty ident
        , withInit ty ident
        ]
    where
        tmpTKIdent ident = (HT.TokenLCNums 1 1, HT.TKIdent ident)

        nonInit ty ident = do
            void semi
            ty' <- maybeToParser "defining global variables with a incomplete type" =<< lift (gets $ incomplete ty)
            lift (gets (addGVar ty' (tmpTKIdent ident)))
                >>= \case
                    Left err -> fail $ T.unpack $ fst err
                    Right (_, scp) -> ATEmpty <$ lift (put scp)

        withInit ty ident = do
            void $ symbol "="
            ty' <- maybeToParser "defining global variables with a incomplete type" =<< lift (gets $ incomplete ty)
            gvarInit ty' ident <* semi

        gvarInit ty ident = choice
                [ M.try fromConstant
                , fromOG
                ]
            where
                fromOG = do
                    ast <- conditional
                    case (atkind ast, atkind (atL ast)) of
                        (ATAddr, ATGVar _ name) -> lift (gets (gvarInitWithOG ty name))
                            >>= \case
                                Left err -> fail $ T.unpack $ fst err
                                Right (_, scp) -> ATEmpty <$ lift (put scp)
                        (ATAddr, _) -> fail "invalid initializer in global variable"
                        (ATGVar t name, _)
                            | CT.isCTArray t -> lift (gets (gvarInitWithOG ty name))
                                >>= \case
                                    Left err -> fail $ T.unpack $ fst err
                                    Right (_, scp) -> ATEmpty <$ lift (put scp)
                            -- TODO: support initializing from other global variables
                            | otherwise -> fail "initializer element is not constant"
                        _ -> fail "initializer element is not constant"

                gvarInitWithOG ty' to = addGVarWith ty' (tmpTKIdent ident) (PV.GVarInitWithOG to)
                gvarInitWithVal ty' to = addGVarWith ty' (tmpTKIdent ident) (PV.GVarInitWithVal to)

                fromConstant = do
                    cval <- constantExp
                    lift (gets (gvarInitWithVal ty cval))
                        >>= \case
                            Left err -> fail $ T.unpack $ fst err
                            Right (_, scp) -> ATEmpty <$ lift (put scp)

compoundStmt :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i [ATree i]
compoundStmt = bracket (lift get) (lift . modify . fallBack) $ const $
    braces (lift (modify succNest) *> M.many stmt)

stmt = choice
    [ returnStmt
    , ifStmt
    , whileStmt
    , forStmt
    , breakStmt
    , continueStmt
    , switchStmt
    , caseStmt
    , defaultStmt
    , gotoStmt
    , labelStmt
    , atBlock <$> compoundStmt
    , lvarStmt
    , atExprStmt <$> (expr <* semi)
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

        forStmt = (>>) (M.try kFor) $ bracket (lift get) (lift . modify . fallBack) $ const $ do
            es <- parens $ do
                lift $ modify succNest
                initSect <- ATForInit
                    <$> choice [ATEmpty <$ semi, M.try (atExprStmt <$> expr <* semi), lvarStmt]
                condSect <- ATForCond
                    <$> choice [atNumLit 1 <$ semi, expr <* semi]
                incrSect <- ATForIncr
                    <$> M.option ATEmpty (atExprStmt <$> expr)
                pure
                    [ x | x <- [initSect, condSect, incrSect]
                    , case fromATKindFor x of ATEmpty -> False; x' -> not $ isEmptyExprStmt x'
                    ]
            atFor es <$ semi M.<|> atFor . (es <>) . (:[]) . ATForStmt <$> stmt

        breakStmt = atBreak <$ (M.try kBreak *> semi)

        continueStmt = atContinue <$ (M.try kContinue *> semi)

        switchStmt = do
            cond <- M.try kSwitch *> parens expr
            bracket (putSwitchState True) (const $ putSwitchState False) (const stmt)
                >>= \case
                    ATNode (ATBlock ats) ty _ _ -> pure $ atSwitch cond ats ty
                    _ -> fail "expected compound statement after the token ')'"
            where
                putSwitchState b = lift $ modify $ \scp -> scp { isSwitchStmt = b }

        caseStmt = M.try kCase
            *> ifM (lift $ gets isSwitchStmt)
                ((atCase 0 <$> constantExp <* symbol ":") <*> stmt)
                (fail "stray 'case'")

        defaultStmt = (M.try kDefault <* symbol ":")
            *> ifM (lift $ gets isSwitchStmt)
                (atDefault 0 <$> stmt)
                (fail "stray 'default'")

        gotoStmt = atGoto <$> (M.try kGoto *> identifier <* semi)

        labelStmt = atLabel <$> M.try (identifier <* symbol ":")

        lvarStmt = choice
            [ ATEmpty <$ M.try (cType <* semi)
            , M.try (declIdent <* semi) >>= fmap atNull . uncurry registerLVar
            , (declIdent <* symbol "=" >>= uncurry (varInit assign)) <* semi
            ]

expr = assign

assign = do
    nd <- conditional
    M.option nd $ choice $ map (`id` nd) $
        [ assignOp ATAssign "="
        , assignOp ATMulAssign "*="
        , assignOp ATDivAssign "/="
        , assignOp ATAndAssign "&="
        , assignOp ATOrAssign "|="
        , assignOp ATXorAssign "^="
        , assignOp ATShlAssign "<<="
        , assignOp ATShrAssign ">>="
        , assignOp (maybe ATAddAssign (const ATAddPtrAssign) $ CT.deref (atype nd)) "+="
        , assignOp (maybe ATSubAssign (const ATSubPtrAssign) $ CT.deref (atype nd)) "-="
        ]
    where
        assignOp k s nd = symbol s *> (ATNode k (atype nd) nd <$> assign)

conditional = do
    nd <- logicalOr
    ifM (M.option False (True <$ M.lookAhead "?")) (gnuCondOmitted nd M.<|> condOp nd) $ pure nd
    where
        -- GNU extension (Conditionals with Omitted Operands, see also: https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html)
        gnuCondOmitted nd = M.try (symbol "?" *> symbol ":") *> ((atConditional (atype nd) nd ATEmpty) <$> conditional)
        condOp nd = uncurry (flip atConditional nd) . first atype . dupe
            <$> (symbol "?" *> expr <* symbol ":")
            <*> conditional

logicalOr = binaryOperator logicalAnd [(symbol "||", binOpBool ATLOr)]

logicalAnd = binaryOperator bitwiseOr [(symbol "&&", binOpBool ATLAnd)]

bitwiseOr = binaryOperator bitwiseXor [(symbol "|", binOpIntOnly ATOr)]

bitwiseXor = binaryOperator bitwiseAnd [(symbol "^", binOpIntOnly ATXor)]

bitwiseAnd = binaryOperator equality [(MC.char '&' `notFollowedOp` MC.char '&', binOpIntOnly ATAnd)]

equality = binaryOperator relational
    [ (symbol "==", binOpBool ATEQ)
    , (symbol "!=", binOpBool ATNEQ)
    ]

relational = binaryOperator shift
    [ (symbol "<=", binOpBool ATLEQ)
    , (symbol "<",  binOpBool ATLT)
    , (symbol ">=", binOpBool ATGEQ)
    , (symbol ">",  binOpBool ATGT)
    ]

shift = binaryOperator add
    [ (symbol "<<", binOpIntOnly ATShl)
    , (symbol ">>", binOpIntOnly ATShr)
    ]

add = binaryOperator term
    [ (symbol "+", \l r -> maybeToParser "invalid operands" $ addKind l r)
    , (symbol "-", \l r -> maybeToParser "invalid operands" $ subKind l r)
    ]

term = binaryOperator unary
    [ (symbol "*", binOpCon ATMul)
    , (symbol "/", binOpCon ATDiv)
    , (symbol "%", binOpCon ATMod)
    ]

unary = choice
    [ symbol "++" *> unary <&> \n -> ATNode ATPreInc (atype n) n ATEmpty
    , symbol "--" *> unary <&> \n -> ATNode ATPreDec (atype n) n ATEmpty
    , symbol "+" *> unary
    , symbol "-" *> unary <&> \n -> ATNode ATSub (atype n) (atNumLit 0) n
    , symbol "!" *> unary <&> flip (ATNode ATNot (CT.SCAuto CT.CTBool)) ATEmpty
    , symbol "~" *> unary <&> flip (ATNode ATBitNot (CT.SCAuto CT.CTInt)) ATEmpty
    , addr
    , symbol "*" *> unary >>= deref'
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
                    , postInc fac
                    , postDec fac
                    ]

                idxAcc fac = do
                    idx <- brackets expr
                    kt <- maybeToParser "invalid operands" (addKind fac idx)
                    ty <- maybeToParser "subscripted value is neither array nor pointer nor vector" $ CT.deref $ atype kt
                    ty' <- maybeToParser "incomplete value dereference" =<< lift (gets $ incomplete ty)
                    allAcc $ atUnary ATDeref ty' kt

                postInc fac = allAcc =<< atUnary ATPostInc (atype fac) fac <$ symbol "++"
                postDec fac = allAcc =<< atUnary ATPostDec (atype fac) fac <$ symbol "--"

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
    , atNumLit . fromIntegral . ord <$> charLiteral
    , sizeof
    , strLiteral
    , identifier'
    , M.try (parens expr)
    , gnuStmtExpr
    , ATEmpty <$ M.eof
    ]
    where
        sizeof = kSizeof >> choice
            [ incomplete <$> M.try (parens cType) <*> lift get
                >>= fmap (atNumLit . fromIntegral . CT.sizeof)
                . maybeToParser "invalid application of 'sizeof' to incomplete type"
            , atNumLit . fromIntegral . CT.sizeof . atype <$> unary
            ]

        strLiteral = do
            s <- stringLiteral
            lit <- lift $ gets $
                addLiteral (CT.SCAuto $ CT.CTArray (fromIntegral $ length s) CT.CTChar) $
                    (HT.TokenLCNums 1 1, HT.TKString $ BSU.fromString s)
            case lit of
                Left err        -> fail $ T.unpack $ fst err
                Right (nd, scp) -> nd <$ lift (put scp)

        gnuStmtExpr = do
            k <- parens compoundStmt
            if null k then fail "void value not ignored as it ought to be" else case last k of
                (ATNode ATExprStmt _ n _) -> pure $ atNoLeaf (ATStmtExpr $ init k <> [n]) (atype n)
                _ -> fail "void value not ignored as it ought to be"

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
