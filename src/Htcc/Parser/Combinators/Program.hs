{-|
Module      : Htcc.Parser.Combinators.Program
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language Program parser
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TupleSections #-}
module Htcc.Parser.Combinators.Program (
    parser
  , conditional
  , compoundStmt
) where

import           Control.Monad                               (void, when, (>=>))
import           Control.Monad.Combinators                   (choice, some)
import           Control.Monad.Extra                         (ifM)
import           Control.Monad.State                         (get, gets, modify)
import           Control.Monad.Trans                         (MonadTrans (..))
import           Control.Monad.Trans.Maybe                   (MaybeT (..),
                                                              runMaybeT)
import           Data.Bits                                   (Bits)
import           Data.Char                                   (ord)
import           Data.Functor                                ((<&>))
import           Data.List                                   (find)
import           Data.Maybe                                  (fromJust, isJust)
import qualified Data.Text                                   as T
import           Data.Tuple.Extra                            (dupe, first)
import qualified Htcc.CRules.Types                           as CT
import           Htcc.Parser.AST                             (Treealizable (..),
                                                              addKind,
                                                              isEmptyReturn,
                                                              isNonEmptyReturn,
                                                              subKind)
import           Htcc.Parser.AST.Core                        (ATKind (..),
                                                              ATKindFor (..),
                                                              ATree (..),
                                                              atBlock, atBreak,
                                                              atCase, atCast,
                                                              atConditional,
                                                              atContinue,
                                                              atDefFunc,
                                                              atDefault, atElse,
                                                              atExprStmt, atFor,
                                                              atGVar, atGoto,
                                                              atIf, atLabel,
                                                              atNoLeaf, atNull,
                                                              atNumLit,
                                                              atReturn,
                                                              atSwitch, atUnary,
                                                              atWhile,
                                                              fromATKindFor,
                                                              isEmptyExprStmt)
import           Htcc.Parser.AST.Type                        (ASTs)
import           Htcc.Parser.Combinators.BasicOperator
import           Htcc.Parser.Combinators.ConstExpr           (evalConstexpr)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Decl                (absDeclarator, declarator, declspec)
import qualified Htcc.Parser.Combinators.GNUExtensions       as GNU
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.Combinators.Type                (toNamedParams)
import           Htcc.Parser.Combinators.Utils               (bracket,
                                                              getPosState,
                                                              maybeToParser,
                                                              registerFunc,
                                                              registerGVar,
                                                              registerGVarWith,
                                                              registerLVar,
                                                              registerStringLiteral)
import           Htcc.Parser.Combinators.Var                 (varInit)
import           Htcc.Parser.ConstructionData.Core           (fallBack,
                                                              incomplete,
                                                              isSwitchStmt,
                                                              lookupFunction,
                                                              lookupVar,
                                                              pushWarn,
                                                              resetLocal,
                                                              succNest)
import           Htcc.Parser.ConstructionData.Scope          (LookupVarResult (..))
import qualified Htcc.Parser.ConstructionData.Scope.Function as PSF
import qualified Htcc.Parser.ConstructionData.Scope.Var      as PV
import qualified Text.Megaparsec                             as M
import qualified Text.Megaparsec.Char                        as MC

import           Text.Megaparsec.Debug                       (dbg)

parser, program :: (Integral i, Bits i, Read i, Show i) => Parser i (ASTs i)
parser = spaceConsumer *> program <* M.eof
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
    cast,
    unary,
    factor :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)

global = choice
    [ M.try function
    , gvar
    ]

function = do
    pos <- getPosState
    declspec >>= declarator >>= \case
        (_, Nothing) -> fail "function name omitted, expected unqualified-id"
        (ty@(CT.SCAuto (CT.CTFunc _ _)), Just ident) -> modify resetLocal
            *> choice
                [ declaration ty ident
                , definition ty ident pos
                ]
        _ -> fail "expected function" -- TODO: currentry, ignore storage class
    where
        declaration ty ident = ATEmpty <$ (semi *> registerFunc False ty ident)

        definition ty ident pos = do
            registerFunc True ty ident
            params <- mapM (uncurry registerLVar) =<< toNamedParams ty
            stmt >>= fromValidFunc params
            where
                fromValidFunc params' st@(ATNode (ATBlock block) _ _ _)
                    | CT.toTypeKind ty == CT.CTVoid =
                        if isJust (find isNonEmptyReturn block) then
                            fail $ mconcat
                                [ "the return type of function '"
                                , T.unpack ident
                                , "' is void, but the statement returns a value"
                                ]
                        else
                            pure $ atDefFunc ident (if null params' then Nothing else Just params') ty st
                    | otherwise = do
                        when (isJust (find isEmptyReturn block)) $
                            pushWarn pos $ mconcat
                                [ "the return type of function '"
                                , T.unpack ident
                                , "' is "
                                , show (CT.toTypeKind ty)
                                , ", but the statement returns no value"
                                ]
                        pure $ atDefFunc ident (if null params' then Nothing else Just params') ty st
                fromValidFunc _ _ = fail "internal compiler error"

gvar = do
    ty <- declspec
    M.choice
        [ ATEmpty <$ semi
        , declGVar ty
        ]
    where
        declGVar ty = declarator ty >>= \case
            (_, Nothing) -> fail "variable name omitted, expected unqualified-id"
            (ty', Just ident) -> choice
                [ nonInit ty' ident
                , withInit ty' ident
                ]

        nonInit ty ident = semi
            >> gets (incomplete ty)
            >>= maybeToParser "defining global variables with a incomplete type"
            >>= flip registerGVar ident
            >> pure ATEmpty

        withInit ty ident = do
            void equal
            ty' <- maybeToParser "defining global variables with a incomplete type" =<< gets (incomplete ty)
            gvarInit ty' ident <* semi

        gvarInit ty ident = choice
            [ M.try fromConstant
            , fromOG
            ]
            where
                fromConstant = evalConstexpr
                    >>= registerGVarWith ty ident . PV.GVarInitWithVal

                fromOG = do
                    ast <- conditional
                    case (atkind ast, atkind (atL ast)) of
                        (ATAddr, ATGVar _ name) -> registerGVarWith ty ident (PV.GVarInitWithOG name)
                        (ATAddr, _) -> fail "invalid initializer in global variable"
                        (ATGVar t name, _)
                            | CT.isCTArray t -> registerGVarWith ty ident (PV.GVarInitWithOG name)
                            -- TODO: support initializing from other global variables
                            | otherwise -> fail "initializer element is not constant"
                        _ -> fail "initializer element is not constant"

compoundStmt :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i [ATree i]
compoundStmt = bracket get (modify . fallBack) $ const $
    braces (modify succNest *> M.many stmt)

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
            [ atReturn (CT.SCUndef CT.CTUndef) ATEmpty <$ M.try (kReturn *> semi)
            , atReturn (CT.SCUndef CT.CTUndef) <$> (M.try kReturn *> expr) <* semi
            ]

        ifStmt = do
            r <- atIf <$> (M.try kIf >> parens expr) <*> stmt
            M.option ATEmpty (M.try kElse >> stmt) <&> \case
                ATEmpty -> r
                nd -> atElse r nd

        whileStmt = atWhile <$> (M.try kWhile >> parens expr) <*> stmt

        forStmt = (>>) (M.try kFor) $ bracket get (modify . fallBack) $ const $ do
            es <- parens $ do
                modify succNest
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
                putSwitchState b = modify $ \scp -> scp { isSwitchStmt = b }

        caseStmt = M.try kCase
            *> ifM (gets isSwitchStmt)
                ((atCase 0 <$> evalConstexpr <* colon) <*> stmt)
                (fail "stray 'case'")

        defaultStmt = (M.try kDefault <* colon)
            *> ifM (gets isSwitchStmt)
                (atDefault 0 <$> stmt)
                (fail "stray 'default'")

        gotoStmt = atGoto <$> (M.try kGoto *> identifier <* semi)

        labelStmt = atLabel <$> M.try (identifier <* colon)

        lvarStmt = do
            ty <- M.try declspec
            M.choice
                [ ATEmpty <$ semi
                , declLVar ty
                ]
            where
                declLVar ty = declarator ty >>= \case
                    (_, Nothing) -> fail "variable name omitted, expected unqualified-id"
                    (ty', Just ident) -> M.choice
                        [ nonInit ty' ident
                        , withInit ty' ident
                        ]

                nonInit ty ident = semi *> registerLVar ty ident <&> atNull
                withInit ty ident = equal *> varInit assign ty ident <* semi

expr = assign

assign = do
    nd <- conditional
    M.option nd $ choice $ map (`id` nd)
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
    ifM (M.option False (True <$ M.lookAhead question)) (GNU.condOmitted nd M.<|> condOp nd) $ pure nd
    where
        condOp nd = uncurry (`atConditional` nd) . first atype . dupe
            <$> (question *> expr <* colon)
            <*> conditional

logicalOr = binaryOperator logicalAnd [(symbol "||", binOpBool ATLOr)]

logicalAnd = binaryOperator bitwiseOr [(symbol "&&", binOpBool ATLAnd)]

bitwiseOr = binaryOperator bitwiseXor [(vertical, binOpIntOnly ATOr)]

bitwiseXor = binaryOperator bitwiseAnd [(hat, binOpIntOnly ATXor)]

bitwiseAnd = binaryOperator equality [(MC.char '&' `notFollowedOp` MC.char '&', binOpIntOnly ATAnd)]

equality = binaryOperator relational
    [ (symbol "==", binOpBool ATEQ)
    , (symbol "!=", binOpBool ATNEQ)
    ]

relational = binaryOperator shift
    [ (symbol "<=", binOpBool ATLEQ)
    , (langle, binOpBool ATLT)
    , (symbol ">=", binOpBool ATGEQ)
    , (rangle, binOpBool ATGT)
    ]

shift = binaryOperator add
    [ (symbol "<<", binOpIntOnly ATShl)
    , (symbol ">>", binOpIntOnly ATShr)
    ]

add = binaryOperator term
    [ (symbol "+", \l r -> maybeToParser "invalid operands" $ addKind l r)
    , (symbol "-", \l r -> maybeToParser "invalid operands" $ subKind l r)
    ]

term = binaryOperator cast
    [ (star, binOpCon ATMul)
    , (slash, binOpCon ATDiv)
    , (percent, binOpCon ATMod)
    ]

cast = choice
    [ atCast <$> M.try (parens absDeclarator) <*> cast
    , unary
    ]

unary = choice
    [ symbol "++" *> unary <&> \n -> ATNode ATPreInc (atype n) n ATEmpty
    , symbol "--" *> unary <&> \n -> ATNode ATPreDec (atype n) n ATEmpty
    , symbol "+" *> unary
    , symbol "-" *> unary <&> \n -> ATNode ATSub (atype n) (atNumLit 0) n
    , lnot *> unary <&> flip (ATNode ATNot (CT.SCAuto CT.CTBool)) ATEmpty
    , tilda *> unary <&> flip (ATNode ATBitNot (CT.SCAuto CT.CTInt)) ATEmpty
    , addr
    , star *> unary >>= deref'
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
                    ty' <- maybeToParser "incomplete value dereference" =<< gets (incomplete ty)
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
    , alignof
    , strLiteral
    , identifier'
    , M.try (parens expr)
    , GNU.stmtExpr
    , ATEmpty <$ M.eof
    ]
    where
        memOp p op opS = p *> choice
            [ memOpType
            , memOpUnary
            ]
            where
                memOpType = incomplete <$> M.try (parens absDeclarator) <*> get
                    >>= fmap (atNumLit . fromIntegral . op)
                    . maybeToParser ("invalid application of '" <> opS <> "' to incomplete type")

                memOpUnary = do
                    u <- unary
                    if CT.isCTUndef (atype u) then
                        fail $ opS <> " must be an expression or type"
                    else
                        pure $ atNumLit $ fromIntegral $ op $ atype u

        sizeof = memOp kSizeof CT.sizeof "sizeof"
        alignof = memOp k_Alignof CT.alignof "alignof"

        strLiteral = stringLiteral >>= registerStringLiteral

        identifier' = do
            pos <- getPosState
            ident <- identifier
            choice
                [ fnCall ident pos
                , variable ident
                ]
            where
                variable ident =
                    gets (lookupVar ident)
                        >>= \case
                            FoundGVar (PV.GVar t _) -> return $ atGVar t ident
                            FoundLVar sct -> return $ treealize sct
                            FoundEnum sct -> return $ treealize sct
                            FoundFunc sct -> return $ treealize sct
                            NotFound -> fail $ "The '" <> T.unpack ident <> "' is not defined identifier"

                fnCall ident pos = do
                    params <- lparen *> M.manyTill (M.try (expr <* comma) M.<|> expr) rparen
                    let params' = if null params then Nothing else Just params
                    gets (lookupFunction ident) >>= \case
                        -- TODO: set warning message
                        -- TODO: Infer the return type of a function
                        Nothing -> atNoLeaf (ATCallFunc ident params') (CT.SCAuto CT.CTInt)
                            <$ pushWarn pos ("the function '" <> T.unpack ident <> "' is not declared.")
                        Just fn -> pure $ atNoLeaf (ATCallFunc ident params') (PSF.fntype fn)
