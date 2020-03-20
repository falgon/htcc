{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase, BangPatterns, ScopedTypeVariables #-}
{-|
Module      : Htcc.Parser.Parsing.Core
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The C languge parser and AST constructor
-}
module Htcc.Parser.Parsing.Core (
    -- * Recursive descent implementation functions
    program,
    globalDef,
    stmt,
    inners,
    logicalOr,
    logicalAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseAnd,
    shift,
    add,
    term,
    cast,
    unary,
    factor,
    relational,
    equality,
    conditional,
    assign,
    expr,
    -- * Parser
    parse,
    -- * Types and synonyms
    ASTs,
    ASTSuccess,
    ASTConstruction,
    ASTResult,
    -- * Utilities
    stackSize
) where

import Prelude hiding (toInteger)
import Data.Bits hiding (shift)
import qualified Data.ByteString as B
import Data.Foldable (Foldable (..))
import Data.Tuple.Extra (first, second, uncurry3, snd3, dupe)
import Data.List (find, foldl')
import Data.List.Split (linesBy)
import Data.Either (isLeft, lefts, rights)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (forM, when)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)
import Numeric.Natural

import Htcc.Utils (
    first3, second3, third3, first4,
    tshow, toNatural, toInteger,
    maybeToRight, maybe')
import qualified Htcc.Tokenizer as HT
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Parser.ConstructionData.Scope.Var as PV
import Htcc.Parser.AST
import qualified Htcc.Parser.ConstructionData.Scope.Function as PSF
import Htcc.Parser.ConstructionData.Scope (Scoped (..), LookupVarResult (..))
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import Htcc.Parser.ConstructionData
import Htcc.Parser.Utils
import Htcc.Parser.Parsing.Type
import Htcc.Parser.Parsing.Typedef

{-# INLINE varDecl #-}
varDecl :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> ASTConstruction i
varDecl tk scp = takeType tk scp >>= validDecl (HT.altEmptyToken tk) >>= varDecl'
    where
        varDecl' (_, Nothing, (_, HT.TKReserved ";"):ds, scp') = Right (ds, ATEmpty, scp')
        varDecl' (t, Just ident, (_, HT.TKReserved ";"):ds, scp') = maybeToRight ("declaration with incomplete type", ident) (incomplete t scp) >>= \t' ->
            addLVar t' ident scp' >>= \(lat, scp'') -> Right (ds, atNull lat, scp'')
        varDecl' (t, Just ident, (_, HT.TKReserved "="):ds, scp') = (>>=) (varInit assign t ident ds scp') $ \case
            ((_, HT.TKReserved ";"):ds', at, sc) -> Right (ds', at, sc)
            _ -> Left ("expected ';' token, the subject iteration statement starts here:", head tk)
        varDecl' (_, _, ds, _) = Left $ if null ds then ("expected unqualified-id", head tk) else ("expected unqualified-id before '" <> tshow (snd (head ds)) <> T.singleton '\'', head ds)
        validDecl _ tnt@(t, Just ident, _, scp') = maybe' (Right tnt) (incomplete t scp') $ \t' -> if CT.toTypeKind t == CT.CTVoid then 
            Left ("variable or field '" <> tshow (snd ident) <> "' declarated void", ident) else Right $ first4 (const t') tnt
        validDecl errPlaceholder tnt@(t, _, _, scp') = maybe' (Right tnt) (incomplete t scp') $ \t' -> if CT.toTypeKind t == CT.CTVoid then
            Left ("declarations of type void is invalid in this context", errPlaceholder) else Right $ first4 (const t') tnt

gvarInit :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> CT.StorageClass i -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) ([HT.TokenLC i], ConstructionData i)
gvarInit xs ty ident sc = do
    (ds, ast, sc') <- conditional xs ATEmpty sc
    case (atkind ast, atkind (atL ast)) of
        (ATAddr, ATGVar _ name) -> (ds,) . snd <$> gvarInitWithOG ty ident name sc'
        (ATAddr, _) -> Left ("invalid initializer in global variable", HT.altEmptyToken ds)
        (ATGVar t name, _) 
            | CT.isCTArray t -> (ds,) . snd <$> gvarInitWithOG ty ident name sc'
            | otherwise -> gvarInitWithVal ds sc'
        _ -> gvarInitWithVal ds sc'
    where
        gvarInitWithOG ty' from to = addGVarWith ty' from (PV.GVarInitWithOG to)
        gvarInitWithVal ds sc' = do
            (ds', cval) <- either (maybe (Left ("initializer element is not constant", HT.altEmptyToken ds)) Left) Right $ constantExp xs sc'
            (ds',) . snd <$> addGVarWith ty ident (PV.GVarInitWithVal cval) sc'

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (ASTs i, ConstructionData i)
program [] !scp = Right ([], scp)
program xs !scp = either Left (\(ys, atn, !scp') -> first (atn:) <$> program ys scp') $ globalDef xs ATEmpty scp

-- | `globalDef` parses global definitions (include functions and global variables)
globalDef :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
globalDef (cur@(_, HT.TKReserved "register"):_) _ _ = Left ("illegal storage class on file-scoped identifier", cur)
globalDef (cur@(_, HT.TKReserved "auto"):_) _ _ = Left ("illegal storage class on file-scoped identifier", cur)
globalDef xs@((_, HT.TKTypedef):_) _ sc = typedef xs sc -- for global @typedef@
globalDef tks at !va = (>>=) (takeType tks va) $ \case
    (_, Nothing, (_, HT.TKReserved ";"):ds', scp) -> Right (ds', ATEmpty, scp) -- e.g., @int;@ is legal in C11 (See N1570/section 6.7 Declarations)
    (funcType, Just cur@(_, HT.TKIdent fname), tk@((_, HT.TKReserved "("):_), !sc) -> let scp = resetLocal sc in -- for a function declaration or definition
        (>>=) (maybeToRight (internalCE, cur) (takeBrace "(" ")" $ tail (cur:tk))) $
            either (Left . ("invalid function declaration/definition",)) $ \(fndec, st) -> case st of
                ((_, HT.TKReserved ";"):ds'') -> addFunction False funcType cur scp >>= globalDef ds'' at -- for a function declaration -- TODO: read types of parameters and register them
                ((_, HT.TKReserved "{"):_) -> (>>=) (addFunction True funcType cur scp) $ \scp' -> checkErr fndec scp' $ \args -> runST $ do -- for a function definition
                    eri <- newSTRef Nothing
                    v <- newSTRef scp'
                    mk <- flip unfoldrM args $ \args' -> if null args' then return Nothing else let arg = head args' in do
                        -- About @t'@:
                        -- An array of type T is equivalent to a pointer of type T in the context of function parameters.
                        m <- flip fmap (readSTRef v) $ \scp'' -> let (t, mident, _, _) = arg; t' = fromMaybe t $ aboutArray t in case mident of
                            Nothing -> Left ("anonymouse variable is not implemented yet", cur) -- TODO
                            Just ident -> addLVar t' ident scp''
                        flip (either ((<$) Nothing . writeSTRef eri . Just)) m $ \(vat, scp'') -> Just (vat, tail args') <$ writeSTRef v scp''
                    (>>=) (readSTRef eri) $ flip maybe (return . Left) $ flip fmap (readSTRef v) $ \v' -> (>>=) (stmt st at v') $ \case -- Forbid void to return a value in a return type function.
                        (ert, erat@(ATNode (ATBlock block) _ _ _), erscp) 
                            | CT.toTypeKind funcType == CT.CTVoid -> if isJust (find isNonEmptyReturn block) then
                                Left ("The return type of function '" <> fname <> "' is void, but the statement returns a value", cur) else
                                    Right (ert, atDefFunc fname (if null mk then Nothing else Just mk) funcType erat, erscp)
                            | otherwise -> let fnode = atDefFunc fname (if null mk then Nothing else Just mk) funcType erat in
                                maybe' (Right (ert, fnode, erscp)) (find isEmptyReturn block) $ const $ 
                                    Right (ert, fnode, pushWarn ("The return type of function '" <> fname <> "' is " <> tshow (CT.toTypeKind funcType) <> ", but the statement returns no value") cur erscp)
                        _ -> Left (internalCE, HT.emptyToken)
                _ -> stmt tk at scp
    (ty, Just cur@(_, HT.TKIdent _), xs, !scp) -> case xs of -- for global variables
        (_, HT.TKReserved "="):ds -> do -- for initializing
            ty' <- maybeToRight ("defining global variables with a incomplete type", cur) (incomplete ty scp)
            (ds', nsc) <- gvarInit ds ty' cur scp
            case ds' of
                (_, HT.TKReserved ";"):ds'' -> return (ds'', ATEmpty, nsc)
                _ -> Left $ if null ds' then 
                    ("expected ';' token after '" <> tshow (snd cur) <> "' token", HT.altEmptyToken ds') else
                        ("expected ';' token" <> (if null ds' then "" else " before '" <> tshow (snd $ head ds') <> "' token"), HT.altEmptyToken ds')
        (_, HT.TKReserved ";"):ds -> do -- for non initializing
            ty' <- maybeToRight ("defining global variables with a incomplete type", cur) (incomplete ty scp)
            (ds, ATEmpty,) . snd <$> addGVar ty' cur scp
        _ -> Left ("expected ';' token after '" <> tshow (snd cur) <> "' token", cur)
    _ -> Left ("invalid definition of global identifier", HT.altEmptyToken tks)
    where
        checkErr ar !scp' f = let ar' = init $ tail ar in if not (null ar') && snd (head ar') == HT.TKReserved "," then Left ("unexpected ',' token", head ar') else
            let args = linesBy ((==HT.TKReserved ",") . snd) ar' in mapM (`takeType` scp') args >>= f
        aboutArray t
            | CT.isCTArray t = CT.mapTypeKind CT.CTPtr <$> CT.deref t
            | CT.isIncompleteArray t = Just $ CT.mapTypeKind (\(CT.CTIncomplete (CT.IncompleteArray t')) -> CT.CTPtr t') t
            | otherwise = Nothing

-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
stmt ((_, HT.TKReturn):(_, HT.TKReserved ";"):xs) _ scp = Right (xs, atReturn (CT.SCUndef CT.CTUndef) ATEmpty, scp) -- for @return;@
stmt (cur@(_, HT.TKReturn):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @return@
    (_, HT.TKReserved ";"):ys -> Right (ys, atReturn (CT.SCUndef CT.CTUndef) erat, erscp)
    ert' -> Left $ expectedMessage ";" cur ert'
stmt (cur@(_, HT.TKIf):(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @if@
    (_, HT.TKReserved ")"):ys -> (>>=) (stmt ys erat erscp) $ \x -> case second3 (atIf erat) x of
        ((_, HT.TKElse):zs, eerat, eerscp) -> second3 (atElse eerat) <$> stmt zs eerat eerscp -- for @else@
        zs -> Right zs
    ert' -> Left $ expectedMessage ")" cur ert'
stmt (cur@(_, HT.TKWhile):(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @while@
    (_, HT.TKReserved ")"):ys -> second3 (atWhile erat) <$> stmt ys erat erscp
    ert' -> Left $ expectedMessage ")" cur ert'
stmt xxs@(cur@(_, HT.TKFor):(_, HT.TKReserved "("):_) _ !scp = (>>=) (maybeToRight (internalCE, cur) (takeBrace "(" ")" (tail xxs))) $ -- for @for@
    either (Left . ("expected ')' token. The subject iteration statement starts here:",)) $ \(forSt, ds) -> (>>=) (initSect (tail (init forSt)) $ succNest scp) $ \(fxs, finit, fscp') ->
        (>>=) (condSect fxs fscp') $ \(fxs', fcond, fscp'') -> (>>=) (incrSect fxs' fscp'') $ \case
            ([], fincr, fscp''') -> 
                let fnd = filter (\x' -> case fromATKindFor x' of ATEmpty -> False; x'' -> not $ isEmptyExprStmt x'') [ATForInit finit, ATForCond fcond, ATForIncr fincr]
                    mkk = maybe (ATForCond (atNumLit 1) : fnd) (const fnd) $ find isATForCond fnd in case ds of
                        ((_, HT.TKReserved ";"):ys) -> Right (ys, atFor mkk, fallBack scp fscp''')
                        _ -> third3 (fallBack scp) . second3 (atFor . (mkk ++) . (:[]) . ATForStmt) <$> stmt ds ATEmpty fscp'''
            _ -> Left ("unexpected end of for statement", cur)
    where
        initSect [] _ = Left ("the iteration statement for must be `for (expression_opt; expression_opt; expression_opt) statement`. See section 6.8.5.", cur)
        initSect ((_, HT.TKReserved ";"):ds) fsc = Right (ds, ATEmpty, fsc)
        initSect forSect fsc
            | isTypeName (head forSect) fsc = varDecl forSect fsc
            | otherwise = (>>=) (expr forSect ATEmpty fsc) $ \(x, y, z) -> case x of
                (_, HT.TKReserved ";"):ds -> Right (ds, atExprStmt y, z)
                _ -> if null x then Left ("expected ';' token", HT.emptyToken) else Left ("expected ';' token after '" <> tshow (snd $ head x) <> "'", head x)
        condSect [] _ = Left ("the iteration statement for must be `for (expression_opt; expression_opt; expression_opt) statement`. See section 6.8.5.", cur)
        condSect ((_, HT.TKReserved ";"):ds) fsc = Right (ds, ATEmpty, fsc)
        condSect forSect fsc = (>>=) (expr forSect ATEmpty fsc) $ \case
            ((_, HT.TKReserved ";"):ds, y, z) -> Right (ds, y, z)
            (x, _, _) -> if null x then Left ("expected ';' token", HT.emptyToken) else Left ("expected ';' token after '" <> tshow (snd $ head x) <> "'", head x)
        incrSect [] fsc = Right ([], ATEmpty, fsc)
        incrSect forSect fsc = second3 atExprStmt <$> expr forSect ATEmpty fsc
stmt xxs@(cur@(_, HT.TKReserved "{"):_) _ !scp = (>>=) (maybeToRight (internalCE, cur) (takeBrace "{" "}" xxs)) $ -- for compound statement
    either (Left . ("the compound statement is not closed",)) $ \(sctk, ds) -> runST $ do
        eri <- newSTRef Nothing
        v <- newSTRef $ succNest scp
        mk <- flip unfoldrM (init $ tail sctk) $ \ert -> if null ert then return Nothing else do
            erscp <- readSTRef v
            either (\err -> Nothing <$ writeSTRef eri (Just err)) (\(ert', erat', erscp') -> Just (erat', ert') <$ writeSTRef v erscp') $ stmt ert ATEmpty erscp
        (>>=) (readSTRef eri) $ flip maybe (return . Left) $ Right . (ds, atBlock mk,) . fallBack scp <$> readSTRef v
stmt ((_, HT.TKReserved ";"):xs) atn !scp = Right (xs, atn, scp) -- for only @;@
stmt (cur@(_, HT.TKBreak):xs) _ scp = case xs of -- for @break@
    (_, HT.TKReserved ";"):ds -> Right (ds, atBreak, scp)
    _ -> Left ("expected ';' token after 'break' token", cur)
stmt (cur@(_, HT.TKContinue):xs) _ scp = case xs of -- for @continue@
    (_, HT.TKReserved ";"):ds -> Right (ds, atContinue, scp)
    _ -> Left ("expected ';' token after 'continue' token", cur)
stmt (cur@(_, HT.TKSwitch):xs) atn scp = case xs of -- for @switch@
    (_, HT.TKReserved "("):xs' -> (>>=) (expr xs' atn scp) $ \case
        (cur1@(_, HT.TKReserved ")"):xs'', cond, scp') ->
            (>>=) (stmt xs'' ATEmpty (scp' { isSwitchStmt = True })) $ \case
                (xs''', ATNode (ATBlock ats) t _ _, scp'') -> Right (xs''', atSwitch cond ats t, scp'' { isSwitchStmt = False })
                _ -> Left ("expected compound statement after the token ')'", cur1)
        (xs'', _, _) -> Left $ if not (null xs'') then ("expected token ')' before '" <> tshow (snd $ head xs') <> "' token", head xs') else ("expected ')' token", HT.emptyToken)
    _ -> Left ("expected token '(' after the token 'switch'", cur)
stmt (cur@(_, HT.TKCase):xs) atn scp -- for @case@
    | isSwitchStmt scp = flip (either (Left . fromMaybe ("expected constant expression after 'case' token", cur))) (constantExp xs scp) $ \case
        ((_, HT.TKReserved ":"):ds, val) -> second3 (atCase 0 val) <$> stmt ds atn scp
        (ds, _) -> Left $ if not (null ds) then ("expected ':' token before '" <>  tshow (snd $ head ds) <> "'", head ds) else ("expected ':' token", head ds)
    | otherwise = Left ("stray 'case'", cur)
stmt (cur@(_, HT.TKDefault):(_, HT.TKReserved ":"):xs) atn scp -- for @default@
    | isSwitchStmt scp = second3 (atDefault 0) <$> stmt xs atn scp
    | otherwise = Left ("stray 'default'", cur)
stmt (cur@(_, HT.TKGoto):xs) _ scp = case xs of -- for @goto@
    (_, HT.TKIdent ident):(_, HT.TKReserved ";"):ds -> Right (ds, atGoto ident, scp)
    (_, HT.TKIdent ident):_ -> Left ("expected ';' token after the identifier '" <> ident <> "'", cur)  
    _ -> Left ("expected identifier after the 'goto' token", cur)
stmt ((_, HT.TKIdent ident):(_, HT.TKReserved ":"):xs) _ scp = Right (xs, atLabel ident, scp) -- for local label
stmt xs@((_, HT.TKTypedef):_) _ scp = typedef xs scp -- for local @typedef@
stmt tk atn !scp
    | not (null tk) && isTypeName (head tk) scp = varDecl tk scp -- for a local variable declaration
    | otherwise = (>>=) (expr tk atn scp) $ \(ert, erat, erscp) -> case ert of -- for stmt;
        (_, HT.TKReserved ";"):ys -> Right (ys, atExprStmt erat, erscp)
        ert' -> Left $ expectedMessage ";" (if null tk then HT.emptyToken else last tk) ert'

{-# INLINE expr #-}
-- | \({\rm expr} = {\rm assign}\left("," {\rm assign}\right)\ast\)
expr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
expr tk at cd = assign tk at cd >>= uncurry3 f
    where
        f ((_, HT.TKReserved ","):xs) at' cd' = assign xs at' cd' >>= uncurry3 f . second3 (\x -> atComma (atype x) (atExprStmt at') x)
        f tk' at' cd' =  Right (tk', at', cd')

-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
assign xs atn scp = (>>=) (conditional xs atn scp) $ \(ert, erat, erscp) -> case ert of
    (_, HT.TKReserved "="):ys -> nextNode ATAssign ys erat erscp
    (_, HT.TKReserved "*="):ys -> nextNode ATMulAssign ys erat erscp
    (_, HT.TKReserved "/="):ys -> nextNode ATDivAssign ys erat erscp
    (_, HT.TKReserved "&="):ys -> nextNode ATAndAssign ys erat erscp
    (_, HT.TKReserved "|="):ys -> nextNode ATOrAssign ys erat erscp
    (_, HT.TKReserved "^="):ys -> nextNode ATXorAssign ys erat erscp
    (_, HT.TKReserved "<<="):ys -> nextNode ATShlAssign ys erat erscp
    (_, HT.TKReserved ">>="):ys -> nextNode ATShrAssign ys erat erscp
    (_, HT.TKReserved "+="):ys -> nextNode (maybe ATAddAssign (const ATAddPtrAssign) $ CT.deref (atype erat)) ys erat erscp
    (_, HT.TKReserved "-="):ys -> nextNode (maybe ATSubAssign (const ATSubPtrAssign) $ CT.deref (atype erat)) ys erat erscp
    _ -> Right (ert, erat, erscp)
    where
        nextNode atk ys erat erscp = (>>=) (assign ys erat erscp) $ \(zs, erat', erscp') -> 
            (>>=) (validAssign  (if not (null zs) then head zs else if not (null ys) then head ys else if not (null xs) then head xs else HT.emptyToken) erat') $ \erat'' -> 
                Right (zs, ATNode atk (atype erat) erat erat'', erscp')

-- | `conditional` indicates \(\eqref{eq:seventeenth}\) among the comments of `inners`.
conditional :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
conditional xs atn scp = (>>=) (logicalOr xs atn scp) $ \(ert, cond, erscp) -> case ert of
    -- GNU extension (Conditionals with Omitted Operands, see also: https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html)
    (_, HT.TKReserved "?"):(_, HT.TKReserved ":"):ds -> second3 (atConditional (atype cond) cond ATEmpty) <$> conditional ds cond erscp 
    cur@(_, HT.TKReserved "?"):ds -> (>>=) (expr ds cond erscp) $ \(ert', thn, erscp') -> case ert' of
        (_, HT.TKReserved ":"):ds' -> second3 (atConditional (atype thn) cond thn) <$> conditional ds' thn erscp'
        ds' -> if null ds' then Left ("expected ':' token for this '?'", cur) else Left ("expected ':' before '" <> tshow (snd (head ds')) <> "' token", head ds')
    _ -> Right (ert, cond, erscp)

-- | `inners` is a general function for creating `equality`, `relational`, `add` and `term` in the following syntax (EBNF) of \({\rm LL}\left(k\right)\) where \(k\in\mathbb{N}\).
--
-- \[
-- \begin{eqnarray}
-- {\rm program} &=& {\rm stmt}^\ast\label{eq:eigth}\tag{1}\\
-- {\rm stmt} &=& \begin{array}{l}
-- {\rm expr}?\ {\rm ";"}\\ 
-- \mid\ {\rm "\{"\ stmt}^\ast\ {\rm "\}"}\\
-- \mid\ {\rm "return"}\ {\rm expr}\ ";"\\
-- \mid\ "{\rm if}"\ "("\ {\rm expr}\ ")"\ {\rm stmt}\ ("{\rm else}"\ {\rm stmt})?\\
-- \mid\ {\rm "while"\ "("\ expr\ ")"\ stmt}\\
-- \mid\ {\rm "for"\ "("\ expr?\ ";" expr?\ ";"\ expr?\ ")"\ stmt? ";"}
-- \end{array}\label{eq:nineth}\tag{2}\\
-- {\rm expr} &=& {\rm assign}\\
-- {\rm assign} &=& {\rm conditional} \left(\left("="\ \mid\ "+="\ \mid\ "-="\ \mid\ "*="\ \mid\ "/="\right)\ {\rm assign}\right)?\label{eq:seventh}\tag{3}\\
-- {\rm conditional} &=& {\rm logicalOr} \left("?"\ {\rm expr}\ ":"\ {\rm conditional}\right)?\label{eq:seventeenth}\tag{4}\\
-- {\rm logicalOr} &=& {\rm logicalAnd}\ \left("||"\ {\rm logicalAnd}\right)^\ast\label{eq:fifteenth}\tag{5}\\
-- {\rm logicalAnd} &=& {\rm bitwiseOr}\ \left("|"\ {\rm bitwiseOr}\right)^\ast\label{eq:sixteenth}\tag{6}\\
-- {\rm bitwiseOr} &=& {\rm bitwiseXor}\ \left("|"\ {\rm bitwiseXor}\right)^\ast\label{eq:tenth}\tag{7}\\
-- {\rm bitwiseXor} &=& {\rm bitwiseAnd}\ \left("\hat{}"\ {\rm bitwiseAnd}\right)^\ast\label{eq:eleventh}\tag{8}\\
-- {\rm bitwiseAnd} &=& {\rm equality}\ \left("\&"\ {\rm equality}\right)^\ast\label{eq:twelveth}\tag{9}\\
-- {\rm equality} &=& {\rm relational}\ \left("=="\ {\rm relational}\ \mid\ "!="\ {\rm relational}\right)^\ast\label{eq:fifth}\tag{10}\\
-- {\rm relational} &=& {\rm shift}\ \left("\lt"\ {\rm shift}\mid\ "\lt ="\ {\rm shift}\mid\ "\gt"\ {\rm shift}\mid\ "\gt ="\ {\rm shift}\right)^\ast\label{eq:sixth}\tag{11}\\
-- {\rm shift} &=& {\rm add}\ \left("\lt\lt"\ {\rm add}\mid\ "\gt\gt"\ {\rm add}\right)^\ast\label{eq:thirteenth}\tag{12}\\
-- {\rm add} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{13} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{14} \\
-- {\rm cast} &=& "(" {\rm type-name} ")"\ {\rm cast}\ \mid\ {\rm unary}\label{eq:fourteenth}\tag{15} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm cast}\mid\ \left("!"\ \mid\ "\sim"\ \mid\ "\&"\ \mid\ "\ast"\right)?\ {\rm unary}\label{eq:fourth}\tag{16} \\
-- {\rm factor} &=& {\rm num} \mid\ {\rm ident}\ \left({\rm "(" \left(expr\ \left(\left(","\ expr\right)^\ast\right)?\right)? ")"}\right)?\ \mid\ "(" {\rm expr} ")"\label{eq:third}\tag{17}
-- \end{eqnarray}
-- \]
inners :: ([HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i) -> [(T.Text, ATKind i)] -> [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
inners _ _ [] atn scp = Right ([], atn, scp)
inners f cs xs atn scp = either Left (uncurry3 (inners' f cs)) $ f xs atn scp
    where
        inners' _ _ [] at ars = Right ([], at, ars)
        inners' g ds ys at ars = maybe' (Right (ys, at, ars)) (find (\(c, _) -> case snd (head ys) of HT.TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            either Left (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k (CT.SCAuto CT.CTInt) at)) $ g (tail ys) at ars

-- | `logicalOr` indicates \(\eqref{eq:fifteenth}\) among the comments of `inners`.
logicalOr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
logicalOr = inners logicalAnd [("||", ATLOr)]

-- | `logicalAnd` indicates \(\eqref{eq:sixteenth}\) among the comments of `inners`.
logicalAnd :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
logicalAnd = inners bitwiseOr [("&&", ATLAnd)]

-- | `bitwiseOr` indicates \(\eqref{eq:tenth}\) among the comments of `inners`.
bitwiseOr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
bitwiseOr = inners bitwiseXor [("|", ATOr)]

-- | `bitwiseXor` indicates \(\eqref{eq:eleventh}\) amont the comments of `inners`.
bitwiseXor :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
bitwiseXor = inners bitwiseAnd [("^", ATXor)]

-- | `bitwiseAnd` indicates \(\eqref{eq:twelveth}\) among the comments of `inners`.
bitwiseAnd :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
bitwiseAnd = inners equality [("&", ATAnd)]

-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [HT.TokenLC i] -> ATree i -> [LVar i] -> Either (ASTError i) ([HT.TokenLC i], ATree i)
-- > equality xs atn scp = (>>=) (relational xs atn scp) $ uncurry3 equality'
-- >     where
-- >         equality' ((_, HT.TKReserved "=="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' ((_, HT.TKReserved "!="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
-- >         equality' ert era ars = Right (ert, era, ars)
equality :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
relational = inners shift [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `shift` indicates \(\eqref{eq:thirteenth}\\) among the comments of `inners`.
shift :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
shift = inners add [("<<", ATShl), (">>", ATShr)]
        
-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
add :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
add xs atn scp = (>>=) (term xs atn scp) $ uncurry3 add'
    where
        add' (cur@(_, HT.TKReserved "+"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            maybeToRight ("invalid operands", cur) (addKind era $ snd3 zz) >>= \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' (cur@(_, HT.TKReserved "-"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            maybeToRight ("invalid operands", cur) (subKind era $ snd3 zz) >>= \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' ert erat ars = Right (ert, erat, ars)

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
term = inners cast [("*", ATMul), ("/", ATDiv), ("%", ATMod)]

-- | `cast` indicates \(\eqref{eq:fourteenth}\) amont the comments of `inners`.
cast :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
cast (cur@(_, HT.TKReserved "("):xs) at scp = flip (either (const $ unary (cur:xs) at scp)) (takeTypeName xs scp) $ \case
    (t, (_, HT.TKReserved ")"):xs') -> second3 (atCast t) <$> cast xs' at scp
    _ -> Left ("The token ')' corresponding to '(' is expected", cur)
cast xs at scp = unary xs at scp

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
unary ((_, HT.TKReserved "+"):xs) at scp = cast xs at scp
unary ((_, HT.TKReserved "-"):xs) at scp = second3 (ATNode ATSub (CT.SCAuto CT.CTInt) (atNumLit 0)) <$> cast xs at scp
unary ((_, HT.TKReserved "!"):xs) at scp = second3 (flip (ATNode ATNot $ CT.SCAuto CT.CTInt) ATEmpty) <$> cast xs at scp
unary ((_, HT.TKReserved "~"):xs) at scp = second3 (flip (ATNode ATBitNot $ CT.SCAuto CT.CTInt) ATEmpty) <$> cast xs at scp
unary ((_, HT.TKReserved "&"):xs) at scp = flip fmap (cast xs at scp) $ second3 $ \x -> let ty = if CT.isCTArray (atype x) then fromJust $ CT.deref (atype x) else atype x in 
    atUnary ATAddr (CT.mapTypeKind CT.CTPtr ty) x
unary (cur@(_, HT.TKReserved "*"):xs) at !scp = (>>=) (cast xs at scp) $ \(ert, erat, erscp) -> 
    maybeToRight ("invalid pointer dereference", cur) (CT.deref $ atype erat) >>= \y -> case CT.toTypeKind y of
        CT.CTVoid -> Left ("void value not ignored as it ought to be", cur)
        _ -> (\ty' ->  (ert, atUnary ATDeref ty' erat, erscp)) <$> maybeToRight ("incomplete value dereference", cur) (incomplete y scp)
unary ((_, HT.TKReserved "++"):xs) at scp = second3 (\x -> ATNode ATPreInc (atype x) x ATEmpty) <$> unary xs at scp
unary ((_, HT.TKReserved "--"):xs) at scp = second3 (\x -> ATNode ATPreDec (atype x) x ATEmpty) <$> unary xs at scp
unary xs at scp = either Left (uncurry3 f) $ factor xs at scp
    where
        f (cur@(_, HT.TKReserved "["):xs') erat !erscp = (>>=) (expr xs' erat erscp) $ \(ert', erat', erscp') -> case ert' of
            (_, HT.TKReserved "]"):xs'' -> maybeToRight ("invalid operands", cur) (addKind erat erat') >>= \erat'' ->
                maybeToRight ("subscripted value is neither array nor pointer nor vector", HT.altEmptyToken xs) (CT.deref $ atype erat'') >>= \t -> 
                    maybeToRight ("incomplete value dereference", cur) (incomplete t erscp') >>= \t' -> f xs'' (atUnary ATDeref t' erat'') erscp'
            _ -> Left $ if null ert' then ("expected expression after '[' token", cur) else ("expected expression before '" <> tshow (snd (head ert')) <> "' token", head ert')
        f (cur@(_, HT.TKReserved "."):xs') erat !erscp 
            | CT.isCTStruct (atype erat) || CT.isIncompleteStruct (atype erat) = if null xs' then Left ("expected identifier at end of input", cur) else case head xs' of
                (_, HT.TKIdent ident) -> maybeToRight ("incomplete type '" <> tshow (atype erat) <> "'", cur) (incomplete (atype erat) erscp) >>= \t ->
                    maybeToRight ("no such member", cur) (CT.lookupMember ident (CT.toTypeKind t)) >>= \mem ->
                        f (tail xs') (atMemberAcc mem erat) erscp
                _ -> Left ("expected identifier after '.' token", cur)
            | otherwise = Left ("request for a member in something not a structure or union", cur)
        f (cur@(_, HT.TKReserved "->"):xs') erat !erscp
            | maybe False CT.isCTStruct (CT.deref $ atype erat) || maybe False CT.isIncompleteStruct (CT.deref $ atype erat) = if null xs' then Left ("expected identifier at end of input", cur) else
                case head xs' of
                    (_, HT.TKIdent ident) -> maybeToRight ("incomplete type '" <> tshow (atype erat) <> "'", cur) (incomplete (fromJust (CT.deref $ atype erat)) erscp) >>= \t ->
                        maybeToRight ("no such member", cur) (CT.lookupMember ident (CT.toTypeKind t)) >>= \mem ->
                            f (tail xs') (atMemberAcc mem (atUnary ATDeref (CT.SCAuto $ CT.smType mem) erat)) erscp
                    _ -> Left ("expected identifier after '->' token", cur)
            | otherwise = Left ("invalid type argument of '->'" <> if CT.isCTUndef (atype erat) then "" else " (have '" <> tshow (atype erat) <> "')", cur)
        f ((_, HT.TKReserved "++"):xs') erat !erscp = f xs' (atUnary ATPostInc (atype erat) erat) erscp
        f ((_, HT.TKReserved "--"):xs') erat !erscp = f xs' (atUnary ATPostDec (atype erat) erat) erscp
        f ert erat !erscp = Right (ert, erat, erscp)

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
factor [] atn !scp = Right ([], atn, scp)
factor ((_, HT.TKReserved "("):xs@((_, HT.TKReserved "{"):_)) _ !scp = (>>=) (maybeToRight (internalCE, head xs) (takeBrace "{" "}" xs)) $ -- for statement expression (GNU extension: <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>)
    either (Left . ("the statement expression is not closed",)) $ \(sctk, ds) -> case ds of
        (_, HT.TKReserved ")"):ds' -> runST $ do
            eri <- newSTRef Nothing
            v <- newSTRef $ succNest scp
            lastA <- newSTRef ATEmpty 
            mk <- flip unfoldrM (init $ tail sctk) $ \ert -> if null ert then return Nothing else do
                erscp <- readSTRef v
                flip (either $ \err -> Nothing <$ writeSTRef eri (Just err)) (stmt ert ATEmpty erscp) $ \(ert', erat', erscp') -> 
                    Just (erat', ert') <$ (writeSTRef v erscp' >> when (case erat' of ATEmpty -> False; _ -> True) (writeSTRef lastA erat'))
            (>>=) (readSTRef eri) $ flip maybe (return . Left) $ do
                v' <- readSTRef v
                flip fmap (readSTRef lastA) $ \case
                        (ATNode ATExprStmt _ lhs _) -> Right (ds', atNoLeaf (ATStmtExpr (init mk ++ [lhs])) (atype lhs), fallBack scp v')
                        _ -> Left ("void value not ignored as it ought to be. the statement expression starts here:", head xs)
        _ -> Left $ if null sctk then ("expected ')' token. the statement expression starts here: ", head xs) else
            ("expected ')' token after '" <> tshow (snd $ last sctk) <> "' token", last sctk)
factor (cur@(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for (expr)
    (_, HT.TKReserved ")"):ys -> Right (ys, erat, erscp)
    ert' -> Left $ expectedMessage ")" cur ert'
factor ((_, HT.TKNum n):xs) _ !scp = Right (xs, atNumLit n, scp) -- for numbers
factor (cur@(_, HT.TKIdent v):(_, HT.TKReserved "("):(_, HT.TKReserved ")"):xs) _ !scp = case lookupFunction v scp of -- for no arguments function call
    Nothing -> Right (xs, atNoLeaf (ATCallFunc v Nothing) (CT.SCAuto CT.CTInt), pushWarn ("the function '" <> v <> "' is not declared.") cur scp)
    Just fn -> Right (xs, atNoLeaf (ATCallFunc v Nothing) (PSF.fntype fn), scp)
factor (cur1@(_, HT.TKIdent v):cur2@(_, HT.TKReserved "("):xs) _ scp = (>>=) (maybeToRight (internalCE, cur1) (takeBrace "(" ")" (cur2:xs))) $ -- for some argumets function call
    either (Left . ("invalid function call",)) $ \(fsec, ds) -> case lookupFunction v scp of
        Nothing -> f fsec ds (pushWarn ("The function '" <> tshow (snd cur1) <> "' is not declared.") cur1 scp) $ CT.SCAuto CT.CTInt
        Just fn -> f fsec ds scp (PSF.fntype fn) 
    where
        f fsec ds scp' t = maybeToRight ("invalid function call", cur1) (takeExps fsec) >>= \exps -> runST $ do
            mk <- newSTRef scp'
            expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
            if any isLeft expl then return $ Left $ head $ lefts expl else do
                scp'' <- readSTRef mk
                return $ Right (ds, atNoLeaf (ATCallFunc v (Just $ rights expl)) t, scp'')
factor (cur0@(_, HT.TKSizeof):cur@(_, HT.TKReserved "("):xs) atn scp = case takeTypeName xs scp of
    Left _ -> second3 (atNumLit . fromIntegral . CT.sizeof . atype) <$> unary (cur:xs) atn scp -- for `sizeof(variable)`
    Right (t, (_, HT.TKReserved ")"):ds) -> (ds, , scp) . atNumLit . fromIntegral . CT.sizeof <$> 
        maybeToRight ("invalid application of 'sizeof' to incomplete type '" <> tshow (CT.toTypeKind t) <> "'", cur0) (incomplete t scp) 
    Right _ -> Left ("The token ')' corresponding to '(' is expected", cur) 
factor ((_, HT.TKSizeof):xs) atn !scp = second3 (atNumLit . fromIntegral . CT.sizeof . atype) <$> unary xs atn scp -- for `sizeof variable` -- TODO: the type of sizeof must be @size_t@
factor (cur@(_, HT.TKAlignof):xs) atn !scp = (>>=) (unary xs atn scp) $ \(ert, erat, erscp) -> 
    if CT.isCTUndef (atype erat) then Left ("_Alignof must be an expression or type", cur) else Right (ert, atNumLit $ fromIntegral $ CT.alignof $ atype erat, erscp) -- Note: Using alignof for expressions is a non-standard feature of C11
factor (cur@(_, HT.TKString slit):xs) _ !scp = uncurry (xs,,) <$> addLiteral (CT.SCAuto $ CT.CTArray (fromIntegral $ B.length slit) CT.CTChar) cur scp -- for literals
factor (cur@(_, HT.TKIdent ident):xs) _ !scp = case lookupVar ident scp of
    FoundGVar (PV.GVar t _) -> Right (xs, atGVar t ident, scp) -- for declared global variable
    FoundLVar sct -> Right (xs, treealize sct, scp) -- for declared local variable
    FoundEnum sct -> Right (xs, treealize sct, scp) -- for declared enumerator
    NotFound -> Left ("The '" <> ident <> "' is not defined variable", cur)
factor ert _ _ = Left (if null ert then "unexpected token in program" else "unexpected token '" <> tshow (snd (head ert)) <> "' in program", HT.altEmptyToken ert)

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `parse` returns the error message and the token at the error location.
-- Otherwise, `parse` returns a list of abstract syntax trees, a set of global variables, and a list of literals.
parse :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ASTResult i
parse = fmap (\(ast, sc) -> (warns sc, ast, PV.globals $ vars $ scope sc, PV.literals $ vars $ scope sc)) . flip program initConstructionData

-- | `stackSize` returns the stack size of variable per function.
stackSize :: (Show i, Integral i) => ATree i -> Natural
stackSize (ATNode (ATDefFunc _ args) _ body _) = let ms = f body $ maybe S.empty (foldr (\(ATNode (ATLVar t x) _ _ _) acc -> S.insert (t, x) acc) S.empty) args in
    if S.size ms == 1 then toNatural $ flip CT.alignas 8 $ toInteger $ CT.sizeof $ fst $ head (S.toList ms) else toNatural $ flip CT.alignas 8 $ uncurry (+) $
        first (toInteger . CT.sizeof . fst) $ second (fromIntegral . snd) $ dupe $ foldl' (\acc x -> if snd acc < snd x then x else acc) (CT.SCUndef CT.CTUndef, 0) $ S.toList ms
    where
        f ATEmpty !s = s
        f (ATNode (ATCallFunc _ (Just arg)) t l r) !s = f (ATNode (ATBlock arg) t l r) s
        f (ATNode (ATLVar t x) _ l r) !s = let i = S.insert (t, x) s in f l i `S.union` f r i
        f (ATNode (ATBlock xs) _ l r) !s = let i = foldr (S.union . (`f` s)) s xs in f l i `S.union` f r i
        f (ATNode (ATStmtExpr xs) t l r) !s = f (ATNode (ATBlock xs) t l r) s 
        f (ATNode (ATFor xs) _ l r) !s = let i = foldr (S.union . flip f s . fromATKindFor) S.empty xs in f l i `S.union` f r i
        f (ATNode (ATNull x) _ _ _) !s = f x s
        f (ATNode _ _ l r) !s = f l s `S.union` f r s
stackSize _ = 0

