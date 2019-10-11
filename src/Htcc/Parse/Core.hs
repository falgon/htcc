{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
{-|
Module      : Htcc.Parse.Core
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The C languge parser and AST constructor
-}
module Htcc.Parse.Core (
    -- * Recursive descent implementation functions
    program,
    globalDef,
    stmt,
    inners,
    bitwiseOr,
    bitwiseXor,
    bitwiseAnd,
    shift,
    add,
    term,
    unary,
    factor,
    relational,
    equality,
    assign,
    expr,
    -- * Parser
    parse,
    -- * Utilities
    stackSize
) where

import Data.Bits ((.&.), complement)
import qualified Data.ByteString as B
import Data.Tuple.Extra (first, second, uncurry3, snd3)
import Data.List (find)
import Data.List.Split (linesBy)
import Data.Either (isLeft, lefts, rights)
import Data.Maybe (isJust, fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad (forM, zipWithM, when)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)
import Numeric.Natural

import Htcc.Utils (first3, second3, tshow, toNatural, mapEither)
import qualified Htcc.Token as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parse.AST
import Htcc.Parse.Var
import Htcc.Parse.Utils

-- import Debug.Trace

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> Vars i -> Either (T.Text, HT.TokenLC i) ([ATree i], Vars i)
program [] vars = Right ([], vars)
program xs vars = either Left (\(ys, btn, vars') -> first (btn:) <$> program ys vars') $ globalDef xs ATEmpty vars

-- | `globalDef` parses global definitions (include functions and global variables)
globalDef :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
globalDef tks at va = (>>=) (HT.makeTypes tks) $ \(ty, ds) -> globalDef' ty ds at $ resetLocal va
    where
        checkErr ar f = let ar' = init $ tail ar in if not (null ar') && snd (head ar') == HT.TKReserved "," then Left ("unexpected ',' token", head ar') else
            let args = linesBy ((==HT.TKReserved ",") . snd) ar' in (>>=) (mapEither HT.makeTypes args) f
        globalDef' _ tk@(cur@(_, HT.TKIdent fname):(_, HT.TKReserved "("):_) atn vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" $ tail tk) $ -- for function definitions
            either (Left . ("invalid function definition",)) $ \(fndec, st) -> case st of
                ((_, HT.TKReserved "{"):_) -> checkErr fndec $ \args -> runST $ do
                    eri <- newSTRef Nothing
                    v <- newSTRef vars
                    mk <- flip unfoldrM args $ \args' -> if null args' then return Nothing else let arg = head args' in do
                        m <- uncurry addLVar (second head arg) <$> readSTRef v
                        flip (either ((<$) Nothing . writeSTRef eri . Just)) m $ \(vat, vars') ->
                            Just (vat, tail args') <$ writeSTRef v vars'
                    (>>=) (readSTRef eri) $ flip maybe (return . Left) $ 
                        fmap (second3 (flip (ATNode (ATDefFunc fname $ if null mk then Nothing else Just mk) CT.CTUndef) ATEmpty)) . stmt st atn <$> readSTRef v
                _ -> stmt tk atn vars
        globalDef' ty' (cur@(_, HT.TKIdent _):xs) _ vars = case HT.arrayDeclSuffix ty' xs of -- for global variables -- TODO: support initialize by global variables
            Nothing -> case xs of 
                (_, HT.TKReserved ";"):ds -> flip fmap (addGVar (ty', cur) vars) $ \(_, vars') -> (ds, ATEmpty, vars')
                _ -> Left ("expected ';' token after '" <> tshow (snd cur) <> "' token", cur)
            Just rs -> (>>=) rs $ \(tk, ds) -> case ds of
                (_, HT.TKReserved ";"):ds' -> flip fmap (addGVar (tk, cur) vars) $ \(_, vars') -> (ds', ATEmpty, vars')
                _ -> Left ("expected ';' token after '" <> tshow (snd cur) <> "' token", cur)
        globalDef' _ tk _ _ = Left ("invalid definition of global identifier", if null tk then (HT.TokenLCNums 0 0, HT.TKEmpty) else head tk)

-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
stmt (cur@(_, HT.TKReturn):xs) atn vars = (>>=) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @return@
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATReturn CT.CTUndef erat ATEmpty, ervars)
    ert' -> Left $ expectedMessage ";" cur ert'
stmt (cur@(_, HT.TKIf):(_, HT.TKReserved "("):xs) atn vars = (>>=) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @if@
    (_, HT.TKReserved ")"):ys -> (>>=) (stmt ys erat ervars) $ \x -> case second3 (ATNode ATIf CT.CTUndef erat) x of
        ((_, HT.TKElse):zs, eerat, eervars) -> second3 (ATNode ATElse CT.CTUndef eerat) <$> stmt zs eerat eervars -- for @else@
        zs -> Right zs
    ert' -> Left $ expectedMessage ")" cur ert'
stmt (cur@(_, HT.TKWhile):(_, HT.TKReserved "("):xs) atn vars = (>>=) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @while@
    (_, HT.TKReserved ")"):ys -> second3 (ATNode ATWhile CT.CTUndef erat) <$> stmt ys erat ervars
    ert' -> Left $ expectedMessage ")" cur ert'
stmt xxs@(cur@(_, HT.TKFor):(_, HT.TKReserved "("):_) atn vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" (tail xxs)) $ -- for @for@
    either (Left . ("expected ')' token. The subject iteration statement starts here:",)) $ \(forSt, ds) ->
        let fsect = linesBy ((== HT.TKReserved ";") . snd) (tail (init forSt)); stlen = length fsect in
            if stlen < 2 || stlen > 3 then 
                Left ("the iteration statement for must be `for (expression_opt; expression_opt; expression_opt) statement`. see section 6.8.5.", cur) else runST $ do
                    v <- newSTRef (atn, vars)
                    mk <- flip (`zipWithM` fsect) [ATForInit, ATForCond, ATForIncr] $ \fs at -> do
                        aw <- readSTRef v
                        either (return . Left) (\(_, ert, ervars) -> Right (at ert) <$ writeSTRef v (ert, ervars)) $ 
                            -- call `stmt` to register a variable only if it matches with declaration expression_opt
                            -- if isATForInit (at ATEmpty) && not (null fs) && HT.isTKType (snd $ head fs) then 
                                -- uncurry (stmt $ fs ++ [(fst $ last fs, HT.TKReserved ";")]) aw 
                            {-else-} if isATForInit (at ATEmpty) || isATForIncr (at ATEmpty) then  -- TODO: support @for@ to define local variables
                                uncurry ((.) (fmap (second3 (flip (ATNode ATExprStmt CT.CTUndef) ATEmpty))) . expr fs) aw 
                            else uncurry (expr fs) aw
                    if any isLeft mk then return $ Left $ head $ lefts mk else do -- TODO: more efficiency
                        let jo = [m | (Right m) <- mk, case fromATKindFor m of ATEmpty -> False; x -> not $ isEmptyExprStmt x] 
                            mkk = maybe (ATForCond (ATNode (ATNum 1) CT.CTInt ATEmpty ATEmpty) : jo) (const jo) $ find isATForCond jo
                        (anr, vsr) <- readSTRef v
                        case ds of 
                            ((_, HT.TKReserved ";"):ys) -> return $ Right (ys, ATNode (ATFor mkk) CT.CTUndef ATEmpty ATEmpty, vsr)
                            _ -> return $ second3 (flip (flip (flip ATNode CT.CTUndef) ATEmpty) ATEmpty . ATFor . (mkk ++) . (:[]) . ATForStmt) <$> stmt ds anr vsr
stmt xxs@(cur@(_, HT.TKReserved "{"):_) _ vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "{" "}" xxs) $ -- for compound statement
    either (Left . ("the compound statement is not closed",)) $ \(scope, ds) -> runST $ do
        eri <- newSTRef Nothing
        v <- newSTRef $ succNest vars
        mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
            ervars <- readSTRef v
            either (\err -> Nothing <$ writeSTRef eri (Just err)) (\(ert', erat', ervars') -> Just (erat', ert') <$ writeSTRef v ervars') $ stmt ert ATEmpty ervars
        (>>=) (readSTRef eri) $ flip maybe (return . Left) $ Right . (ds, ATNode (ATBlock mk) CT.CTUndef ATEmpty ATEmpty,) . fallBack vars <$> readSTRef v
stmt ((_, HT.TKReserved ";"):xs) atn vars = Right (xs, atn, vars) -- for only @;@
stmt tk atn vars
    | not (null tk) && (HT.isTKType (snd $ head tk) || HT.isTKStruct (snd $ head tk)) = (>>=) (HT.makeTypes tk) $ \(t, ds) -> case ds of -- for a local variable declaration
        cur2@(_, HT.TKIdent _):(_, HT.TKReserved ";"):ds' -> (>>=) (addLVar t cur2 vars) $ \(lat, vars') -> Right (ds', ATNode (ATNull lat) CT.CTUndef ATEmpty ATEmpty, vars')
        cur2@(_, HT.TKIdent _):(_, HT.TKReserved "="):ds' -> (>>=) (addLVar t cur2 vars) $ \(lat, vars') -> 
            (>>=) (expr ds' atn vars') $ \(ert, erat, ervar) -> case ert of
                (_, HT.TKReserved ";"):ds'' -> Right (ds'', ATNode ATExprStmt CT.CTUndef (ATNode ATAssign (atype lat) lat erat) ATEmpty, ervar)
                _ -> Left ("expected ';' token. The subject iteration statement start here:", head tk)
        cur2@(_, HT.TKIdent _):ds' -> flip (maybe (err ds (cur2:ds'))) (HT.arrayDeclSuffix t ds') $ either Left $ \(t', ds'') -> 
            (>>=) (addLVar t' cur2 vars) $ \(lat, vars') -> Right (ds'', ATNode (ATNull lat) CT.CTUndef ATEmpty ATEmpty, vars')
        ds' -> err ds ds'
    | otherwise = (>>=) (expr tk atn vars) $ \(ert, erat, ervars) -> case ert of -- for stmt;
        (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATExprStmt CT.CTUndef erat ATEmpty, ervars)
        ert' -> Left $ expectedMessage ";" (HT.TokenLCNums 0 0, HT.TKEmpty) ert'
    where
        err ds ds' = Left $ if null ds' then ("expected unqualified-id", head tk) else ("expected unqualified-id before '" <> tshow (snd (head ds)) <> T.singleton '\'', head ds')

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
expr = assign

-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
assign xs atn vars = (>>=) (bitwiseOr xs atn vars) $ \(ert, erat, ervars) -> case ert of
    (_, HT.TKReserved "="):ys -> second3 (ATNode ATAssign (atype erat) erat) <$> assign ys erat ervars
    _ -> Right (ert, erat, ervars)

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
-- {\rm assign} &=& {\rm bitwiseOr} \left("="\ {\rm assign}\right)?\label{eq:seventh}\tag{3}\\
-- {\rm bitwiseOr} &=& {\rm bitwiseXor}\ \left("|"\ {\rm bitwiseXor}\right)^\ast\label{eq:tenth}\tag{4}\\
-- {\rm bitwiseXor} &=& {\rm bitwiseAnd}\ \left("\hat{}"\ {\rm bitwiseAnd}\right)^\ast\label{eq:eleventh}\tag{5}\\
-- {\rm bitwiseAnd} &=& {\rm equality}\ \left("\&"\ {\rm equality}\right)^\ast\label{eq:twelveth}\tag{6}\\
-- {\rm equality} &=& {\rm relational}\ \left("=="\ {\rm relational}\ \mid\ "!="\ {\rm relational}\right)^\ast\label{eq:fifth}\tag{7}\\
-- {\rm relational} &=& {\rm shift}\ \left("\lt"\ {\rm shift}\mid\ "\lt ="\ {\rm shift}\mid\ "\gt"\ {\rm shift}\mid\ "\gt ="\ {\rm shift}\right)^\ast\label{eq:sixth}\tag{8}\\
-- {\rm shift} &=& {\rm add}\ \left("\lt\lt"\ {\rm add}\mid\ "\gt\gt"\ {\rm add}\right)^\ast\label{eq:thirteenth}\tag{9}\\
-- {\rm add} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{10} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{11} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\mid\ \left("!"\ \mid\ "\sim"\ \mid\ "\&"\ \mid\ "\ast"\right)?\ {\rm unary}\label{eq:fourth}\tag{12} \\
-- {\rm factor} &=& {\rm num} \mid\ {\rm ident}\ \left({\rm "(" \left(expr\ \left(\left(","\ expr\right)^\ast\right)?\right)? ")"}\right)?\ \mid\ "(" {\rm expr} ")"\label{eq:third}\tag{13}
-- \end{eqnarray}
-- \]
inners :: ([HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)) -> [(T.Text, ATKind i)] -> [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
inners _ _ [] atn vars = Right ([], atn, vars)
inners f cs xs atn vars = either Left (uncurry3 (inners' f cs)) $ f xs atn vars
    where
        inners' _ _ [] at ars = Right ([], at, ars)
        inners' g ds ys at ars = flip (maybe (Right (ys, at, ars))) (find (\(c, _) -> case snd (head ys) of HT.TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            either Left (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k CT.CTInt at)) $ g (tail ys) at ars

-- | `bitwiseOr` indicates \(\eqref{eq:tenth}\) among the comments of `inners`.
bitwiseOr :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
bitwiseOr = inners bitwiseXor [("|", ATOr)]

-- | `bitwiseXor` indicates \(\eqref{eq:eleventh}\) amont the comments of `inners`.
bitwiseXor :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
bitwiseXor = inners bitwiseAnd [("^", ATXor)]

-- | `bitwiseAnd` indicates \(\eqref{eq:twelveth}\) among the comments of `inners`.
bitwiseAnd :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
bitwiseAnd = inners equality [("&", ATAnd)]

-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [HT.TokenLC i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i)
-- > equality xs atn vars = (>>=) (relational xs atn vars) $ uncurry3 equality'
-- >     where
-- >         equality' ((_, HT.TKReserved "=="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' ((_, HT.TKReserved "!="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
-- >         equality' ert era ars = Right (ert, era, ars)
equality :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
relational = inners shift [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `shift` indicates \(\eqref{eq:thirteenth}\\) among the comments of `inners`.
shift :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
shift = inners add [("<<", ATShl), (">>", ATShr)]
        
{-# INLINE addKind #-}
addKind :: Show i => ATree i -> ATree i -> Maybe (ATree i)
addKind lhs rhs
    | all (CT.isFundamental . atype) [lhs, rhs] = Just $ ATNode ATAdd (max (atype lhs) (atype rhs)) lhs rhs
    | isJust (CT.derefMaybe $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATAddPtr (atype lhs) lhs rhs
    | CT.isFundamental (atype lhs) && isJust (CT.derefMaybe $ atype rhs) = Just $ ATNode ATAddPtr (atype rhs) rhs lhs
    | otherwise = Nothing

{-# INLINE subKind #-}
subKind :: ATree i -> ATree i -> Maybe (ATree i)
subKind lhs rhs
    | all (CT.isFundamental . atype) [lhs, rhs] = Just $ ATNode ATSub (max (atype lhs) (atype rhs)) lhs rhs
    | isJust (CT.derefMaybe $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATSubPtr (atype lhs) lhs rhs
    | all (isJust . CT.derefMaybe . atype) [lhs, rhs] = Just $ ATNode ATPtrDis (atype lhs) lhs rhs
    | otherwise = Nothing

-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
add :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
add xs atn vars = (>>=) (term xs atn vars) $ uncurry3 add'
    where
        add' (cur@(_, HT.TKReserved "+"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("invalid operands", cur))) (addKind era $ snd3 zz) $ \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' (cur@(_, HT.TKReserved "-"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("invalid operands", cur))) (subKind era $ snd3 zz) $ \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' ert erat ars = Right (ert, erat, ars)

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
term = inners unary [("*", ATMul), ("/", ATDiv), ("%", ATMod)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
unary ((_, HT.TKReserved "+"):xs) at vars = factor xs at vars
unary ((_, HT.TKReserved "-"):xs) at vars = second3 (ATNode ATSub CT.CTInt (ATNode (ATNum 0) CT.CTInt ATEmpty ATEmpty)) <$> factor xs at vars
unary ((_, HT.TKReserved "!"):xs) at vars = second3 (flip (ATNode ATNot CT.CTInt) ATEmpty) <$> unary xs at vars
unary ((_, HT.TKReserved "~"):xs) at vars = second3 (flip (ATNode ATBitNot CT.CTInt) ATEmpty) <$> unary xs at vars
unary ((_, HT.TKReserved "&"):xs) at vars = second3 (\x -> (ATNode ATAddr $ CT.CTPtr $ if CT.isCTArray (atype x) then fromJust $ CT.derefMaybe (atype x) else atype x) x ATEmpty) <$> unary xs at vars
unary (cur@(_, HT.TKReserved "*"):xs) at vars = (>>=) (unary xs at vars) $ \(ert, erat, ervars) -> 
    flip (maybe $ Left ("invalid pointer dereference", cur)) (CT.derefMaybe $ atype erat) $ \t -> Right (ert, ATNode ATDeref t erat ATEmpty, ervars)
unary xs at vars = either Left (uncurry3 f) $ factor xs at vars
    where
        f (cur@(_, HT.TKReserved "["):xs') erat ervars = (>>=) (expr xs' erat ervars) $ \(ert', erat', ervars') -> case ert' of
            (_, HT.TKReserved "]"):xs'' -> flip (maybe $ Left ("invalid operands", cur)) (addKind erat erat') $ \erat'' ->
                flip (maybe $ Left ("subscripted value is neither array nor pointer nor vector", if null xs then (HT.TokenLCNums 0 0, HT.TKEmpty) else head xs)) 
                    (CT.derefMaybe $ atype erat'') $ \t -> f xs'' (ATNode ATDeref t erat'' ATEmpty) ervars'
            _ -> Left $ if null ert' then ("expected expression after '[' token", cur) else ("expected expression before '" <> tshow (snd (head ert')) <> "' token", head ert')
        f (cur@(_, HT.TKReserved "."):xs') erat ervars 
            | CT.isCTStruct (atype erat) = if null xs' then Left ("expected identifier at end of input", cur) else case head xs' of 
                (_, HT.TKIdent ident) -> flip (maybe (Left ("no such member", cur))) (CT.lookupMember ident (atype erat)) $ \mem ->
                    f (tail xs') (ATNode (ATMemberAcc mem) (CT.smType mem) erat ATEmpty) ervars
                _ -> Left ("expected identifier after '.' token", cur)
            | otherwise = Left ("request for a member in something not a structure or union", cur)
        f ert erat ervars = Right (ert, erat, ervars)

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Show i, Eq i, Read i, Integral i) => [HT.TokenLC i] -> ATree i -> Vars i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Vars i)
factor [] atn vars = Right ([], atn, vars)
factor ((_, HT.TKReserved "("):xs@((_, HT.TKReserved "{"):_)) _ vars = flip (maybe $ Left (internalCE, head xs)) (HT.takeBrace "{" "}" xs) $ -- for statement expression (GNU extension: <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>)
    either (Left . ("the statement expression is not closed",)) $ \(scope, ds) -> case ds of
        (_, HT.TKReserved ")"):ds' -> runST $ do
            eri <- newSTRef Nothing
            v <- newSTRef $ succNest vars
            lastA <- newSTRef ATEmpty 
            mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
                ervars <- readSTRef v
                flip (either $ \err -> Nothing <$ writeSTRef eri (Just err)) (stmt ert ATEmpty ervars) $ \(ert', erat', ervars') -> 
                    Just (erat', ert') <$ (writeSTRef v ervars' >> when (case erat' of ATEmpty -> False; _ -> True) (writeSTRef lastA erat'))
            (>>=) (readSTRef eri) $ flip maybe (return . Left) $ do
                v' <- readSTRef v
                flip fmap (readSTRef lastA) $ \case
                        (ATNode ATExprStmt _ lhs _) -> Right (ds', ATNode (ATStmtExpr (init mk ++ [lhs])) (atype lhs) ATEmpty ATEmpty, fallBack vars v')
                        _ -> Left ("void value not ignored as it ought to be. the statement expression starts here:", head xs)
        _ -> Left $ if null scope then ("expected ')' token. the statement expression starts here: ", head xs) else
            ("expected ')' token after '" <> tshow (snd $ last scope) <> "' token", last scope)
factor (cur@(_, HT.TKReserved "("):xs) atn vars = (>>=) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for (expr)
    (_, HT.TKReserved ")"):ys -> Right (ys, erat, ervars)
    ert' -> Left $ expectedMessage ")" cur ert'
factor ((_, HT.TKNum n):xs) _ vars = Right (xs, ATNode (ATNum n) CT.CTInt ATEmpty ATEmpty, vars) -- for numbers
factor ((_, HT.TKIdent v):(_, HT.TKReserved "("):(_, HT.TKReserved ")"):xs) _ vars = Right (xs, ATNode (ATCallFunc v Nothing) CT.CTInt ATEmpty ATEmpty, vars) -- for no arguments function call
factor (cur1@(_, HT.TKIdent v):cur2@(_, HT.TKReserved "("):xs) _ vars = flip (maybe $ Left (internalCE, cur1)) (HT.takeBrace "(" ")" (cur2:xs)) $
    either (Left . ("invalid function call",)) $ \(fsec, ds) -> flip (maybe $ Left ("invalid function call", cur1)) (HT.takeExps (cur1:fsec)) $ \exps -> runST $ do
        mk <- newSTRef vars
        expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
        if any isLeft expl then return $ Left $ head $ lefts expl else do
            vars' <- readSTRef mk
            return $ Right (ds, ATNode (ATCallFunc v (Just $ rights expl)) CT.CTInt ATEmpty ATEmpty, vars')
factor ((_, HT.TKSizeof):xs) atn vars = second3 (\x -> ATNode (ATNum (fromIntegral $ CT.sizeof $ atype x)) CT.CTInt ATEmpty ATEmpty) <$> unary xs atn vars -- for `sizeof` -- TODO: the type of sizeof must be @size_t@
factor (cur@(_, HT.TKString slit):xs) _ vars = uncurry (xs,,) <$> addLiteral (CT.CTArray (fromIntegral $ B.length slit) CT.CTChar, cur) vars -- for literals
factor (cur@(_, HT.TKIdent ident):xs) _ vars = flip (maybe (Left ("undefined variable", cur))) (lookupVar ident vars) $ \case -- if the variable is not declared, it returns error wrapped with `Left`
    Right (LVar t o _) -> Right (xs, ATNode (ATLVar t o) t ATEmpty ATEmpty, vars) -- for declared local variable
    Left (GVar t) -> Right (xs, ATNode (ATGVar t ident) t ATEmpty ATEmpty, vars) -- for declared global variable
factor ert _ _ = Left (if null ert then "unexpected token in program" else "unexpected token '" <> tshow (snd (head ert)) <> "' in program", if null ert then (HT.TokenLCNums 0 0, HT.TKEmpty) else head ert)

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: (Show i, Num i, Eq i, Integral i, Read i) => [HT.TokenLC i] -> Either (T.Text, HT.TokenLC i) ([ATree i], M.Map T.Text GVar, [Literal])
parse = fmap (\(ast, vars) -> (ast, globals vars, literals vars)) . flip program initVars

-- | `stackSize` returns the stack size of variable per function.
stackSize :: (Show i, Ord i) => ATree i -> Natural
stackSize (ATNode (ATDefFunc _ args) _ body _) = toNatural $ alignas 8 $ sum $ map ((fromIntegral :: Natural -> Integer) . CT.sizeof . fst) $ S.toList $ f body $
    maybe S.empty (foldr (\(ATNode (ATLVar t x) _ _ _) acc -> S.insert (t, x) acc) S.empty) args 
    where
        f ATEmpty s = s
        f (ATNode (ATCallFunc _ (Just arg)) t l r) s = f (ATNode (ATBlock arg) t l r) s
        f (ATNode (ATLVar t x) _ l r) s = let i = S.insert (t, x) s in f l i `S.union` f r i
        f (ATNode (ATBlock xs) _ l r) s = let i = foldr (S.union . (`f` s)) s xs in f l i `S.union` f r i
        f (ATNode (ATStmtExpr xs) t l r) s = f (ATNode (ATBlock xs) t l r) s 
        f (ATNode (ATFor xs) _ l r) s = let i = foldr (S.union . flip f s . fromATKindFor) S.empty xs in f l i `S.union` f r i
        f (ATNode (ATNull x) _ _ _) s = f x s
        f (ATNode _ _ l r) s = f l s `S.union` f r s
        alignas aval n = pred (n + aval) .&. complement (pred aval)
stackSize _ = 0

