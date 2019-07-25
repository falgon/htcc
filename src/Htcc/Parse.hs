{-|
Module      : Htcc.Parse
Description : LL (1) Parser
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module Htcc.Parse (
    LVar (..),
    ATKind (..),
    ATree (..),
    inners,
    add,
    term,
    unary,
    factor,
    relational,
    equality,
    assign,
    expr,
    stmt,
    program,
    parse
) where

import Data.Tuple.Extra (first, second, dupe, uncurry3)
import Data.List (find)

import Htcc.Utils (first3, second3)
import Htcc.Token

-- | The local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        name :: String, -- ^ The name of local variable
        offset :: a -- ^ The offset value
    } deriving Show

{-# INLINE lookupLVar #-}
lookupLVar :: Num i => Token i -> [LVar i] -> Maybe (LVar i)
lookupLVar (TKIdent s) lvs = find (\(LVar ss _) -> ss == s) lvs
lookupLVar _ _ = Nothing

-- | The syntax tree type
data ATKind a = ATAdd -- ^ \(+\) 
    | ATSub -- ^ \(-\)
    | ATMul -- ^ \(\times \)
    | ATDiv -- ^ \(\div\)
    | ATLT  -- ^ \(\lt\)
    | ATLEQ -- ^ \(\leq\)
    | ATGT  -- ^ \(\gt\)
    | ATGEQ -- ^ \(\geq\)
    | ATEQ  -- ^ \(=\)
    | ATNEQ -- ^ \(\not=\)
    | ATNum a -- ^ The number
    | ATAssign -- ^ The assign operator
    | ATReturn -- ^ The return keyword
    | ATIf -- ^ The if keyword
    | ATElse -- ^ The else keyword
    | ATLVar a -- ^ The local variable. It has a offset value
    deriving Show

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode (ATKind a) (ATree a) (ATree a) -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: Num i => [Token i] -> [LVar i] -> Maybe [(ATree i, [LVar i])]
program [] _ = Just []
program xs vars = maybe Nothing (\(ys, btn, ars) -> ((btn, ars) :) <$> program ys ars) $ stmt xs ATEmpty vars


-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
--
-- NOTE: 
-- 
-- Current status
--
-- \[
-- \begin{eqnarray}
-- {\rm stmt} &=& {\rm expr\ ";"}\\
-- &\mid& {\rm "if"\ "("\ expr\ ")"\ stmt}\ \left({\rm "else"\ stmt}\right)?\\
-- &\mid& {\rm "return"\ expr\ ";"}
-- \end{eqnarray}
-- \]
stmt :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
stmt (TKReturn:xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for `return`
    TKReserved ";":ys -> Just (ys, ATNode ATReturn erat ATEmpty, ervars)
    _ -> Nothing
stmt (TKIf:TKReserved "(":xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for `if`
    TKReserved ")":ys -> flip (maybe Nothing) (stmt ys erat ervars) $ \x -> case second3 (ATNode ATIf erat) x of
        (TKElse:zs, eerat, eervars) -> second3 (ATNode ATElse eerat) <$> stmt zs eerat eervars -- for `else`
        zs -> Just zs
    _ -> Nothing
stmt xs atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for only `;`
    TKReserved ";":ys -> Just (ys, erat, ervars)
    _ -> Nothing

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
expr = assign


-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
assign xs atn vars = flip (maybe Nothing) (equality xs atn vars) $ \(ert, erat, ervars) -> case ert of
    TKReserved "=":ys -> second3 (ATNode ATAssign erat) <$> assign ys erat ervars
    _ -> Just (ert, erat, ervars)


-- | `inners` is a general function for creating `equality`, `relational`, `add` and `term` in the following syntax (EBNF) of LL (1).
--
-- \[
-- \begin{eqnarray}
-- {\rm program} &=& {\rm stmt}^\ast\label{eq:eigth}\tag{1}\\
-- {\rm stmt} &=& {\rm expr}\ {\rm ";"}\label{eq:nineth}\ \mid\ {\rm "return"}\ {\rm expr}\ ";"\ \mid\ "{\rm if}"\ "("\ {\rm expr}\ ")"\ {\rm stmt}\ ("{\rm else}"\ {\rm stmt})?\tag{2}\\
-- {\rm expr} &=& {\rm assign}\\
-- {\rm assign} &=& {\rm equality} \left("="\ {\rm assign}\right)?\label{eq:seventh}\tag{3}\\
-- {\rm equality} &=& {\rm relational}\ \left("=="\ {\rm relational}\ \mid\ "!="\ {\rm relational}\right)^\ast\label{eq:fifth}\tag{4}\\
-- {\rm relational} &=& {\rm add}\ \left("\lt"\ {\rm add}\mid\ "\lt ="\ {\rm add}\mid\ "\gt"\ {\rm add}\mid\ "\gt ="\ {\rm add}\right)^\ast\label{eq:sixth}\tag{5}\\
-- {\rm add} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{6} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{7} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\label{eq:fourth}\tag{8} \\
-- {\rm factor} &=& {\rm num} \mid\ {\rm ident} \mid\ "(" {\rm add} ")"\label{eq:third}\tag{9}
-- \end{eqnarray}
-- \]
inners :: ([Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])) -> [(String, ATKind i)] -> [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
inners _ _ [] atn vars = Just ([], atn, vars)
inners f cs xs atn vars = maybe Nothing (uncurry3 (inners' f cs)) $ f xs atn vars
    where
        inners' _ _ [] at ars = Just ([], at, ars)
        inners' g ds ys at ars = flip (maybe (Just (ys, at, ars))) (find (\(c, _) -> case head ys of TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            maybe Nothing (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k at)) $ g (tail ys) at ars


-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i)
-- > equality xs atn vars = flip (maybe Nothing) (relational xs atn vars) $ uncurry3 equality'
-- >     where
-- >         equality' (TKReserved "+":ys) era ars = maybe Nothing (uncurry id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' (TKReserved "-":ys) era ars = maybe Nothing (uncurry id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
-- >         equality' ert era ars = Just (ert, era, ars)
equality :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
relational = inners add [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
add :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
add = inners term [("+", ATAdd), ("-", ATSub)]

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
term = inners unary [("*", ATMul), ("/", ATDiv)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
unary (TKReserved "+":xs) at vars = factor xs at vars
unary (TKReserved "-":xs) at vars = second3 (ATNode ATSub (ATNode (ATNum 0) ATEmpty ATEmpty)) <$> factor xs at vars
unary xs at vars = factor xs at vars

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: Num i => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
factor [] atn vars = Just ([], atn, vars)
factor (TKReserved "(":xs) atn vars = flip (maybe Nothing) (add xs atn vars) $ \(ert, erat, ervars) -> case ert of
    TKReserved ")":ys -> Just (ys, erat, ervars)
    _ -> Nothing
factor (TKNum n:xs) _ vars = Just (xs, ATNode (ATNum n) ATEmpty ATEmpty, vars)
factor (TKIdent v:xs) _ vars = maybe
    (let lvars = (LVar v (if null vars then 8 else offset (head vars) + 8)):vars in Just (xs, ATNode (ATLVar $ offset $ head lvars) ATEmpty ATEmpty, lvars))
    (\(LVar _ o) -> Just (xs, ATNode (ATLVar o) ATEmpty ATEmpty, vars)) $
    lookupLVar (TKIdent v) vars
factor _ _ _ = Nothing

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
-- `parse` is equivalent to `program`.
parse :: Num i => [Token i] -> Maybe ([ATree i], Int)
parse = fmap (first (map fst) . second (length . snd . last) . dupe) . flip program []
