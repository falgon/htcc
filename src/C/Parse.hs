{-|
Module      : C.Parse
Description : LL (1) Parser
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module C.Parse (
    ATKind (..),
    ATree (..),
    inners,
    add,
    term,
    unary,
    factor,
    relational,
    equality,
    expr,
    parse
) where

import Data.Tuple.Extra (first, second)
import Data.List (find)
import C.Token

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


-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode (ATKind a) (ATree a) (ATree a) -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has


-- | `inners` is a general function for creating `equality`, `relational`, `add` and `term` in the following syntax (EBNF) of LL (1).
--
-- \[
-- \begin{eqnarray}
-- {\rm expr} &=& {\rm equality}\\
-- {\rm equality} &=& {\rm relational}\ \left("=="\ {\rm relational}\ \mid\ "!="\ {\rm relational}\right)^\ast\label{eq:fifth}\tag{1}\\
-- {\rm relational} &=& {\rm add}\ \left("\lt"\ {\rm add}\mid\ "\lt ="\ {\rm add}\mid\ "\gt"\ {\rm add}\mid\ "\gt ="\ {\rm add}\right)^\ast\label{eq:sixth}\tag{2}\\
-- {\rm add} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{3} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{4} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\label{eq:fourth}\tag{5} \\
-- {\rm factor} &=& {\rm num} \mid\ "(" {\rm add} ")"\label{eq:third}\tag{6}
-- \end{eqnarray}
-- \]
inners :: ([Token i] -> ATree i -> Maybe ([Token i], ATree i)) -> [(String, ATKind i)] -> [Token i] -> ATree i -> Maybe ([Token i], ATree i)
inners _ _ [] atn = Just ([], atn)
inners f cs xs atn = maybe Nothing (uncurry (inners' f cs)) $ f xs atn
    where
        inners' _ _ [] at = Just ([], at)
        inners' g ds ys at = flip (maybe (Just (ys, at))) (find (\(c, _) -> case head ys of TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            maybe Nothing (uncurry id . first (inners' f cs) . second (ATNode k at)) $ g (tail ys) at

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`
expr :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
expr = equality

-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > equality xs atn = flip (maybe Nothing) (relational xs atn) $ \(ert, erat) -> equality' ert erat
-- >     where
-- >         equality' (TKReserved "+":ys) era = maybe Nothing (uncurry id . first equality' . second (ATNode ATEQ era)) $ relational ys era
-- >         equality' (TKReserved "-":ys) era = maybe Nothing (uncurry id . first equality' . second (ATNode ATNEQ era)) $ relational ys era
-- >         equality' ert era = Just (ert, era)
equality :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
-- > relational ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > relational xs atn = flip (maybe Nothing) (add xs atn) $ \(ert, erat) -> relational' ert erat
-- >     where
-- >         relational' (TKReserved "<":ys) era = maybe Nothing (uncurry id . first relational' . second (ATNode ATLT era)) $ add ys era
-- >         relational' (TKReserved "<=":ys) era = maybe Nothing (uncurry id . first relational' . second (ATNode ATLEQ era)) $ add ys era
-- >         relational' ert era = Just (ert, era)
relational :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
relational = inners add [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
-- 
-- > add ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > add xs atn = flip (maybe Nothing) (term xs atn) $ \(ert, erat) -> add' ert erat
-- >     where
-- >         add' (TKReserved "+":ys) era = maybe Nothing (uncurry id . first add' . second (ATNode ATAdd era)) $ term ys era
-- >         add' (TKReserved "-":ys) era = maybe Nothing (uncurry id . first add' . second (ATNode ATSub era)) $ term ys era
-- >         add' ert era = Just (ert, era)
add :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
add = inners term [("+", ATAdd), ("-", ATSub)]

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
-- Same as `add`. This is equivalent to the following code:
--
-- > term ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > term xs atn = flip (maybe Nothing) (unary xs atn) $ \(ert, erat) -> term' ert erat
-- >     where
-- >         term' (TKReserved "*":ys) era = maybe Nothing (uncurry id . first term' . second (ATNode ATMul era)) $ unary ys era
-- >         term' (TKReserved "/":ys) era = maybe Nothing (uncurry id . first term' . second (ATNode ATDiv era)) $ unary ys era
-- >         term' ert era = Just (ert, era)
term ::  Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
term = inners unary [("*", ATMul), ("/", ATDiv)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
unary (TKReserved "+":xs) at = factor xs at
unary (TKReserved "-":xs) at = maybe Nothing (Just . second (ATNode ATSub (ATNode (ATNum 0) ATEmpty ATEmpty))) $ factor xs at
unary xs at = factor xs at

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
factor [] atn = Just ([], atn)
factor (TKReserved "(":xs) atn = flip (maybe Nothing) (add xs atn) $ \(ert, erat) -> case ert of
    TKReserved ")":ys -> Just (ys, erat)
    _ -> Nothing
factor (TKNum n:xs) _ = Just (xs, ATNode (ATNum n) ATEmpty ATEmpty)
factor _ _ = Nothing

-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: Num i => [Token i] -> Maybe (ATree i)
parse = fmap snd . flip expr ATEmpty
