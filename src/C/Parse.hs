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
    expr,
    term,
    unary,
    factor,
    parse
) where

import Data.Tuple.Extra (first, second)
import Data.List (find)
import C.Token

-- | Syntax tree type
data ATKind a = ATAdd | ATSub | ATMul | ATDiv | ATNum a

-- | Abstract syntax tree
data ATree a = ATEmpty | ATNode (ATKind a) (ATree a) (ATree a)

-- | `inners` is a general function for creating expr and factor in the following syntax (EBNF) of LL (1).
--
-- \[
-- \begin{eqnarray}
-- {\rm expr} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{1} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{2} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\label{eq:fourth}\tag{3} \\
-- {\rm factor} &=& {\rm num} \mid\ "(" {\rm expr} ")"\label{eq:third}\tag{4}
-- \end{eqnarray}
-- \]
inners :: ([Token i] -> ATree i -> Maybe ([Token i], ATree i)) -> [(Char, ATKind i)] -> [Token i] -> ATree i -> Maybe ([Token i], ATree i)
inners _ _ [] atn = Just ([], atn)
inners f cs xs atn = maybe Nothing (uncurry (inners' f cs)) $ f xs atn
    where
        inners' _ _ [] at = Just ([], at)
        inners' g ds ys at = flip (maybe (Just (ys, at))) (find (\(c, _) -> case head ys of TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            maybe Nothing (uncurry id . first (inners' f cs) . second (ATNode k at)) $ g (tail ys) at


-- | `expr` indicates \(\eqref{eq:first}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
-- 
-- > expr ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > expr xs atn = flip (maybe Nothing) (term xs atn) $ \(ert, erat) -> expr' ert erat
-- >     where
-- >         expr' (TKReserved '+':ys) era = maybe Nothing (uncurry id . first expr' . second (ATNode ATAdd era)) $ term ys era
-- >         expr' (TKReserved '-':ys) era = maybe Nothing (uncurry id . first expr' . second (ATNode ATSub era)) $ term ys era
-- >         expr' ert era = Just (ert, era)
expr :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
expr = inners term [('+', ATAdd), ('-', ATSub)]

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
-- Same as `expr`. This is equivalent to the following code:
--
-- > term ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
-- > term xs atn = flip (maybe Nothing) (unary xs atn) $ \(ert, erat) -> term' ert erat
-- >     where
-- >         term' (TKReserved '*':ys) era = maybe Nothing (uncurry id . first expr' . second (ATNode ATMul era)) $ unary ys era
-- >         term' (TKReserved '/':ys) era = maybe Nothing (uncurry id . first expr' . second (ATNode ATDiv era)) $ unary ys era
-- >         term' ert era = Just (ert, era)
term ::  Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
term = inners unary [('*', ATMul), ('/', ATDiv)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
unary (TKReserved '+':xs) at = factor xs at
unary (TKReserved '-':xs) at = maybe Nothing (Just . second (ATNode ATSub (ATNode (ATNum 0) ATEmpty ATEmpty))) $ factor xs at
unary xs at = factor xs at

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: Num i => [Token i] -> ATree i -> Maybe ([Token i], ATree i)
factor [] atn = Just ([], atn)
factor (TKReserved '(':xs) atn = flip (maybe Nothing) (expr xs atn) $ \(ert, erat) -> case ert of
    TKReserved ')':ys -> Just (ys, erat)
    _ -> Nothing
factor (TKNum n:xs) _ = Just (xs, ATNode (ATNum n) ATEmpty ATEmpty)
factor _ _ = Nothing

-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: Num i => [Token i] -> Maybe (ATree i)
parse = fmap snd . flip expr ATEmpty
