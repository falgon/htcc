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
    parse
) where

import Data.Tuple.Extra (first, second)
import Data.List (find)
import C.Token

-- | Syntax tree type
data ATKind a = ATAdd | ATSub | ATMul | ATDiv | ATNum a

-- | Abstract syntax tree
data ATree a = ATEmpty | ATNode (ATKind a) (ATree a) (ATree a)

-- | `inners` is a general function for creating expr and factor in the following syntax (EBNF)
-- 
-- expr     = factor ("+" factor | "-" factor)*
-- factor   = term ("*" term | "/" term)*
-- term     = num | "(" expr ")"
--
-- of LL (1).
inners :: ([Token i] -> ATree i -> Maybe ([Token i], ATree i)) -> [(Char, ATKind i)] -> [Token i] -> ATree i -> Maybe ([Token i], ATree i)
inners _ _ [] atn = Just ([], atn)
inners f cs xs atn = maybe Nothing (uncurry (inners' f cs)) $ f xs atn --  flip (maybe Nothing) (f xs atn) $ \(ert, erat) -> inners' f cs ert erat 
    where
        inners' _ _ [] at = Just ([], at)
        inners' g ds ys at = flip (maybe (Just (ys, at))) (find (\(c, _) -> case head ys of TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            maybe Nothing (uncurry id . first (inners' f cs) . second (ATNode k at)) $ g (tail ys) at


-- | `expr` indicates "expr = factor ("+" factor | "-" factor)*" among the comments of `inners`.
-- This is equivalent to the following code:
-- @
--      expr ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
--      expr xs atn = flip (maybe Nothing) (factor xs atn) $ \(ert, erat) -> expr' ert erat
--          where
--              expr' (TKReserved '+':ys) era = maybe Nothing (\(zrt, zrat) -> expr' zrt $ ATNode ATAdd era zrat) $ factor ys era
--              expr' (TKReserved '-':ys) era = maybe Nothing (\(zrt, zrat) -> expr' zrt $ ATNode ATSub era zrat) $ factor ys era
--              expr' ert era = Just (ert, era)
--
-- @
expr ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
expr = inners factor [('+', ATAdd), ('-', ATSub)]

-- | `factor` indicates "term ("*" term | "/" term)*" amont the comments of `inners`.
-- Same as `expr`.
factor ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
factor = inners term [('*', ATMul), ('/', ATDiv)]

-- | `term` indicates "num | "(" expr ")"" amount the comments of `inners`.
term ::  [Token i] -> ATree i -> Maybe ([Token i], ATree i)
term [] atn = Just ([], atn)
term (TKReserved '(':xs) atn = flip (maybe Nothing) (expr xs atn) $ \(ert, erat) -> case ert of
    TKReserved ')':ys -> Just (ys, erat)
    _ -> Nothing
term (TKNum n:xs) _ = Just (xs, ATNode (ATNum n) ATEmpty ATEmpty)
term _ _ = Nothing

-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse ::  [Token i] -> Maybe (ATree i)
parse = fmap snd . flip expr ATEmpty
