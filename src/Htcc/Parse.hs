{-# LANGUAGE LambdaCase #-}
{-|
Module      : Htcc.Parse
Description : The C languge parser
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module Htcc.Parse (
    LVar (..),
    ATKindFor (..),
    ATKind (..),
    ATree (..),
    isATForInit,
    isATForCond,
    isATForStmt,
    isATForIncr,
    fromATKindFor,
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

import Data.Tuple.Extra (first, second, dupe, uncurry3, snd3, thd3)
import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as T
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)

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

-- | Specially for syntax tree type
data ATKindFor a = ATForkw -- ^ The for keyword
    | ATForInit (ATree a) -- ^ The initial section of for statement
    | ATForCond (ATree a) -- ^ The conditional section of for statement
    | ATForIncr (ATree a) -- ^ The incremental section of for statement
    | ATForStmt (ATree a) -- ^ The statement section of for statement
    deriving Show

{-# INLINE isATForInit #-}
-- | An utility of `ATForInit`. When an argument is `ATForInit`, return `True` otherwise `False`
isATForInit :: ATKindFor a -> Bool
isATForInit (ATForInit _) = True
isATForInit _ = False

-- | An utility of `ATForCond`. When an argument is `ATForCond`, return `True` otherwise `False`
{-# INLINE isATForCond #-}
isATForCond :: ATKindFor a -> Bool
isATForCond (ATForCond _) = True
isATForCond _ = False

-- | An utility `ATForStmt`. When an argument is `ATForStmt`, return `True` otherwise `False`
{-# INLINE isATForStmt #-}
isATForStmt :: ATKindFor a -> Bool
isATForStmt (ATForStmt _) = True
isATForStmt _ = False

-- | An utility `ATForIncr`. When an argument is `ATForIncr`, return `True` otherwise `False`
{-# INLINE isATForIncr #-}
isATForIncr :: ATKindFor a -> Bool
isATForIncr (ATForIncr _) = True
isATForIncr _ = False

-- | take ATree data from `ATKindFor`.
{-# INLINE fromATKindFor #-}
fromATKindFor :: ATKindFor a -> ATree a
fromATKindFor (ATForInit x) = x
fromATKindFor (ATForCond x) = x
fromATKindFor (ATForIncr x) = x
fromATKindFor (ATForStmt x) = x
fromATKindFor _ = error "ATKindFor is ATForkw"

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
    | ATWhile -- ^ The while keyword
    | ATFor [ATKindFor a] -- ^ The for keyword
    | ATBlock [ATree a] -- ^ The block
    | ATLVar a -- ^ The local variable. It has a offset value
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ The function. It has a offset value and arguments (`Maybe`).
    deriving Show

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode (ATKind a) (ATree a) (ATree a) -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Eq i, Num i) => [Token i] -> [LVar i] -> Maybe [(ATree i, [LVar i])]
program [] _ = Just []
program xs vars = maybe Nothing (\(ys, btn, ars) -> ((btn, ars) :) <$> program ys ars) $ stmt xs ATEmpty vars

-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
stmt (TKReturn:xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for `return`
    TKReserved ";":ys -> Just (ys, ATNode ATReturn erat ATEmpty, ervars)
    _ -> Nothing
stmt (TKIf:TKReserved "(":xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for `if`
    TKReserved ")":ys -> flip (maybe Nothing) (stmt ys erat ervars) $ \x -> case second3 (ATNode ATIf erat) x of
        (TKElse:zs, eerat, eervars) -> second3 (ATNode ATElse eerat) <$> stmt zs eerat eervars -- for `else`
        zs -> Just zs
    _ -> Nothing
stmt (TKWhile:TKReserved "(":xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for `while`
    TKReserved ")":ys -> second3 (ATNode ATWhile erat) <$> stmt ys erat ervars
    _ -> Nothing
stmt (TKFor TKForkw:xs) atn vars = runST $ do
    v <- newSTRef (atn, vars)
    mkk <- forM (takeWhile isTKFor xs) $ \case
        TKFor (TKForInit tk) -> f v tk ATForInit
        TKFor (TKForCond tk) -> f v tk ATForCond
        TKFor (TKForIncr tk) -> f v tk ATForIncr
        _ -> error "this function should not reach here"
    if any isNothing mkk then return Nothing else do
        let jo = map fromJust mkk
            mk = maybe (ATForCond (ATNode (ATNum 1) ATEmpty ATEmpty) : jo) (const jo) $ find isATForCond jo -- `for (;;)` means `for (;1;)`
            elseTkf = dropWhile isTKFor xs
        (anr, vsr) <- readSTRef v
        case elseTkf of
            (TKReserved ";":ys) -> return $ Just (ys, ATNode (ATFor mk) ATEmpty ATEmpty, vsr)
            _ -> return $ second3 (flip (flip ATNode ATEmpty) ATEmpty . ATFor . (mk ++) . (:[]) . ATForStmt) <$> stmt elseTkf anr vsr
    where
        f v tk fk = do
            aw <- readSTRef v
            maybe (return Nothing) (\(_, ert, ervars) -> Just (fk ert) <$ writeSTRef v (ert, ervars)) $ uncurry (expr tk) aw 
stmt (TKReserved "{":xs) atn vars = let scope = takeWhile (TKReserved "}" /=) xs in runST $ do -- for block
    v <- newSTRef vars
    mk <- flip unfoldrM (scope, atn, vars) $ \(ert, erat, ervars) -> if null ert then return Nothing else
            maybe (return $ Just (Nothing, (ert, erat, ervars))) (\y -> Just (Just (snd3 y), y) <$ writeSTRef v (thd3 y)) $ stmt ert erat ervars
    nvars <- readSTRef v
    return $ if any isNothing mk then Nothing else
        Just (tail $ dropWhile (TKReserved "}" /=) xs, ATNode (ATBlock (map fromJust mk)) ATEmpty ATEmpty, nvars)
stmt (TKReserved ";":xs) atn vars = Just (xs, atn, vars) -- for only ";"
stmt xs atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for stmt;
    TKReserved ";":ys -> Just (ys, erat, ervars)
    _ -> Nothing

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
expr = assign


-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
assign xs atn vars = flip (maybe Nothing) (equality xs atn vars) $ \(ert, erat, ervars) -> case ert of
    TKReserved "=":ys -> second3 (ATNode ATAssign erat) <$> assign ys erat ervars
    _ -> Just (ert, erat, ervars)


-- | `inners` is a general function for creating `equality`, `relational`, `add` and `term` in the following syntax (EBNF) of LL (1).
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
-- {\rm assign} &=& {\rm equality} \left("="\ {\rm assign}\right)?\label{eq:seventh}\tag{3}\\
-- {\rm equality} &=& {\rm relational}\ \left("=="\ {\rm relational}\ \mid\ "!="\ {\rm relational}\right)^\ast\label{eq:fifth}\tag{4}\\
-- {\rm relational} &=& {\rm add}\ \left("\lt"\ {\rm add}\mid\ "\lt ="\ {\rm add}\mid\ "\gt"\ {\rm add}\mid\ "\gt ="\ {\rm add}\right)^\ast\label{eq:sixth}\tag{5}\\
-- {\rm add} &=& {\rm term}\ \left("+"\ {\rm term}\ \mid\ "-"\ {\rm term}\right)^\ast\label{eq:first}\tag{6} \\
-- {\rm term} &=& {\rm factor}\ \left("\ast"\ {\rm factor}\ \mid\ "/"\ {\rm factor}\right)^\ast\label{eq:second}\tag{7} \\
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\label{eq:fourth}\tag{8} \\
-- {\rm factor} &=& {\rm num} \mid\ {\rm ident}\ \left({\rm "("\ ")"}\right)?\ \mid\ "(" {\rm expr} ")"\label{eq:third}\tag{9}
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
equality :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
relational = inners add [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
add :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
add = inners term [("+", ATAdd), ("-", ATSub)]

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
term = inners unary [("*", ATMul), ("/", ATDiv)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
unary (TKReserved "+":xs) at vars = factor xs at vars
unary (TKReserved "-":xs) at vars = second3 (ATNode ATSub (ATNode (ATNum 0) ATEmpty ATEmpty)) <$> factor xs at vars
unary xs at vars = factor xs at vars

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Eq i, Num i) => [Token i] -> ATree i -> [LVar i] -> Maybe ([Token i], ATree i, [LVar i])
factor [] atn vars = Just ([], atn, vars)
factor (TKReserved "(":xs) atn vars = flip (maybe Nothing) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for (expr)
    TKReserved ")":ys -> Just (ys, erat, ervars)
    _ -> Nothing
factor (TKNum n:xs) _ vars = Just (xs, ATNode (ATNum n) ATEmpty ATEmpty, vars) -- for numbers
factor (TKIdent v:TKReserved "(":TKReserved ")":xs) _ vars = Just (xs, ATNode (ATCallFunc (T.pack v) Nothing) ATEmpty ATEmpty, vars) -- for no arguments function call
{-
factor (TKIdent v:TKReserved "(":xs) _ vars = if succ (length cm) /= length vals then Nothing else -- for some arguments function call
    let nodes = map f vals in if any isNothing nodes then Nothing else
        Just (tail $ dropWhile (/=TKReserved ")") xs, ATNode (ATCallFunc (T.pack v) (Just $ map fromJust nodes)) ATEmpty ATEmpty, vars)
    where
        tw = takeWhile (/=TKReserved ")") xs
        cm = filter (==TKReserved ",") tw
        vals = filter (/=TKReserved ",") tw
        f x@(TKIdent _) = \(LVar _ o) -> ATNode (ATLVar o) ATEmpty ATEmpty <$> lookupLVar x vars
        f (TKNum n) = Just (ATNode (ATNum n) ATEmpty ATEmpty)
        f _ = Nothing
-}
factor (TKIdent v:xs) _ vars = maybe -- for variables
    (let lvars = LVar v (if null vars then 8 else offset (head vars) + 8):vars in Just (xs, ATNode (ATLVar $ offset $ head lvars) ATEmpty ATEmpty, lvars))
    (\(LVar _ o) -> Just (xs, ATNode (ATLVar o) ATEmpty ATEmpty, vars)) $
    lookupLVar (TKIdent v) vars
factor _ _ _ = Nothing

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: (Num i, Eq i) => [Token i] -> Maybe ([ATree i], Int)
parse = fmap (first (map fst) . second (length . snd . last) . dupe) . flip program []
