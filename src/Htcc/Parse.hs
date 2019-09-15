{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-|
Module      : Htcc.Parse
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The C languge parser and AST constructor
-}
module Htcc.Parse (
    -- * Abstract tree types
    LVar (..),
    ATKindFor (..),
    ATKind (..),
    ATree (..),
    isATForInit,
    isATForCond,
    isATForStmt,
    isATForIncr,
    fromATKindFor,
    -- * Recursive descent implementation functions
    program,
    funcDef,
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
    varNum
) where

import Data.Tuple.Extra (first, second, dupe, uncurry3, fst3, snd3, thd3)
import Data.List (find)
import Data.List.Split (linesBy)
import Data.Either (isLeft, lefts, rights)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (forM, zipWithM)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)

import Htcc.Utils (first3, second3, tshow)
import qualified Htcc.Token as HT

-- import Debug.Trace (trace)

-- | The local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        name :: String, -- ^ The name of local variable
        offset :: a -- ^ The offset value
    } deriving Show

{-# INLINE lookupLVar #-}
lookupLVar :: Num i => HT.Token i -> [LVar i] -> Maybe (LVar i)
lookupLVar (HT.TKIdent s) lvs = find (\(LVar ss _) -> ss == s) lvs
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
    | ATMod -- ^ modulus
    | ATAnd -- ^ bitwise and
    | ATOr -- ^ bitwise or
    | ATXor -- ^ bitwise xor
    | ATNot -- ^ bitwise not
    | ATShl -- ^ left shift \(\lt\lt\)
    | ATShr -- ^ right shift \(\gt\gt\)
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
    | ATDefFunc T.Text (Maybe [ATree a])
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ The function. It has a offset value and arguments (`Maybe`)
    | ATExprStmt -- The expression of a statement
    deriving Show

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode (ATKind a) (ATree a) (ATree a) -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

expectedMessage :: Show i => T.Text -> HT.TokenIdx i -> [HT.TokenIdx i] -> (T.Text, HT.TokenIdx i)
expectedMessage x t xs 
    | length xs > 1 = ("expected '" <> x <> "' token before '" <> tshow (snd (xs !! 1)) <> "'", head xs)
    | otherwise = ("expected '" <> x <> "' token", if null xs then t else head xs) 

{-# INLINE internalCE #-}
-- | the message of an internal compiler error
internalCE :: T.Text
internalCE = "internal compiler error: Please submit a bug report,\nwith preprocessed source if appropriate.\n See this repo: <URL> "

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> [LVar i] -> Either (T.Text, HT.TokenIdx i) [ATree i]
program [] _ = Right []
program xs vars = either Left (\(ys, btn, ars) -> (btn:) <$> program ys ars) $ funcDef {- stmt -} xs ATEmpty vars

-- | `funcDef` indicates X among the comments of `inners`.
funcDef :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
funcDef tk@(cur@(_, HT.TKIdent fname):(_, HT.TKReserved "("):_) atn vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" $ tail tk) $ 
    either (Left . ("Invalid function definition",)) $ \(fndec, st) -> case st of
        ((_, HT.TKReserved "{"):_) -> let args = linesBy ((==HT.TKReserved ",") . snd) $ init $ tail fndec in runST $ do
            v <- newSTRef (atn, vars)
            mk <- forM args $ \arg -> readSTRef v >>= either (return . Left) (\(_, y, z) -> Right y <$ writeSTRef v (y, z)) . uncurry (factor arg)
            if any isLeft mk then return $ Left $ head $ lefts mk else do
                (erat, ervar) <- readSTRef v
                return $ second3 (ATNode (ATDefFunc (T.pack fname) $ if null mk then Nothing else Just $ rights mk) ATEmpty) <$> stmt st erat ervar
        _ -> stmt tk atn vars
funcDef tk _ _ = Left ("Invalid function definition syntax", if null tk then (0, HT.TKEmpty) else head tk)

-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
stmt (cur@(_, HT.TKReturn):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @return@
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATReturn erat ATEmpty, ervars)
    ert' -> Left $ expectedMessage ";" cur ert'
stmt (cur@(_, HT.TKIf):(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @if@
    (_, HT.TKReserved ")"):ys -> flip (either Left) (stmt ys erat ervars) $ \x -> case second3 (ATNode ATIf erat) x of
        ((_, HT.TKElse):zs, eerat, eervars) -> second3 (ATNode ATElse eerat) <$> stmt zs eerat eervars -- for @else@
        zs -> Right zs
    ert' -> Left $ expectedMessage ")" cur ert'
stmt (cur@(_, HT.TKWhile):(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @while@
    (_, HT.TKReserved ")"):ys -> second3 (ATNode ATWhile erat) <$> stmt ys erat ervars
    ert' -> Left $ expectedMessage ")" cur ert'
stmt xxs@(cur@(_, HT.TKFor):(_, HT.TKReserved "("):_) atn vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" (tail xxs)) $ -- for @for@
    either (Left . ("expected ')' token. The subject iteration statement starts here:",)) $ \(forSt, ds) ->
        let fsect = linesBy ((== HT.TKReserved ";") . snd) (tail (init forSt)); stlen = length fsect in
            if stlen < 2 || stlen > 3 then 
                Left ("the iteration statement for must be `for (expression_opt; expression_opt; expression_opt) statement`. see section 6.8.5.", cur) else runST $ do
                    v <- newSTRef (atn, vars)
                    mk <- flip (`zipWithM` fsect) [ATForInit, ATForCond, ATForIncr] $ \fs at -> do
                        aw <- readSTRef v
                        either (return . Left) (\(_, ert, ervars) -> Right (at ert) <$ writeSTRef v (ert, ervars)) $ uncurry (expr fs) aw
                    if any isLeft mk then return $ Left $ head $ lefts mk else do
                        let jo = [m | (Right m) <- mk, case fromATKindFor m of ATEmpty -> False; _ -> True] 
                            mkk = maybe (ATForCond (ATNode (ATNum 1) ATEmpty ATEmpty) : jo) (const jo) $ find isATForCond jo
                        (anr, vsr) <- readSTRef v
                        case ds of 
                            ((_, HT.TKReserved ";"):ys) -> return $ Right (ys, ATNode (ATFor mkk) ATEmpty ATEmpty, vsr)
                            _ -> return $ second3 (flip (flip ATNode ATEmpty) ATEmpty . ATFor . (mkk ++) . (:[]) . ATForStmt) <$> stmt ds anr vsr
stmt xxs@(cur@(_, HT.TKReserved "{"):_) _ vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "{" "}" xxs) $ -- for compound statement
    either (Left . ("The statement is not closed",)) $ \(scope, ds) -> runST $ do
        v <- newSTRef vars
        mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
            ervars <- readSTRef v
            either (\err -> return $ Just (Left err, ert)) (\(ert', erat', ervars') -> Just (Right erat', ert') <$ writeSTRef v ervars') $ stmt ert ATEmpty ervars
        nvars <- readSTRef v
        return $ if any isLeft mk then Left $ head $ lefts mk else Right (ds, ATNode (ATBlock (rights mk)) ATEmpty ATEmpty, nvars)
stmt ((_, HT.TKReserved ";"):xs) atn vars = Right (xs, atn, vars) -- for only @;@
stmt xs atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for stmt;
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATExprStmt ATEmpty erat, ervars)
    ert' -> Left $ expectedMessage ";" (0, HT.TKEmpty) ert'

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
expr = assign

-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
assign xs atn vars = flip (either Left) (bitwiseOr xs atn vars) $ \(ert, erat, ervars) -> case ert of
    (_, HT.TKReserved "="):ys -> second3 (ATNode ATAssign erat) <$> assign ys erat ervars
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
-- {\rm unary} &=& \left("+"\ \mid\ "-"\right)?\ {\rm factor}\mid\ \left("!"\ \mid\ "\sim"\ \right)?\ {\rm unary}\label{eq:fourth}\tag{12} \\
-- {\rm factor} &=& {\rm num} \mid\ {\rm ident}\ \left({\rm "(" \left(expr\ \left(\left(","\ expr\right)^\ast\right)?\right)? ")"}\right)?\ \mid\ "(" {\rm expr} ")"\label{eq:third}\tag{13}
-- \end{eqnarray}
-- \]
inners :: ([HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])) -> [(String, ATKind i)] -> [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
inners _ _ [] atn vars = Right ([], atn, vars)
inners f cs xs atn vars = either Left (uncurry3 (inners' f cs)) $ f xs atn vars
    where
        inners' _ _ [] at ars = Right ([], at, ars)
        inners' g ds ys at ars = flip (maybe (Right (ys, at, ars))) (find (\(c, _) -> case snd (head ys) of HT.TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            either Left (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k at)) $ g (tail ys) at ars

-- | `bitwiseOr` indicates \(\eqref{eq:tenth}\) among the comments of `inners`.
bitwiseOr :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
bitwiseOr = inners bitwiseXor [("|", ATOr)]

-- | `bitwiseXor` indicates \(\eqref{eq:eleventh}\) amont the comments of `inners`.
bitwiseXor :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
bitwiseXor = inners bitwiseAnd [("^", ATXor)]

-- | `bitwiseAnd` indicates \(\eqref{eq:twelveth}\) among the comments of `inners`.
bitwiseAnd :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
bitwiseAnd = inners equality [("&", ATAnd)]

-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i)
-- > equality xs atn vars = flip (either Nothing) (relational xs atn vars) $ uncurry3 equality'
-- >     where
-- >         equality' ((_, HT.TKReserved "+"):ys) era ars = either Left (uncurry id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' ((_, HT.TKReserved "-"):ys) era ars = either Left (uncurry id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
-- >         equality' ert era ars = Right (ert, era, ars)
equality :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
relational = inners shift [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `shift` indicates \(\eqref{eq:thirteenth}\\) among the comments of `inners`.
shift :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
shift = inners add [("<<", ATShl), (">>", ATShr)]

-- | `add` indicates \(\eqref{eq:first}\) among the comments of `inners`.
add :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
add = inners term [("+", ATAdd), ("-", ATSub)]

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
term = inners unary [("*", ATMul), ("/", ATDiv), ("%", ATMod)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
unary ((_, HT.TKReserved "+"):xs) at vars = factor xs at vars
unary ((_, HT.TKReserved "-"):xs) at vars = second3 (ATNode ATSub (ATNode (ATNum 0) ATEmpty ATEmpty)) <$> factor xs at vars
unary ((_, HT.TKReserved "!"):xs) at vars = second3 (\x -> ATNode ATElse (ATNode ATIf (ATNode ATEQ x (ATNode (ATNum 0) ATEmpty ATEmpty)) (ATNode ATReturn (ATNode (ATNum 1) ATEmpty ATEmpty) ATEmpty)) (ATNode ATReturn (ATNode (ATNum 0) ATEmpty ATEmpty) ATEmpty)) <$> unary xs at vars
unary ((_, HT.TKReserved "~"):xs) at vars = second3 (flip (ATNode ATNot) ATEmpty) <$> unary xs at vars
unary xs at vars = factor xs at vars

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
factor [] atn vars = Right ([], atn, vars)
factor (cur@(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for (expr)
    (_, HT.TKReserved ")"):ys -> Right (ys, erat, ervars)
    ert' -> Left $ expectedMessage ")" cur ert'
factor ((_, HT.TKNum n):xs) _ vars = Right (xs, ATNode (ATNum n) ATEmpty ATEmpty, vars) -- for numbers
factor ((_, HT.TKIdent v):(_, HT.TKReserved "("):(_, HT.TKReserved ")"):xs) _ vars = Right (xs, ATNode (ATCallFunc (T.pack v) Nothing) ATEmpty ATEmpty, vars) -- for no arguments function call
{-
factor (cur@(i, HT.TKIdent v):(j, HT.TKReserved "("):xs) _ vars = let fsec = takeWhile ((/=HT.TKReserved ";") . snd) xs in -- for function call with some arguments
    flip (maybe (Left ("invalid function call syntax", cur))) (HT.takeExps $ [(i, HT.TKIdent v), (j, HT.TKReserved "(")] ++ fsec ++ [fsec !! length fsec]) $ \exps -> 
        if sum (map length exps) + pred (length exps) /= pred (length fsec) then Left ("invalid function call syntax", cur) else runST $ do
            mk <- newSTRef vars
            expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
            if any isLeft expl then return $ Left $ head $ lefts expl else 
                return $ Right (drop (length fsec) xs, ATNode (ATCallFunc (T.pack v) (Just $ rights expl)) ATEmpty ATEmpty, vars)
-}
factor (cur1@(_, HT.TKIdent v):cur2@(_, HT.TKReserved "("):xs) _ vars = flip (maybe $ Left (internalCE, cur1)) (HT.takeBrace "(" ")" (cur2:xs)) $
    either (Left . ("invalid function call",)) $ \(fsec, ds) -> flip (maybe $ Left ("invalid function call", cur1)) (HT.takeExps (cur1:fsec)) $ \exps -> runST $ do
        mk <- newSTRef vars
        expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
        if any isLeft expl then return $ Left $ head $ lefts expl else do
            vars' <- readSTRef mk
            return $ Right (ds, ATNode (ATCallFunc (T.pack v) (Just $ rights expl)) ATEmpty ATEmpty, vars')
factor ((_, HT.TKIdent v):xs) _ vars = maybe -- for variables
    (let lvars = LVar v (if null vars then 8 else offset (head vars) + 8):vars in Right (xs, ATNode (ATLVar $ offset $ head lvars) ATEmpty ATEmpty, lvars))
    (\(LVar _ o) -> Right (xs, ATNode (ATLVar o) ATEmpty ATEmpty, vars)) $
    lookupLVar (HT.TKIdent v) vars
factor ert _ _ = Left (if null ert then "unexpected token in program" else "unexpected token '" <> tshow (snd (head ert)) <> "' in program", 
    if null ert then (0, HT.TKEmpty) else head ert)

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: (Show i, Num i, Eq i, Integral i, Read i) => [HT.TokenIdx i] -> Either (T.Text, HT.TokenIdx i) [ATree i] -- ([ATree i], Int)
parse = flip program [] 

-- | `varNum` returns the number of variables per function.
varNum :: (Show i, Ord i) => ATree i -> Int
varNum (ATNode (ATDefFunc _ args) _ body) = S.size $ f body $ maybe S.empty (foldr (\(ATNode (ATLVar x) _ _) acc -> S.insert x acc) S.empty) args
    where
        f ATEmpty s = s
        f (ATNode (ATLVar x) l r) s = f l (S.insert x s) `S.union` f r (S.insert x s)
        f (ATNode (ATBlock xs) l r) s = let i =  foldr (S.union . (`f` s)) s xs in f l i `S.union` f r i
        f (ATNode (ATFor xs) l r) s = let i =  foldr (S.union . flip f s . fromATKindFor) S.empty xs in f l i `S.union` f r i
        f (ATNode _ l r) s = f l s `S.union` f r s
varNum _ = 0
