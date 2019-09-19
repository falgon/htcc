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

import Data.Tuple.Extra (second, uncurry3, snd3)
import Data.List (find)
import Data.List.Split (linesBy)
import Data.Either (isLeft, lefts, rights)
import Data.Maybe (isJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (forM, zipWithM)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)

import Htcc.Utils (first3, second3, tshow, fstNothingIdx)
import qualified Htcc.Token as HT
import Htcc.CRules.Types as CT

-- import Debug.Trace (trace)

-- | The local variable
data LVar a = LVar -- ^ The constructor of local variable
    {
        name :: String, -- ^ The name of local variable
        vtype :: CT.TypeKind, -- ^ The type of local variable
        offset :: a -- ^ The offset from RBP
    } deriving (Show, Eq)

{-# INLINE lookupLVar #-}
lookupLVar :: Num i => HT.Token i -> [LVar i] -> Maybe (LVar i)
lookupLVar (HT.TKIdent s) lvs = find (\(LVar ss _ _) -> ss == s) lvs
lookupLVar _ _ = Nothing

-- | Specially @for@ syntax tree type
data ATKindFor a = ATForkw -- ^ The @for@ keyword
    | ATForInit (ATree a) -- ^ The initial section of @for@ statement
    | ATForCond (ATree a) -- ^ The conditional section of @for@ statement
    | ATForIncr (ATree a) -- ^ The incremental section of @for@ statement
    | ATForStmt (ATree a) -- ^ The statement section of @for@ statement
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

-- | The syntax tree type. Let \(x,y\in\mathbb{N}\), Let \(p\) and \(q\) be pointers to variables \(a\) and \(b\), respectively (@p=&a,q=&b@).
data ATKind a = ATAdd -- ^ \(x+y\): @x + y@
    | ATAddPtr -- ^ Add operation for pointer \(p+x,x+p\): @p + x, x + p@
    | ATSub -- ^ \(x-y\): @x - y@
    | ATSubPtr -- ^ Sub operation for pointer \(p-x\): @p - x@
    | ATPtrDis -- ^ The distance of pointers \(p-q\): @p - q@
    | ATMul -- ^ \(x\times y\): @x * y@
    | ATDiv -- ^ \(x\div y\): @x / y@
    | ATMod -- ^ \(x\bmod y\): @x % y@
    | ATAnd -- ^ bitwise and: @x & y@
    | ATOr -- ^ bitwise or: @x | y@
    | ATXor -- ^ bitwise xor: @x ^ y@
    | ATNot -- ^ bitwise not: @~x@
    | ATShl -- ^ left shift: @x << y@
    | ATShr -- ^ right shift: @x >> y@
    | ATLT  -- ^ \(x\lt y\): @x < y@
    | ATLEQ -- ^ \(x\leq y\): @x <= y@
    | ATGT  -- ^ \(x\gt y\): @x > y@
    | ATGEQ -- ^ \(x\geq y\): @x >= y@
    | ATEQ  -- ^ \(x=y\): @x == y@
    | ATNEQ -- ^ \(x\not= y\): @x != y@
    | ATAddr -- ^ The addressing operator @&@: @&x@
    | ATDeref -- ^ The dereferencing operator @*@: @*p@
    | ATAssign -- ^ The assign operator: @x=y@
    | ATNum a -- ^ The number
    | ATReturn -- ^ The @return@ keyword
    | ATIf -- ^ The @if@ keyword
    | ATElse -- ^ The @else@ keyword
    | ATWhile -- ^ The @while@ keyword
    | ATFor [ATKindFor a] -- ^ The @for@ keyword
    | ATBlock [ATree a] -- ^ The compound statement
    | ATLVar CT.TypeKind a -- ^ The local variable. It has a type information (as `CT.TypeKinds`) and an offset value
    | ATDefFunc T.Text (Maybe [ATree a]) -- ^ The function definition
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ The function call. It has a offset value and arguments (`Maybe`)
    | ATExprStmt -- ^ The expression of a statement
    deriving Show

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode { 
    atkind :: ATKind a, -- ^ The kind of abstract tree
    atype :: CT.TypeKind, -- ^ The data type
    atL :: ATree a, -- ^ The left hand side abstract tree
    atR :: ATree a -- ^ The right hand side abstract tree
    } -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

-- | "expected" error message
expectedMessage :: Show i => T.Text -> HT.TokenIdx i -> [HT.TokenIdx i] -> (T.Text, HT.TokenIdx i)
expectedMessage x t xs 
    | length xs > 1 = ("expected '" <> x <> "' token before '" <> tshow (snd (xs !! 1)) <> "'", head xs)
    | otherwise = ("expected '" <> x <> "' token", if null xs then t else head xs) 

{-# INLINE internalCE #-}
-- | the message of an internal compiler error
internalCE :: T.Text
internalCE = "internal compiler error: Please submit a bug report,\nwith preprocessed source if appropriate.\n See this repo: <URL> "

-- | `addLVar` only returns a pair of AST nodes and a list of variables in `Just`, without adding them if `lookupLVar` has already registered the variables,
-- only if the given arguments are types and identifiers. If it has not been registered, 
-- it is newly registered and a list of AST nodes, variables is paired and wrapped in `Just`.
-- Otherwise, `Nothing` returned.
addLVar :: Num i => (CT.TypeKind, HT.TokenIdx i) -> [LVar i] -> Maybe (ATree i, [LVar i])
addLVar (t, (_, HT.TKIdent ident)) vars = flip (`maybe` (\(LVar _ t' o) -> Just (ATNode (ATLVar t' o) t' ATEmpty ATEmpty, vars))) (lookupLVar (HT.TKIdent ident) vars) $
    let lvars = LVar ident t (if null vars then 8 else offset (head vars) + 8):vars in 
        Just (ATNode (ATLVar (vtype $ head lvars) (offset $ head lvars)) t ATEmpty ATEmpty, lvars)
addLVar _  _ = Nothing

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> [LVar i] -> Either (T.Text, HT.TokenIdx i) [ATree i]
program [] _ = Right []
program xs vars = either Left (\(ys, btn, ars) -> (btn:) <$> program ys ars) $ funcDef xs ATEmpty vars

-- | `funcDef` parses function definitions.
funcDef :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
funcDef tks@((_, HT.TKType _):_) at va = flip (maybe $ Left ("ISO C forbids declaration with no type", head tks)) (HT.makeTypes tks) $ \(_, ds) -> funcDef' ds at va
    where
        checkErr ar f = let ar' = init $ tail ar in if not (null ar') && snd (head ar') == HT.TKReserved "," then Left ("unexpected ',' token", head ar') else
            let args = linesBy ((==HT.TKReserved ",") . snd) ar'; args' = map HT.makeTypes args in flip (maybe (f args')) (fstNothingIdx args') $ \idx -> let inv = head (args !! idx) in
                Left $ if HT.isTKIdent $ snd inv then (T.singleton '\'' <> tshow (snd inv) <> "' was not declared in this scope", inv) else 
                    ("invalid token '" <> tshow (snd inv) <> "' here", inv)
        funcDef' tk@(cur@(_, HT.TKIdent fname):(_, HT.TKReserved "("):_) atn vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" $ tail tk) $ 
            either (Left . ("invalid function definition",)) $ \(fndec, st) -> case st of
                ((_, HT.TKReserved "{"):_) -> checkErr fndec $ \args -> runST $ do
                    eri <- newSTRef Nothing
                    v <- newSTRef vars
                    mk <- flip unfoldrM args $ \args' -> if null args' then return Nothing else let (Just arg) = head args' in do
                        m <- addLVar (second head arg) <$> readSTRef v
                        flip (maybe (Nothing <$ writeSTRef eri (Just $ if null (snd arg) then ("invalid argument", head tks) else ("invalid argument", head $ snd arg)))) m $ \(vat, vars') ->
                            Just (Right vat, tail args') <$ writeSTRef v vars'
                    (>>=) (readSTRef eri) $ flip maybe (return . Left) $ 
                        fmap (second3 (flip (ATNode (ATDefFunc (T.pack fname) $ if null mk then Nothing else Just $ rights mk) CT.CTUndef) ATEmpty)) . stmt st atn <$> readSTRef v
                _ -> stmt tk atn vars
        funcDef' tk _ _ = Left ("invalid function definition syntax", if null tk then (0, HT.TKEmpty) else head tk)
funcDef tk _ _ = Left ("invalid function definition syntax", if null tk then (0, HT.TKEmpty) else head tk)

-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Show i, Eq i, Num i, Integral i, Read i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
stmt (cur@(_, HT.TKReturn):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @return@
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATReturn CT.CTUndef erat ATEmpty, ervars)
    ert' -> Left $ expectedMessage ";" cur ert'
stmt (cur@(_, HT.TKIf):(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @if@
    (_, HT.TKReserved ")"):ys -> flip (either Left) (stmt ys erat ervars) $ \x -> case second3 (ATNode ATIf CT.CTUndef erat) x of
        ((_, HT.TKElse):zs, eerat, eervars) -> second3 (ATNode ATElse CT.CTUndef eerat) <$> stmt zs eerat eervars -- for @else@
        zs -> Right zs
    ert' -> Left $ expectedMessage ")" cur ert'
stmt (cur@(_, HT.TKWhile):(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for @while@
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
                            if isATForInit (at ATEmpty) && not (null fs) && HT.isTKType (snd $ head fs) then 
                                uncurry (stmt $ fs ++ [(fst $ last fs, HT.TKReserved ";")]) aw else uncurry (expr fs) aw
                    if any isLeft mk then return $ Left $ head $ lefts mk else do
                        let jo = [m | (Right m) <- mk, case fromATKindFor m of ATEmpty -> False; _ -> True] 
                            mkk = maybe (ATForCond (ATNode (ATNum 1) CT.CTInt ATEmpty ATEmpty) : jo) (const jo) $ find isATForCond jo
                        (anr, vsr) <- readSTRef v
                        case ds of 
                            ((_, HT.TKReserved ";"):ys) -> return $ Right (ys, ATNode (ATFor mkk) CT.CTUndef ATEmpty ATEmpty, vsr)
                            _ -> return $ second3 (flip (flip (flip ATNode CT.CTUndef) ATEmpty) ATEmpty . ATFor . (mkk ++) . (:[]) . ATForStmt) <$> stmt ds anr vsr
stmt xxs@(cur@(_, HT.TKReserved "{"):_) _ vars = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "{" "}" xxs) $ -- for compound statement
    either (Left . ("the statement is not closed",)) $ \(scope, ds) -> runST $ do
        eri <- newSTRef Nothing
        v <- newSTRef vars
        mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
            ervars <- readSTRef v
            either (\err -> Nothing <$ writeSTRef eri (Just err)) (\(ert', erat', ervars') -> Just (Right erat', ert') <$ writeSTRef v ervars') $ stmt ert ATEmpty ervars
        (>>=) (readSTRef eri) $ flip maybe (return . Left) $ Right . (ds, ATNode (ATBlock (rights mk)) CT.CTUndef ATEmpty ATEmpty,) <$> readSTRef v
stmt tk@(cur1@(_, HT.TKType _):_) atn vars = flip (maybe (Left ("invalid type using", cur1))) (HT.makeTypes tk) $ \(t, ds) -> case ds of -- for a variable declaration
    cur2@(_, HT.TKIdent _):(_, HT.TKReserved ";"):ds' -> flip (maybe (Left (internalCE, cur2))) (addLVar (t, cur2) vars) $ \(lat, vars') -> Right (ds', lat, vars')
    cur2@(_, HT.TKIdent _):(_, HT.TKReserved "="):ds' -> flip (maybe (Left (internalCE, cur2))) (addLVar (t, cur2) vars) $ \(lat, vars') -> 
        flip (either Left) (expr ds' atn vars') $ \(ert, erat, ervar) -> case ert of
            (_, HT.TKReserved ";"):ds'' -> Right (ds'', ATNode ATExprStmt CT.CTUndef (ATNode ATAssign (atype lat) lat erat) ATEmpty, ervar)
            _ -> Left ("expected ';' token. The subject iteration statement start here:", cur1)
    ds' -> Left $ if null ds' then ("expected unqualified-id", cur1) else ("expected unqualified-id before '" <> tshow (snd (head ds)) <> T.singleton '\'', head ds')
stmt ((_, HT.TKReserved ";"):xs) atn vars = Right (xs, atn, vars) -- for only @;@
stmt xs atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for stmt;
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATExprStmt CT.CTUndef erat ATEmpty, ervars)
    ert' -> Left $ expectedMessage ";" (0, HT.TKEmpty) ert'

{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
expr = assign

-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
assign xs atn vars = flip (either Left) (bitwiseOr xs atn vars) $ \(ert, erat, ervars) -> case ert of
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
inners :: ([HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])) -> [(String, ATKind i)] -> [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
inners _ _ [] atn vars = Right ([], atn, vars)
inners f cs xs atn vars = either Left (uncurry3 (inners' f cs)) $ f xs atn vars
    where
        inners' _ _ [] at ars = Right ([], at, ars)
        inners' g ds ys at ars = flip (maybe (Right (ys, at, ars))) (find (\(c, _) -> case snd (head ys) of HT.TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            either Left (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k CT.CTInt at)) $ g (tail ys) at ars

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
-- > equality xs atn vars = flip (either Left) (relational xs atn vars) $ uncurry3 equality'
-- >     where
-- >         equality' ((_, HT.TKReserved "=="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' ((_, HT.TKReserved "!="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
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
add xs atn vars = flip (either Left) (term xs atn vars) $ uncurry3 add'
    where
        addAlloc lhs rhs
            | atype lhs == CT.CTInt && atype rhs == CT.CTInt = Just (ATAdd, CT.CTInt)
            | isJust (derefMaybe $ atype lhs) && atype rhs == CT.CTInt = Just (ATAddPtr, atype lhs)
            | atype lhs == CT.CTInt && isJust (derefMaybe $ atype rhs) = Just (ATAddPtr, atype rhs)
            | otherwise = Nothing
        subAlloc lhs rhs
            | atype lhs == CT.CTInt && atype rhs == CT.CTInt = Just (ATSub, CT.CTInt)
            | isJust (derefMaybe $ atype lhs) && atype rhs == CT.CTInt = Just (ATSubPtr, atype lhs)
            | isJust (derefMaybe $ atype lhs) && isJust (derefMaybe $ atype rhs) = Just (ATPtrDis, atype lhs)
            | otherwise = Nothing
        add' (cur@(_, HT.TKReserved "+"):ys) era ars = flip (either Left) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("Invalid operands", cur))) (addAlloc era $ snd3 zz) $ \(at, ctype) -> uncurry3 id $ first3 add' $ second3 (ATNode at ctype era) zz
        add' (cur@(_, HT.TKReserved "-"):ys) era ars = flip (either Left) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("Invalid operands", cur))) (subAlloc era $ snd3 zz) $ \(at, ctype) -> uncurry3 id $ first3 add' $ second3 (ATNode at ctype era) zz
        add' ert erat ars = Right (ert, erat, ars)

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
term = inners unary [("*", ATMul), ("/", ATDiv), ("%", ATMod)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
unary ((_, HT.TKReserved "+"):xs) at vars = factor xs at vars
unary ((_, HT.TKReserved "-"):xs) at vars = second3 (ATNode ATSub CT.CTInt (ATNode (ATNum 0) CT.CTInt ATEmpty ATEmpty)) <$> factor xs at vars
unary ((_, HT.TKReserved "!"):xs) at vars = second3 (\x -> ATNode ATElse CT.CTUndef (ATNode ATIf CT.CTUndef (ATNode ATEQ CT.CTInt x (ATNode (ATNum 0) CT.CTInt ATEmpty ATEmpty)) (ATNode ATReturn CT.CTUndef (ATNode (ATNum 1) CT.CTInt ATEmpty ATEmpty) ATEmpty)) (ATNode ATReturn CT.CTUndef (ATNode (ATNum 0) CT.CTInt ATEmpty ATEmpty) ATEmpty)) <$> unary xs at vars
unary ((_, HT.TKReserved "~"):xs) at vars = second3 (flip (ATNode ATNot CT.CTInt) ATEmpty) <$> unary xs at vars
unary ((_, HT.TKReserved "&"):xs) at vars = second3 (flip (ATNode ATAddr (CT.CTPtr CT.CTInt)) ATEmpty) <$> unary xs at vars
unary (cur@(_, HT.TKReserved "*"):xs) at vars = flip (either Left) (unary xs at vars) $ \(ert, erat, ervars) -> 
    flip (maybe $ Left ("invalid pointer dereference", cur)) (CT.derefMaybe $ atype erat) $ \t -> Right (ert, ATNode ATDeref t erat ATEmpty, ervars)
unary xs at vars = factor xs at vars

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Show i, Eq i, Read i, Integral i) => [HT.TokenIdx i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenIdx i) ([HT.TokenIdx i], ATree i, [LVar i])
factor [] atn vars = Right ([], atn, vars)
factor (cur@(_, HT.TKReserved "("):xs) atn vars = flip (either Left) (expr xs atn vars) $ \(ert, erat, ervars) -> case ert of -- for (expr)
    (_, HT.TKReserved ")"):ys -> Right (ys, erat, ervars)
    ert' -> Left $ expectedMessage ")" cur ert'
factor ((_, HT.TKNum n):xs) _ vars = Right (xs, ATNode (ATNum n) CT.CTInt ATEmpty ATEmpty, vars) -- for numbers
factor ((_, HT.TKIdent v):(_, HT.TKReserved "("):(_, HT.TKReserved ")"):xs) _ vars = Right (xs, ATNode (ATCallFunc (T.pack v) Nothing) CT.CTInt ATEmpty ATEmpty, vars) -- for no arguments function call
factor (cur1@(_, HT.TKIdent v):cur2@(_, HT.TKReserved "("):xs) _ vars = flip (maybe $ Left (internalCE, cur1)) (HT.takeBrace "(" ")" (cur2:xs)) $
    either (Left . ("invalid function call",)) $ \(fsec, ds) -> flip (maybe $ Left ("invalid function call", cur1)) (HT.takeExps (cur1:fsec)) $ \exps -> runST $ do
        mk <- newSTRef vars
        expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
        if any isLeft expl then return $ Left $ head $ lefts expl else do
            vars' <- readSTRef mk
            return $ Right (ds, ATNode (ATCallFunc (T.pack v) (Just $ rights expl)) CT.CTInt ATEmpty ATEmpty, vars')
factor (cur@(_, HT.TKIdent v):xs) _ vars = maybe (Left ("undefined variable", cur)) (\(LVar _ t o) -> Right (xs, ATNode (ATLVar t o) t ATEmpty ATEmpty, vars)) $ -- for declared variables
    lookupLVar (HT.TKIdent v) vars -- if the variable is not declared, it returns error wrapped with `Left`
factor ert _ _ = Left (if null ert then "unexpected token in program" else "unexpected token '" <> tshow (snd (head ert)) <> "' in program", 
    if null ert then (0, HT.TKEmpty) else head ert)

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `Nothing` is returned.
parse :: (Show i, Num i, Eq i, Integral i, Read i) => [HT.TokenIdx i] -> Either (T.Text, HT.TokenIdx i) [ATree i]
parse = flip program [] 

-- | `varNum` returns the number of variables per function.
varNum :: (Show i, Ord i) => ATree i -> Int
varNum (ATNode (ATDefFunc _ args) _ body _) = S.size $ f body $ maybe S.empty (foldr (\(ATNode (ATLVar _ x) _ _ _) acc -> S.insert x acc) S.empty) args
    where
        f ATEmpty s = s
        f (ATNode (ATLVar _ x) _ l r) s = f l (S.insert x s) `S.union` f r (S.insert x s)
        f (ATNode (ATBlock xs) _ l r) s = let i =  foldr (S.union . (`f` s)) s xs in f l i `S.union` f r i
        f (ATNode (ATFor xs) _ l r) s = let i =  foldr (S.union . flip f s . fromATKindFor) S.empty xs in f l i `S.union` f r i
        f (ATNode _ _ l r) s = f l s `S.union` f r s
varNum _ = 0
