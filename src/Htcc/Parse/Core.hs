{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase, BangPatterns #-}
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
    stackSize,
    defTypedef
) where

import Prelude hiding (toInteger)
import Data.Bits hiding (shift)
import qualified Data.ByteString as B
import Data.Tuple.Extra (first, second, uncurry3, snd3, dupe)
import Data.List (find, foldl')
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

import Htcc.Utils (first3, second3, tshow, toNatural, toInteger, mapEither, dropThd, dropThd4)
import qualified Htcc.Token as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parse.AST
import qualified Htcc.Parse.Scope.Var as PV
import Htcc.Parse.Scope
import Htcc.Parse.Utils

-- | Perform type definition from token string starting from @typedef@ token
defTypedef :: (Integral i, Show i, Read i) => [(HT.TokenLCNums i, HT.Token i)] -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree a, Scoped i)
defTypedef ((_, HT.TKTypedef):xs) !scp = case HT.takeType xs scp of
    Left er -> Left er
    Right (ty, Just ident, ds, scp') -> case ds of
        (_, HT.TKReserved ";"):ds' -> (ds', ATEmpty,) <$> addTypedef ty ident scp'
        _ -> Left ("expected ';' token after '" <> tshow (snd ident) <> "'", ident)
    Right (_, Nothing, ds, scp') -> case ds of
        (_, HT.TKReserved ";"):ds' -> Right (ds', ATEmpty, scp')
        _ -> Left $ if not (null ds) then ("expected ';' token after '" <> tshow (snd $ head ds) <> "'", head ds) else ("expected ';' token", (HT.TokenLCNums 0 0, HT.TKEmpty))
defTypedef _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

-- | `program` indicates \(\eqref{eq:eigth}\) among the comments of `inners`.
program :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> Scoped i -> Either (T.Text, HT.TokenLC i) ([ATree i], Scoped i)
program [] !scp = Right ([], scp)
program xs !scp = either Left (\(ys, btn, !scp') -> first (btn:) <$> program ys scp') $ globalDef xs ATEmpty scp

-- | `globalDef` parses global definitions (include functions and global variables)
globalDef :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
globalDef xs@((_, HT.TKTypedef):(_, HT.TKType _):_) _ sc = defTypedef xs sc -- for global @typedef@ with type
globalDef xs@((_, HT.TKTypedef):(_, HT.TKStruct):_) _ sc = defTypedef xs sc -- for global @typedef@ with struct 
globalDef xs@((_, HT.TKTypedef):(_, HT.TKIdent _):_) _ sc = defTypedef xs sc -- for global @typedef@ with type defined by @typedef@
globalDef tks at !va = (>>=) (HT.takeType tks va) $ \case
    (_, Nothing, (_, HT.TKReserved ";"):ds', scp) -> Right (ds', ATEmpty, scp) -- e.g., @int;@ is legal in C11 (See N1570/section 6.7 Declarations)
    (funcType, Just (cur@(_, HT.TKIdent fname)), tk@((_, HT.TKReserved "("):_), !sc) -> let scp = resetLocal sc in -- for a function declaration or definition
        flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" $ tail (cur:tk)) $
            either (Left . ("invalid function declaration/definition",)) $ \(fndec, st) -> case st of
                ((_, HT.TKReserved ";"):ds'') -> addFunction False funcType cur scp >>= globalDef ds'' at -- for a function declaration
                ((_, HT.TKReserved "{"):_) -> (>>=) (addFunction True funcType cur scp) $ \scp' -> checkErr fndec scp' $ \args -> runST $ do -- for a function definition
                    eri <- newSTRef Nothing
                    v <- newSTRef scp'
                    mk <- flip unfoldrM args $ \args' -> if null args' then return Nothing else let arg = head args' in do
                        m <- uncurry addLVar (second fromJust $ dropThd $ dropThd4 $ arg) <$> readSTRef v
                        flip (either ((<$) Nothing . writeSTRef eri . Just)) m $ \(vat, scp'') -> Just (vat, tail args') <$ writeSTRef v scp''
                    (>>=) (readSTRef eri) $ flip maybe (return . Left) $
                        fmap (second3 (flip (ATNode (ATDefFunc fname $ if null mk then Nothing else Just mk) CT.CTUndef) ATEmpty)) . stmt st at <$> readSTRef v
                _ -> stmt tk at scp
    (ty, Just (cur@(_, HT.TKIdent _)), xs, !scp) -> case xs of -- for global variables -- TODO: support initialize by global variables
        (_, HT.TKReserved ";"):ds -> flip fmap (addGVar ty cur scp) $ \(_, scp') -> (ds, ATEmpty, scp')
        _ -> Left ("expected ';' token after '" <> tshow (snd cur) <> "' token", cur)
    _ -> Left ("invalid definition of global identifier", if null tks then (HT.TokenLCNums 0 0, HT.TKEmpty) else head tks)
    where
        checkErr ar !scp' f = let ar' = init $ tail ar in if not (null ar') && snd (head ar') == HT.TKReserved "," then Left ("unexpected ',' token", head ar') else
            let args = linesBy ((==HT.TKReserved ",") . snd) ar' in mapEither (`HT.takeType` scp') args >>= f


-- | `stmt` indicates \(\eqref{eq:nineth}\) among the comments of `inners`.
stmt :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
stmt (cur@(_, HT.TKReturn):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @return@
    (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATReturn CT.CTUndef erat ATEmpty, erscp)
    ert' -> Left $ expectedMessage ";" cur ert'
stmt (cur@(_, HT.TKIf):(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @if@
    (_, HT.TKReserved ")"):ys -> (>>=) (stmt ys erat erscp) $ \x -> case second3 (ATNode ATIf CT.CTUndef erat) x of
        ((_, HT.TKElse):zs, eerat, eerscp) -> second3 (ATNode ATElse CT.CTUndef eerat) <$> stmt zs eerat eerscp -- for @else@
        zs -> Right zs
    ert' -> Left $ expectedMessage ")" cur ert'
stmt (cur@(_, HT.TKWhile):(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for @while@
    (_, HT.TKReserved ")"):ys -> second3 (ATNode ATWhile CT.CTUndef erat) <$> stmt ys erat erscp
    ert' -> Left $ expectedMessage ")" cur ert'
stmt xxs@(cur@(_, HT.TKFor):(_, HT.TKReserved "("):_) atn !scp = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "(" ")" (tail xxs)) $ -- for @for@
    either (Left . ("expected ')' token. The subject iteration statement starts here:",)) $ \(forSt, ds) ->
        let fsect = linesBy ((== HT.TKReserved ";") . snd) (tail (init forSt)); stlen = length fsect in
            if stlen < 2 || stlen > 3 then 
                Left ("the iteration statement for must be `for (expression_opt; expression_opt; expression_opt) statement`. see section 6.8.5.", cur) else runST $ do
                    v <- newSTRef (atn, scp)
                    mk <- flip (`zipWithM` fsect) [ATForInit, ATForCond, ATForIncr] $ \fs at -> do
                        aw <- readSTRef v
                        either (return . Left) (\(_, ert, erscp) -> Right (at ert) <$ writeSTRef v (ert, erscp)) $ 
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
stmt xxs@(cur@(_, HT.TKReserved "{"):_) _ !scp = flip (maybe $ Left (internalCE, cur)) (HT.takeBrace "{" "}" xxs) $ -- for compound statement
    either (Left . ("the compound statement is not closed",)) $ \(scope, ds) -> runST $ do
        eri <- newSTRef Nothing
        v <- newSTRef $ succNest scp
        mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
            erscp <- readSTRef v
            either (\err -> Nothing <$ writeSTRef eri (Just err)) (\(ert', erat', erscp') -> Just (erat', ert') <$ writeSTRef v erscp') $ stmt ert ATEmpty erscp
        (>>=) (readSTRef eri) $ flip maybe (return . Left) $ Right . (ds, ATNode (ATBlock mk) CT.CTUndef ATEmpty ATEmpty,) . fallBack scp <$> readSTRef v
stmt ((_, HT.TKReserved ";"):xs) atn !scp = Right (xs, atn, scp) -- for only @;@
stmt xs@((_, HT.TKTypedef):(_, HT.TKType _):_) _ scp = defTypedef xs scp -- for definition of @typedef@
stmt xs@((_, HT.TKTypedef):(_, HT.TKStruct):_) _ scp = defTypedef xs scp -- for definition of @typedef@
stmt xs@((_, HT.TKTypedef):(_, HT.TKIdent _):_) _ scp = defTypedef xs scp -- for global @typedef@ with type defined by @typedef@
stmt tk atn !scp
    | not (null tk) && (HT.isTKType (snd $ head tk) || HT.isTKStruct (snd $ head tk)) = HT.takeType tk scp >>= varDecl -- for a local variable declaration
    | not (null tk) && HT.isTKIdent (snd $ head tk) && isJust (lookupTypedef (T.pack $ show $ snd $ head tk) scp) = HT.takeType tk scp >>= varDecl -- for a local variable declaration with @typedef@
    | otherwise = (>>=) (expr tk atn scp) $ \(ert, erat, erscp) -> case ert of -- for stmt;
        (_, HT.TKReserved ";"):ys -> Right (ys, ATNode ATExprStmt CT.CTUndef erat ATEmpty, erscp)
        ert' -> Left $ expectedMessage ";" (HT.TokenLCNums 0 0, HT.TKEmpty) ert'
    where
        varDecl (_, Nothing, ((_, HT.TKReserved ";"):ds), scp') = Right (ds, ATEmpty, scp')
        varDecl (t, (Just ident), ((_, HT.TKReserved ";"):ds), scp') = (>>=) (addLVar t ident scp') $ \(lat, scp'') -> Right (ds, ATNode (ATNull lat) CT.CTUndef ATEmpty ATEmpty, scp'')
        varDecl (t, (Just ident), ((_, HT.TKReserved "="):ds), scp') = (>>=) (addLVar t ident scp') $ \(lat, scp'') -> (>>=) (expr ds atn scp'') $ \(ert, erat, ervar) -> case ert of
            (_, HT.TKReserved ";"):ds' -> Right (ds', ATNode ATExprStmt CT.CTUndef (ATNode ATAssign (atype lat) lat erat) ATEmpty, ervar)
            _ -> Left ("expected ';' token. The subject iteration statement start here:", head tk)
        varDecl (_, _, ds, _) = Left $ if null ds then ("expected unqualified-id", head tk) else ("expected unqualified-id before '" <> tshow (snd (head ds)) <> T.singleton '\'', head ds)


{-# INLINE expr #-}
-- | `expr` is equivalent to `equality`.
expr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
expr = assign

-- | `assign` indicates \(\eqref{eq:seventh}\) among the comments of `inners`.
assign :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
assign xs atn scp = (>>=) (bitwiseOr xs atn scp) $ \(ert, erat, erscp) -> case ert of
    (_, HT.TKReserved "="):ys -> second3 (ATNode ATAssign (atype erat) erat) <$> assign ys erat erscp
    _ -> Right (ert, erat, erscp)

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
inners :: ([HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)) -> [(T.Text, ATKind i)] -> [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
inners _ _ [] atn scp = Right ([], atn, scp)
inners f cs xs atn scp = either Left (uncurry3 (inners' f cs)) $ f xs atn scp
    where
        inners' _ _ [] at ars = Right ([], at, ars)
        inners' g ds ys at ars = flip (maybe (Right (ys, at, ars))) (find (\(c, _) -> case snd (head ys) of HT.TKReserved cc -> cc == c; _ -> False) ds) $ \(_, k) -> 
            either Left (uncurry3 id . first3 (inners' f cs) . second3 (ATNode k CT.CTInt at)) $ g (tail ys) at ars

-- | `bitwiseOr` indicates \(\eqref{eq:tenth}\) among the comments of `inners`.
bitwiseOr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
bitwiseOr = inners bitwiseXor [("|", ATOr)]

-- | `bitwiseXor` indicates \(\eqref{eq:eleventh}\) amont the comments of `inners`.
bitwiseXor :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
bitwiseXor = inners bitwiseAnd [("^", ATXor)]

-- | `bitwiseAnd` indicates \(\eqref{eq:twelveth}\) among the comments of `inners`.
bitwiseAnd :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
bitwiseAnd = inners equality [("&", ATAnd)]

-- | `equality` indicates \(\eqref{eq:fifth}\) among the comments of `inners`.
-- This is equivalent to the following code:
--
--
-- > equality ::  [HT.TokenLC i] -> ATree i -> [LVar i] -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i)
-- > equality xs atn scp = (>>=) (relational xs atn scp) $ uncurry3 equality'
-- >     where
-- >         equality' ((_, HT.TKReserved "=="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATEQ era)) $ relational ys era ars
-- >         equality' ((_, HT.TKReserved "!="):ys) era ars = either Left (uncurry3 id . first3 equality' . second3 (ATNode ATNEQ era)) $ relational ys era ars
-- >         equality' ert era ars = Right (ert, era, ars)
equality :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
equality = inners relational [("==", ATEQ), ("!=", ATNEQ)]

-- | `relational` indicates \(\eqref{eq:sixth}\) among the comments of `inners`.
relational :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
relational = inners shift [("<", ATLT), ("<=", ATLEQ), (">", ATGT), (">=", ATGEQ)]

-- | `shift` indicates \(\eqref{eq:thirteenth}\\) among the comments of `inners`.
shift :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
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
add :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
add xs atn scp = (>>=) (term xs atn scp) $ uncurry3 add'
    where
        add' (cur@(_, HT.TKReserved "+"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("invalid operands", cur))) (addKind era $ snd3 zz) $ \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' (cur@(_, HT.TKReserved "-"):ys) era ars = (>>=) (term ys era ars) $ \zz -> 
            flip (maybe (Left ("invalid operands", cur))) (subKind era $ snd3 zz) $ \nat -> uncurry3 id $ first3 add' $ second3 (const nat) zz
        add' ert erat ars = Right (ert, erat, ars)

-- | `term` indicates \(\eqref{eq:second}\) amont the comments of `inners`.
term ::  (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
term = inners unary [("*", ATMul), ("/", ATDiv), ("%", ATMod)]

-- | `unary` indicates \(\eqref{eq:fourth}\) amount the comments of `inners`.
unary :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
unary ((_, HT.TKReserved "+"):xs) at scp = factor xs at scp
unary ((_, HT.TKReserved "-"):xs) at scp = second3 (ATNode ATSub CT.CTInt (ATNode (ATNum 0) CT.CTInt ATEmpty ATEmpty)) <$> factor xs at scp
unary ((_, HT.TKReserved "!"):xs) at scp = second3 (flip (ATNode ATNot CT.CTInt) ATEmpty) <$> unary xs at scp
unary ((_, HT.TKReserved "~"):xs) at scp = second3 (flip (ATNode ATBitNot CT.CTInt) ATEmpty) <$> unary xs at scp
unary ((_, HT.TKReserved "&"):xs) at scp = second3 (\x -> (ATNode ATAddr $ CT.CTPtr $ if CT.isCTArray (atype x) then fromJust $ CT.derefMaybe (atype x) else atype x) x ATEmpty) <$> unary xs at scp
unary (cur@(_, HT.TKReserved "*"):xs) at !scp = (>>=) (unary xs at scp) $ \(ert, erat, erscp) -> 
    flip (maybe $ Left ("invalid pointer dereference", cur)) (CT.derefMaybe $ atype erat) $ \t -> Right (ert, ATNode ATDeref t erat ATEmpty, erscp)
unary xs at scp = either Left (uncurry3 f) $ factor xs at scp
    where
        f (cur@(_, HT.TKReserved "["):xs') erat !erscp = (>>=) (expr xs' erat erscp) $ \(ert', erat', erscp') -> case ert' of
            (_, HT.TKReserved "]"):xs'' -> flip (maybe $ Left ("invalid operands", cur)) (addKind erat erat') $ \erat'' ->
                flip (maybe $ Left ("subscripted value is neither array nor pointer nor vector", if null xs then (HT.TokenLCNums 0 0, HT.TKEmpty) else head xs)) 
                    (CT.derefMaybe $ atype erat'') $ \t -> f xs'' (ATNode ATDeref t erat'' ATEmpty) erscp'
            _ -> Left $ if null ert' then ("expected expression after '[' token", cur) else ("expected expression before '" <> tshow (snd (head ert')) <> "' token", head ert')
        f (cur@(_, HT.TKReserved "."):xs') erat !erscp 
            | CT.isCTStruct (atype erat) = if null xs' then Left ("expected identifier at end of input", cur) else case head xs' of 
                (_, HT.TKIdent ident) -> flip (maybe (Left ("no such member", cur))) (CT.lookupMember ident (atype erat)) $ \mem ->
                    f (tail xs') (ATNode (ATMemberAcc mem) (CT.smType mem) erat ATEmpty) erscp
                _ -> Left ("expected identifier after '.' token", cur)
            | otherwise = Left ("request for a member in something not a structure or union", cur)
        f (cur@(_, HT.TKReserved "->"):xs') erat !erscp
            | maybe False CT.isCTStruct $ CT.derefMaybe (atype erat) = if null xs' then Left ("expected identifier at end of input", cur) else case head xs' of
                (_, HT.TKIdent ident) -> flip (maybe (Left ("no such member", cur))) (CT.lookupMember ident (fromJust $ CT.derefMaybe $ atype erat)) $ \mem ->
                    f (tail xs') (ATNode (ATMemberAcc mem) (CT.smType mem) (ATNode ATDeref (CT.smType mem) erat ATEmpty) ATEmpty) erscp
                _ -> Left ("expected identifier after '->' token", cur)
            | otherwise = Left ("invalid type argument of '->'" <> if CT.isCTUndef (atype erat) then "" else " (have '" <> tshow (atype erat) <> "')", cur)
        f ert erat !erscp = Right (ert, erat, erscp)

-- | `factor` indicates \(\eqref{eq:third}\) amount the comments of `inners`.
factor :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> Scoped i -> Either (T.Text, HT.TokenLC i) ([HT.TokenLC i], ATree i, Scoped i)
factor [] atn !scp = Right ([], atn, scp)
factor ((_, HT.TKReserved "("):xs@((_, HT.TKReserved "{"):_)) _ !scp = flip (maybe $ Left (internalCE, head xs)) (HT.takeBrace "{" "}" xs) $ -- for statement expression (GNU extension: <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>)
    either (Left . ("the statement expression is not closed",)) $ \(scope, ds) -> case ds of
        (_, HT.TKReserved ")"):ds' -> runST $ do
            eri <- newSTRef Nothing
            v <- newSTRef $ succNest scp
            lastA <- newSTRef ATEmpty 
            mk <- flip unfoldrM (init $ tail scope) $ \ert -> if null ert then return Nothing else do
                erscp <- readSTRef v
                flip (either $ \err -> Nothing <$ writeSTRef eri (Just err)) (stmt ert ATEmpty erscp) $ \(ert', erat', erscp') -> 
                    Just (erat', ert') <$ (writeSTRef v erscp' >> when (case erat' of ATEmpty -> False; _ -> True) (writeSTRef lastA erat'))
            (>>=) (readSTRef eri) $ flip maybe (return . Left) $ do
                v' <- readSTRef v
                flip fmap (readSTRef lastA) $ \case
                        (ATNode ATExprStmt _ lhs _) -> Right (ds', ATNode (ATStmtExpr (init mk ++ [lhs])) (atype lhs) ATEmpty ATEmpty, fallBack scp v')
                        _ -> Left ("void value not ignored as it ought to be. the statement expression starts here:", head xs)
        _ -> Left $ if null scope then ("expected ')' token. the statement expression starts here: ", head xs) else
            ("expected ')' token after '" <> tshow (snd $ last scope) <> "' token", last scope)
factor (cur@(_, HT.TKReserved "("):xs) atn !scp = (>>=) (expr xs atn scp) $ \(ert, erat, erscp) -> case ert of -- for (expr)
    (_, HT.TKReserved ")"):ys -> Right (ys, erat, erscp)
    ert' -> Left $ expectedMessage ")" cur ert'
factor ((_, HT.TKNum n):xs) _ !scp = Right (xs, ATNode (ATNum n) CT.CTLong ATEmpty ATEmpty, scp) -- for numbers
factor (cur@(_, HT.TKIdent v):(_, HT.TKReserved "("):(_, HT.TKReserved ")"):xs) _ !scp = flip (maybe (Left ("The function is not declared", cur))) (lookupFunction v scp) $ 
    const $ Right (xs, ATNode (ATCallFunc v Nothing) CT.CTInt ATEmpty ATEmpty, scp) -- for no arguments function call
factor (cur1@(_, HT.TKIdent v):cur2@(_, HT.TKReserved "("):xs) _ scp = flip (maybe $ Left (internalCE, cur1)) (HT.takeBrace "(" ")" (cur2:xs)) $ -- for some argumets function call
    either (Left . ("invalid function call",)) $ \(fsec, ds) -> flip (maybe (Left ("The function is not declared", cur1))) (lookupFunction v scp) $ 
        const $ flip (maybe $ Left ("invalid function call", cur1)) (HT.takeExps (cur1:fsec)) $ \exps -> runST $ do
            mk <- newSTRef scp
            expl <- forM exps $ \etk -> readSTRef mk >>= either (return . Left) (\(_, erat, ervar) -> Right erat <$ writeSTRef mk ervar) . expr etk ATEmpty
            if any isLeft expl then return $ Left $ head $ lefts expl else do
                scp' <- readSTRef mk
                return $ Right (ds, ATNode (ATCallFunc v (Just $ rights expl)) CT.CTInt ATEmpty ATEmpty, scp')
factor ((_, HT.TKSizeof):xs) atn !scp = second3 (\x -> ATNode (ATNum (fromIntegral $ CT.sizeof $ atype x)) CT.CTInt ATEmpty ATEmpty) <$> unary xs atn scp -- for `sizeof` -- TODO: the type of sizeof must be @size_t@
factor (cur@(_, HT.TKAlignof):xs) atn !scp = (>>=) (unary xs atn scp) $ \(ert, erat, erscp) -> 
    if CT.isCTUndef (atype erat) then Left ("_Alignof must be an expression or type", cur) else Right (ert, ATNode (ATNum (fromIntegral $ CT.alignof $ atype erat)) CT.CTInt ATEmpty ATEmpty, erscp) -- Note: Using alignof for expressions is a non-standard feature of C11
factor (cur@(_, HT.TKString slit):xs) _ !scp = uncurry (xs,,) <$> addLiteral (CT.CTArray (fromIntegral $ B.length slit) CT.CTChar) cur scp -- for literals
factor (cur@(_, HT.TKIdent ident):xs) _ !scp = flip (maybe (Left ("undefined variable", cur))) (lookupVar ident scp) $ \case -- if the variable is not declared, it returns error wrapped with `Left`
    Right (PV.LVar t o _) -> Right (xs, ATNode (ATLVar t o) t ATEmpty ATEmpty, scp) -- for declared local variable
    Left (PV.GVar t) -> Right (xs, ATNode (ATGVar t ident) t ATEmpty ATEmpty, scp) -- for declared global variable
factor ert _ _ = Left (if null ert then "unexpected token in program" else "unexpected token '" <> tshow (snd (head ert)) <> "' in program", if null ert then (HT.TokenLCNums 0 0, HT.TKEmpty) else head ert)

{-# INLINE parse #-}
-- | Constructs the abstract syntax tree based on the list of token strings.
-- if construction fails, `parse` returns the error message and the token at the error location.
-- Otherwise, `parse` returns a list of abstract syntax trees, a set of global variables, and a list of literals.
parse :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> Either (T.Text, HT.TokenLC i) ([ATree i], M.Map T.Text PV.GVar, [PV.Literal])
parse = fmap (\(ast, sc) -> (ast, PV.globals $ vars sc, PV.literals $ vars sc)) . flip program initScope

-- | `stackSize` returns the stack size of variable per function.
stackSize :: (Show i, Integral i) => ATree i -> Natural
stackSize (ATNode (ATDefFunc _ args) _ body _) = let ms = f body $ maybe S.empty (foldr (\(ATNode (ATLVar t x) _ _ _) acc -> S.insert (t, x) acc) S.empty) args in
    if S.size ms == 1 then toNatural $ flip CT.alignas 8 $ toInteger $ CT.sizeof $ fst $ head (S.toList ms) else toNatural $ flip CT.alignas 8 $ uncurry (+) $
        first (toInteger . CT.sizeof . fst) $ second (fromIntegral . snd) $ dupe $ foldl' (\acc x -> if snd acc < snd x then x else acc) (CT.CTUndef, 0) $ S.toList ms
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

