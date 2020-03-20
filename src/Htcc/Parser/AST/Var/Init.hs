{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}
{-|
Module      : Htcc.Parser.AST.Var.Init
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The C languge parser and AST constructor
-}
module Htcc.Parser.AST.Var.Init (
    Assign,
    validAssign,
    varInit
) where

import Prelude hiding (toInteger)
import Data.Bits (Bits)
import qualified Data.ByteString as B
import Data.Foldable (Foldable (..))
import Data.Tuple.Extra (first, second, snd3, dupe)
import Data.List (sortBy, isPrefixOf)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import qualified Data.Sequence as SQ
import qualified Data.Map.Strict as M
import Control.Conditional (ifM)
import Control.Monad (forM)
import Control.Monad.Fix (fix)
import Control.Monad.State (put, gets)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (evalStateT)
import Safe (headMay)

import Htcc.Utils ( 
    second3, fst4, snd4, thd4, fou4,
    dropSnd3,
    tshow,
    maybeToRight,
    swap)
import qualified Htcc.Tokenizer as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import Htcc.Parser.ConstructionData
import Htcc.Parser.AST.Core (ATKind (..), ATree (..), atUnary, atNumLit, atAssign, atExprStmt, atMemberAcc, Treealizable (..))
import Htcc.Parser.AST.Type (ASTConstruction, ASTState)
import Htcc.Parser.AST.DeduceKind
import Htcc.Parser.Utils
import Htcc.Utils.CompilationState (itemCWhen, isSatisfied)

{-# INLINE validAssign #-}
-- | Check for valid substitutions
validAssign :: Eq i => HT.TokenLC i -> ATree i -> Either (ASTError i) (ATree i)
validAssign errPlaceholder x@(ATNode _ t _ _) 
    | CT.toTypeKind t == CT.CTVoid = Left ("void value not ignored as it ought to be", errPlaceholder)
    | otherwise = Right x
validAssign errPlaceholder _ = Left ("Expected to assign", errPlaceholder)

desgNode :: (Num i, Ord i, Show i) => HT.TokenLC i -> ATree i -> [CT.Desg i]-> ConstructionData i -> Either (ASTError i) (ATree i)
desgNode ident rhs desg sc = fmap (atExprStmt . flip atAssign rhs) $ flip (`foldr` ntRightLVarTree) desg $ \idx acc -> case idx of
    CT.DesgIdx idx' -> do
        at <- acc
        nd <- ntRightInvalidInitList $ addKind at $ atNumLit idx'
        flip (atUnary ATDeref) nd <$> ntRightInvalidInitList (CT.deref (atype nd))
    CT.DesgMem mem -> atMemberAcc mem <$> acc
    where    
        ntRightInvalidInitList = maybeToRight ("invalid initializer-list", HT.emptyToken)
        ntRightLVarTree = treealize <$> maybeToRight (internalCE, HT.emptyToken) (lookupLVar (tshow $ snd ident) sc)

initZero :: (Num i, Ord i, Show i, Enum i) => CT.TypeKind i -> HT.TokenLC i -> [CT.Desg i] -> ConstructionData i -> Either (ASTError i) [ATree i]
initZero (CT.CTArray n t) ident desg sc = fmap concat $ forM [0..fromIntegral (pred n)] $ flip (initZero t ident) sc . (:desg) . CT.DesgIdx
initZero _ ident desg sc = (:[]) <$> desgNode ident (atNumLit 0) desg sc
 
-- | needs parameters for Assign
type Assign i = [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i

-- Designator initialization processing loop
-- Returns the consumed token list, the constructed tree, ConstructionData and the number of elements specified in designator
initLoop :: (Bits i, Integral i, Read i, Show i) => 
    Assign i -> CT.StorageClass i -> HT.TokenLC i -> [CT.Desg i] -> SQ.Seq (ATree i) -> HT.TokenLC i -> ASTState i ([HT.TokenLC i], SQ.Seq (ATree i), ConstructionData i, i)
initLoop callback t' ident desg ai c = do
    rs <- initLoop' ai
    itemCWhen const ((==HT.TKReserved "}") . snd) >>= maybe (lift $ Left ("expected '}' token for '{'", c)) (const $ retCur (snd4 rs) (fou4 rs))
    where
        isEnd = uncurry (||) . first (isPrefixOf [HT.TKReserved "}"]) . second (isPrefixOf [HT.TKReserved ",", HT.TKReserved "}"]) . dupe . map snd
                                
        retCur :: SQ.Seq (ATree i) -> i -> ASTState i ([HT.TokenLC i], SQ.Seq (ATree i), ConstructionData i, i)
        retCur ai' n = uncurry (,ai',,n) <$> gets swap
        
        initLoop' ai' = case CT.toTypeKind t' of 
            CT.CTArray _ _ -> ($ (0, ai')) . fix $ \f (!idx, rl) -> do
                rs <- uncurry (desgInit callback ident rl (CT.DesgIdx idx:desg) $ fromJust $ CT.deref t') <$> gets swap
                flip (either (lift . Left)) rs $ \rs' -> do
                    put (swap $ dropSnd3 rs')
                    ifM ((||) <$> isSatisfied isEnd <*> (isNothing <$> itemCWhen const ((==HT.TKReserved ",") . snd))) (retCur (snd3 rs') $ succ idx) $ f (succ idx, snd3 rs')
            CT.CTStruct mems -> ($ (M.elems mems, ai', 0)) . fix $ \f (mems', rl, len) -> if null mems' then retCur ai' len else do
                rs <- uncurry (desgInit callback ident rl (CT.DesgMem (head mems'):desg) $ CT.SCAuto $ CT.smType (head mems')) <$> gets swap
                flip (either (lift . Left)) rs $ \rs' -> do
                    put (swap $ dropSnd3 rs')
                    ifM ((||) <$> isSatisfied isEnd <*> (isNothing <$> itemCWhen const ((==HT.TKReserved ",") . snd))) (retCur (snd3 rs') $ succ len) $ f (tail mems', snd3 rs', succ len)
            _ -> lift $ Left (internalCE, HT.emptyToken)

-- For initializer-list.
-- For example, the declaration @int x[2][2] = { { 1, 2 }, { 3, 4 } };@ is converted to @x[2][2]; x[0][0] = 1; x[0][1] = 2; x[1][0] = 3; x[1][1] = 4;@.
desgInit :: (Bits i, Integral i, Read i, Show i) => 
    Assign i -> HT.TokenLC i -> SQ.Seq (ATree i) -> [CT.Desg i] -> CT.StorageClass i -> [HT.TokenLC i] -> ConstructionData i -> 
    Either (ASTError i) ([HT.TokenLC i], SQ.Seq (ATree i), ConstructionData i)
desgInit callback ident ai desg t' xs' scp
    -- initializer-string
    | CT.isArray t' && maybe False ((==CT.CTChar) . CT.toTypeKind) (CT.deref t') && maybe False (HT.isTKString . snd) (headMay xs') = if CT.isIncompleteArray t' then 
        case snd (head xs') of
            (HT.TKString s) -> let newt = arTypeFromLen (B.length s) in addLVar newt ident scp >>= desgInit callback ident ai desg newt xs' . snd
            _ -> Left (internalCE, HT.emptyToken) -- should not reach here
        else case (snd (head xs'), CT.toTypeKind t') of
            (HT.TKString s, CT.CTArray n _) -> let s' = s `B.append` B.pack (replicate (fromIntegral n - pred (B.length s)) $ toEnum 0) in 
                fmap ((tail xs',, if fromIntegral n < pred (B.length s) then pushWarn "initializer-string for char array is too long" (head xs') scp else scp) . 
                    (ai SQ.><) . SQ.fromList) $ mapM (flip id scp . uncurry (desgNode ident)) $ zipWith (flip (.) (++desg) . (,) . atNumLit . fromIntegral) (B.unpack s') $
                        sortBy (flip (.) reverse . compare . reverse) $ CT.accessibleIndices $ CT.toTypeKind t'
            _ -> Left (internalCE, HT.emptyToken) -- should not reach here
    -- Non-string initializer-list
    | CT.isArray t' = case xs' of -- incomplete dattara takeExps de kazeru
        -- Zero initialization
        (_, HT.TKReserved "{"):(_, HT.TKReserved "}"):ds -> fmap ((ds,, scp) . (ai SQ.><) . SQ.fromList) $ 
            mapM (flip (desgNode ident $ atNumLit 0) scp . (++desg)) $ CT.accessibleIndices $ CT.toTypeKind t'
        -- The specified initializer-list of initialization elements
        c@(_, HT.TKReserved "{"):ds 
            | CT.isIncompleteArray t' -> toComplete (c:ds) >>= \newt -> addLVar newt ident scp >>= desgInit callback ident ai desg newt xs' . snd
            | otherwise -> case CT.toTypeKind t' of
                CT.CTArray n bt -> do
                    rs <- evalStateT (initLoop callback t' ident desg ai c) (scp, ds)
                    zeroResult rs $ forM [fromIntegral (fou4 rs)..pred $ fromIntegral n] $ \idx -> initZero bt ident (CT.DesgIdx idx:desg) (thd4 rs)
                _ -> Left (internalCE, HT.emptyToken)
        _ ->  Left ("expected { initializer-list } or { initializer-list , }", if not (null xs') then head xs' else HT.emptyToken)
    -- struct initializer
    | CT.isCTStruct t' = case (xs', CT.toTypeKind t') of
        ((_, HT.TKReserved "{"):(_, HT.TKReserved "}"):ds, CT.CTStruct mems) -> fmap ((ds,,scp) . (ai SQ.><) . SQ.fromList . concat) $ forM (M.elems mems) $ \mem -> 
            initZero (CT.smType mem) ident (CT.DesgMem mem:desg) scp
        (c@(_, HT.TKReserved "{"):ds, CT.CTStruct mems) -> do
            rs <- evalStateT (initLoop callback t' ident desg ai c) (scp, ds)
            zeroResult rs $ forM (drop (fromIntegral $ fou4 rs) (M.elems mems)) $ \mem -> initZero (CT.smType mem) ident (CT.DesgMem mem:desg) (thd4 rs)
        _ ->  Left ("expected { initializer-list } or { initializer-list , }", if not (null xs') then head xs' else HT.emptyToken)
    -- For a element
    | otherwise = callback xs' ATEmpty scp >>= \(ds, at, scp''') -> (ds,,scp''') . (ai SQ.|>) <$> desgNode ident at desg scp'''
    where
        {-# INLINE zeroResult #-}
        zeroResult rs = fmap ((fst4 rs,,thd4 rs) . (ai SQ.><) . (snd4 rs SQ.><) . SQ.fromList . concat)

        {-# INLINE toComplete #-}
        toComplete ds' = (>>=) 
            (maybeToRight ("expected { initializer-list } or { initializer-list , }", if not (null xs') then head xs' else HT.emptyToken) (takeBrace "{" "}" ds')) $
                either (Left . ("expected { initializer-list } or { initializer-list , }",)) $ \(br, _) -> arTypeFromLen . length <$> 
                    maybeToRight (internalCE, HT.emptyToken) (takeExps $ [(HT.TokenLCNums 0 0, HT.TKReserved "(")] ++ init (tail br) ++ [(HT.TokenLCNums 0 0, HT.TKReserved ")")])

        {-# INLINE arTypeFromLen #-}
        arTypeFromLen len = snd (CT.dctorArray t') $ CT.mapTypeKind (CT.CTArray (fromIntegral len) . fromJust . CT.fromIncompleteArray) t'

varInit' :: (Read i, Show i, Integral i, Bits i) => Assign i -> CT.StorageClass i -> HT.TokenLC i -> [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
varInit' callback t ident xs lat scp'
    | CT.isArray t || CT.isCTStruct t = second3 (\st -> ATNode (ATBlock $ toList st) (CT.SCUndef CT.CTUndef) ATEmpty ATEmpty) <$> desgInit callback ident SQ.empty [] t xs scp'
    | otherwise = do
        (ert, erat, ervar) <- callback xs ATEmpty scp' 
        flip fmap (validAssign (HT.altEmptyToken ert) erat) $ \erat' -> (ert, atExprStmt (ATNode ATAssign (atype lat) lat erat'), ervar)

-- | Initializing local variables
varInit :: (Read i, Show i, Integral i, Bits i) => Assign i -> CT.StorageClass i -> HT.TokenLC i -> [HT.TokenLC i] -> ConstructionData i -> ASTConstruction i
varInit callback t ident token scp = addLVar (fromMaybe t $ incomplete t scp) ident scp >>= uncurry (varInit' callback t ident token)
