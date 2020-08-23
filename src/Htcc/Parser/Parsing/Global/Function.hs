{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase, BangPatterns, ScopedTypeVariables #-}
{-|
Module      : Htcc.Parser.Parsing.Global.Function
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The function declaration
-}
module Htcc.Parser.Parsing.Global.Function (
    function
) where

import Prelude hiding (toInteger)
import Data.Bits hiding (shift)
import Data.Foldable (Foldable (..))
import Data.List (find)
import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe, isJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)

import Htcc.Utils (tshow, maybeToRight, maybe')
import qualified Htcc.Tokenizer as HT
import qualified Htcc.CRules.Types as CT
import Htcc.Parser.AST
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import Htcc.Parser.ConstructionData
import Htcc.Parser.Utils
import Htcc.Parser.Parsing.Type
import {-# SOURCE #-} Htcc.Parser.Parsing.Core (stmt)
import {-# SOURCE #-} Htcc.Parser.Parsing.Global (globalDef)

-- |
-- \[
-- \begin{array}{ccc}
-- \text{function}&=&\text{pre-type}\ \text{declaration}\ \text{"("}\ \text{params?}\ \text{")"}\ \left(\text{"\{"}\ \text{stmt*}\ \text{"\}"}\ \mid\ \text{";"}\right)\\
-- \text{params}&=&\text{params}\left(\text{","}\ \text{param}\right)\text{*}\\
-- \text{param}&=&\text{pre-type}\ \text{declaration}\ \text{array-decl-suffix}
-- \end{array}
-- \]
function :: (Show i, Read i, Integral i, Bits i) => CT.StorageClass i -> Maybe (HT.TokenLC i) -> [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
function funcType (Just cur@(_, HT.TKIdent fname)) tk@((_, HT.TKReserved "("):_) at !sc = let scp = resetLocal sc in
    (>>=) (maybeToRight (internalCE, cur) (takeBrace "(" ")" $ tail (cur:tk))) $
        either (Left . ("invalid function declaration/definition",)) $ \(fndec, st) -> case st of
            ((_, HT.TKReserved ";"):ds'') -> addFunction False funcType cur scp >>= globalDef ds'' at -- for a function declaration -- TODO: read types of parameters and register them
            ((_, HT.TKReserved "{"):_) -> (>>=) (addFunction True funcType cur scp) $ \scp' -> checkErr fndec scp' $ \args -> runST $ do -- for a function definition
                eri <- newSTRef Nothing
                v <- newSTRef scp'
                mk <- flip unfoldrM args $ \args' -> if null args' then return Nothing else let arg = head args' in do
                -- About @t'@:
                -- An array of type T is equivalent to a pointer of type T in the context of function parameters.
                    m <- flip fmap (readSTRef v) $ \scp'' -> let (t, mident, _, _) = arg; t' = fromMaybe t $ aboutArray t in case mident of
                        Nothing -> Left ("anonymouse variable is not implemented yet", cur) -- TODO
                        Just ident -> addLVar t' ident scp''
                    flip (either ((<$) Nothing . writeSTRef eri . Just)) m $ \(vat, scp'') -> Just (vat, tail args') <$ writeSTRef v scp''
                (>>=) (readSTRef eri) $ flip maybe (return . Left) $ flip fmap (readSTRef v) $ \v' -> (>>=) (stmt st at v') $ \case -- Forbid void to return a value in a return type function.
                    (ert, erat@(ATNode (ATBlock block) _ _ _), erscp) 
                        | CT.toTypeKind funcType == CT.CTVoid -> if isJust (find isNonEmptyReturn block) then
                            Left ("The return type of function '" <> fname <> "' is void, but the statement returns a value", cur) else
                                Right (ert, atDefFunc fname (if null mk then Nothing else Just mk) funcType erat, erscp)
                        | otherwise -> let fnode = atDefFunc fname (if null mk then Nothing else Just mk) funcType erat in
                            maybe' (Right (ert, fnode, erscp)) (find isEmptyReturn block) $ const $ 
                                Right (ert, fnode, pushWarn ("The return type of function '" <> fname <> "' is " <> tshow (CT.toTypeKind funcType) <> ", but the statement returns no value") cur erscp)
                    _ -> Left (internalCE, HT.emptyToken)
            _ -> stmt tk at scp
    where
        checkErr ar !scp' f = let ar' = init $ tail ar in if not (null ar') && snd (head ar') == HT.TKReserved "," then Left ("unexpected ',' token", head ar') else
            let args = linesBy ((==HT.TKReserved ",") . snd) ar' in mapM (`takeType` scp') args >>= f
        aboutArray t
            | CT.isCTArray t = CT.mapTypeKind CT.CTPtr <$> CT.deref t
            | CT.isIncompleteArray t = Just $ CT.mapTypeKind (\(CT.CTIncomplete (CT.IncompleteArray t')) -> CT.CTPtr t') t
            | otherwise = Nothing
function _ _ xs _ _ = Left (internalCE, HT.altEmptyToken xs)
