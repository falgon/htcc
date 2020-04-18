{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase, BangPatterns, ScopedTypeVariables #-}
{-|
Module      : Htcc.Parser.Parsing.StmtExpr
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The module of the statement expression (GNU extension: <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>)
-}
module Htcc.Parser.Parsing.StmtExpr (
    stmtExpr
) where

import Prelude hiding (toInteger)
import Data.Bits hiding (shift)
import Data.Foldable (Foldable (..))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Monad.Loops (unfoldrM)

import Htcc.Utils (tshow, maybeToRight)
import qualified Htcc.Tokenizer as HT
import Htcc.Parser.AST
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import Htcc.Parser.ConstructionData
import Htcc.Parser.Utils
import {-# SOURCE #-} Htcc.Parser.Parsing.Core (stmt)

-- | statement expression (GNU extension: <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>)
-- \[\text{stmt-expr}=\text{"("}\ \text{"\{"}\ \text{stmt}\ \text{stmt*}\ \text{"\}"}\ \text{")"}\]
stmtExpr :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
stmtExpr ((_, HT.TKReserved "("):xs@((_, HT.TKReserved "{"):_)) _ !scp = (>>=) (maybeToRight (internalCE, head xs) (takeBrace "{" "}" xs)) $ 
    either (Left . ("the statement expression is not closed",)) $ \(sctk, ds) -> case ds of
        (_, HT.TKReserved ")"):ds' -> runST $ do
            eri <- newSTRef Nothing
            v <- newSTRef $ succNest scp
            lastA <- newSTRef ATEmpty 
            mk <- flip unfoldrM (init $ tail sctk) $ \ert -> if null ert then return Nothing else do
                erscp <- readSTRef v
                flip (either $ \err -> Nothing <$ writeSTRef eri (Just err)) (stmt ert ATEmpty erscp) $ \(ert', erat', erscp') -> 
                    Just (erat', ert') <$ (writeSTRef v erscp' >> when (case erat' of ATEmpty -> False; _ -> True) (writeSTRef lastA erat'))
            (>>=) (readSTRef eri) $ flip maybe (return . Left) $ do
                v' <- readSTRef v
                flip fmap (readSTRef lastA) $ \case
                        (ATNode ATExprStmt _ lhs _) -> Right (ds', atNoLeaf (ATStmtExpr (init mk ++ [lhs])) (atype lhs), fallBack scp v')
                        _ -> Left ("void value not ignored as it ought to be. the statement expression starts here:", head xs)
        _ -> Left $ if null sctk then ("expected ')' token. the statement expression starts here: ", head xs) else
            ("expected ')' token after '" <> tshow (snd $ last sctk) <> "' token", last sctk)
stmtExpr xs _ _ = Left (internalCE, HT.altEmptyToken xs)
