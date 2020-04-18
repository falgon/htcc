{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns, ScopedTypeVariables #-}
{-|
Module      : Htcc.Parser.Parsing.Typedef
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Perspective on @typedef@ declaration
-}
module Htcc.Parser.Parsing.Typedef (
    typedef
) where

import Data.Bits (Bits)
import Data.Foldable (Foldable (..))

import Htcc.Utils (tshow, maybeToRight)
import qualified Htcc.Tokenizer as HT
import Htcc.Parser.AST
import Htcc.Parser.ConstructionData.Scope.Utils (internalCE)
import Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import Htcc.Parser.ConstructionData
import Htcc.Parser.Parsing.Type

-- | Perform type definition from token string starting from @typedef@ token.
-- \[\text{typedef-name}=\text{ident}\]
typedef :: (Integral i, Show i, Read i, Bits i) => [(HT.TokenLCNums i, HT.Token i)] -> ConstructionData i -> Either (ASTError i) ([HT.TokenLC i], ATree a, ConstructionData i)
typedef ((_, HT.TKTypedef):cur@(_, HT.TKReserved _):_) _ = Left ("storage-class specifier is not allowed in this context", cur)
typedef (cur@(_, HT.TKTypedef):xs) !scp = case takeType xs scp of
    Left er -> Left er
    Right (ty, Just ident, ds, scp') -> case ds of
        (_, HT.TKReserved ";"):ds' -> do
            ty' <- maybeToRight ("incomplete type typedef", ident) (incomplete ty scp')
            (ds', ATEmpty,) <$> addTypedef ty' ident scp'
        _ -> Left ("expected ';' token after '" <> tshow (snd ident) <> "'", ident)
    Right (_, Nothing, ds, scp') -> case ds of
        (_, HT.TKReserved ";"):ds' -> Right (ds', ATEmpty, pushWarn "useless type name in empty declaration" cur scp')
        _ -> Left $ if not (null ds) then ("expected ';' token after '" <> tshow (snd $ head ds) <> "'", head ds) else ("expected ';' token", HT.emptyToken)
typedef _ _ = Left (internalCE, HT.emptyToken)

