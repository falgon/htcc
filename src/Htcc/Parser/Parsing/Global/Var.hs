{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables,
             TupleSections #-}
{-|
Module      : Htcc.Parser.Parsing.Global.Var
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Global variable declaration
-}
module Htcc.Parser.Parsing.Global.Var (
    var
) where

import           Data.Bits                                       hiding (shift)
import           Data.Foldable                                   (Foldable (..))
import           Prelude                                         hiding
                                                                  (toInteger)

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST
import           Htcc.Parser.ConstructionData
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Parser.ConstructionData.Scope.Var          as PV
import {-# SOURCE #-} Htcc.Parser.Parsing.Core                        (conditional)
import           Htcc.Parser.Parsing.Type
import qualified Htcc.Tokenizer                                  as HT
import           Htcc.Utils                                      (maybeToRight,
                                                                  tshow)

gvarInit :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> CT.StorageClass i -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) ([HT.TokenLC i], ConstructionData i)
gvarInit xs ty ident sc = do
    (ds, ast, sc') <- conditional xs ATEmpty sc
    case (atkind ast, atkind (atL ast)) of
        (ATAddr, ATGVar _ name) -> (ds,) . snd <$> gvarInitWithOG ty ident name sc'
        (ATAddr, _) -> Left ("invalid initializer in global variable", HT.altEmptyToken ds)
        (ATGVar t name, _)
            | CT.isCTArray t -> (ds,) . snd <$> gvarInitWithOG ty ident name sc'
            | otherwise -> gvarInitWithVal ds sc'
        _ -> gvarInitWithVal ds sc'
    where
        gvarInitWithOG ty' from to = addGVarWith ty' from (PV.GVarInitWithOG to)
        gvarInitWithVal ds sc' = do
            (ds', cval) <- either (maybe (Left ("initializer element is not constant", HT.altEmptyToken ds)) Left) Right $ constantExp xs sc'
            (ds',) . snd <$> addGVarWith ty ident (PV.GVarInitWithVal cval) sc'

-- | \[
-- \text{global-var} = \text{pre-type}\ \text{declaration}\ \text{array-decl-suffix}\ \text{";"}
-- \]
var :: (Show i, Read i, Integral i, Bits i) => CT.StorageClass i -> Maybe (HT.TokenLC i) -> [HT.TokenLC i] -> ConstructionData i -> ASTConstruction i
var ty (Just cur@(_, HT.TKIdent _)) xs !scp = case xs of
    (_, HT.TKReserved "="):ds -> do -- for initializing
        ty' <- maybeToRight ("defining global variables with a incomplete type", cur) (incomplete ty scp)
        (ds', nsc) <- gvarInit ds ty' cur scp
        case ds' of
            (_, HT.TKReserved ";"):ds'' -> return (ds'', ATEmpty, nsc)
            _ -> Left $ if null ds' then
                ("expected ';' token after '" <> tshow (snd cur) <> "' token", HT.altEmptyToken ds') else
                    ("expected ';' token" <> (if null ds' then "" else " before '" <> tshow (snd $ head ds') <> "' token"), HT.altEmptyToken ds')
    (_, HT.TKReserved ";"):ds -> do -- for non initializing
        ty' <- maybeToRight ("defining global variables with a incomplete type", cur) (incomplete ty scp)
        (ds, ATEmpty,) . snd <$> addGVar ty' cur scp
    _ -> Left ("expected ';' token after '" <> tshow (snd cur) <> "' token", cur)
var _ _ xs _ = Left (internalCE, HT.altEmptyToken xs)

