{-# LANGUAGE OverloadedStrings, LambdaCase, BangPatterns #-}
{-|
Module      : Htcc.Parser.Parsing.Global
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The module of the globals
-}
module Htcc.Parser.Parsing.Global (
    globalDef
) where

import Data.Bits

import Htcc.Utils (uncurry4)
import Htcc.Parser.AST
import Htcc.Parser.ConstructionData
import Htcc.Parser.Parsing.Type (takeType)
import Htcc.Parser.Parsing.Global.Var
import Htcc.Parser.Parsing.Global.Function
import Htcc.Parser.Parsing.Typedef
import qualified Htcc.Tokenizer as HT

-- | `globalDef` parses global definitions (include functions and global variables)
-- \[
-- \text{global-def}=\left(\text{global-var}\ \mid\ \text{function}\right)\text{*}
-- \]
globalDef :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
globalDef (cur@(_, HT.TKReserved "register"):_) _ _ = Left ("illegal storage class on file-scoped identifier", cur)
globalDef (cur@(_, HT.TKReserved "auto"):_) _ _ = Left ("illegal storage class on file-scoped identifier", cur)
globalDef xs@((_, HT.TKTypedef):_) _ sc = typedef xs sc -- for global @typedef@
globalDef tks at !va = (>>=) (takeType tks va) $ \case
    (_, Nothing, (_, HT.TKReserved ";"):ds', scp) -> Right (ds', ATEmpty, scp) -- e.g., @int;@ is legal in C11 (See N1570/section 6.7 Declarations)
    (funcType, ident@(Just (_, HT.TKIdent _)), tk@((_, HT.TKReserved "("):_), !sc) -> function funcType ident tk at sc
    p@(_, Just (_, HT.TKIdent _), _, _) -> uncurry4 var p
    _ -> Left ("invalid definition of global identifier", HT.altEmptyToken tks)
