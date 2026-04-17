{-|
Module      : Htcc.Parser.Combinators.ParserType
Description : C language parser type
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser type
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, TupleSections #-}
module Htcc.Parser.Combinators.ParserType (
    runParser
  , runParserAllowSameInputExternalCollisions
  , runParserAllowSameInputExternalCollisionsDetailed
  , ConstructionDataState
  , Parser
) where

import                          Control.Monad.Trans.State.Lazy              (StateT,
                                                                             runStateT)
import                          Data.Functor.Identity
import                qualified Data.Text                                   as T
import                          Data.Void
import                          Htcc.Parser.AST.Type                        (ASTs)
import {-# SOURCE #-}           Htcc.Parser.ConstructionData.Core
import                qualified Htcc.Parser.ConstructionData.Scope          as PS
import                qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import                qualified Htcc.Parser.ConstructionData.Scope.Tag      as PST
import                qualified Htcc.Parser.ConstructionData.Scope.Var      as PSV
import                qualified Text.Megaparsec                             as M

type ConstructionDataState i = StateT (ConstructionData i) Identity
type Parser i = M.ParsecT Void T.Text (ConstructionDataState i)

runParser ::
    Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs i, PSV.GlobalVars i, PSV.Literals i, PF.Functions i)
runParser = runParserWithMode False

runParserAllowSameInputExternalCollisions ::
    Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs i, PSV.GlobalVars i, PSV.Literals i, PF.Functions i)
runParserAllowSameInputExternalCollisions = runParserWithMode True

runParserAllowSameInputExternalCollisionsDetailed ::
    Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either
        (M.ParseErrorBundle T.Text Void)
        ( Warnings
        , ASTs i
        , PSV.GlobalVars i
        , PSV.GlobalVars i
        , PSV.Literals i
        , PF.Functions i
        , PF.Functions i
        )
runParserAllowSameInputExternalCollisionsDetailed =
    runParserWithModeDetailed True

runParserWithMode ::
    Bool
    -> Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs i, PSV.GlobalVars i, PSV.Literals i, PF.Functions i)
runParserWithMode allowSameInputExternalCollisionsMode p fp input =
    (\(warns', asts, gvars, _, lits, funcs, _) -> (warns', asts, gvars, lits, funcs))
        <$> runParserWithModeDetailed allowSameInputExternalCollisionsMode p fp input

runParserWithModeDetailed ::
    Bool
    -> Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either
        (M.ParseErrorBundle T.Text Void)
        ( Warnings
        , ASTs i
        , PSV.GlobalVars i
        , PSV.GlobalVars i
        , PSV.Literals i
        , PF.Functions i
        , PF.Functions i
        )
runParserWithModeDetailed allowSameInputExternalCollisionsMode p fp input =
    (\asts -> (warns (snd result), asts, visibleGlobals finalScope, mergeGlobals finalScope, PSV.literals (PS.vars finalScope), PS.functions finalScope, mergeFunctions finalScope))
        <$> fst result
    where
        finalScope = scope $ snd result
        visibleGlobals scp = PSV.globals $ PS.vars scp
        mergeGlobals scp
            | allowSameInputExternalCollisionsMode = PSV.externalGlobals $ PS.vars scp
            | otherwise = visibleGlobals scp
        mergeFunctions scp
            | allowSameInputExternalCollisionsMode = PS.externalFunctions scp
            | otherwise = PS.functions scp
        result =
            runIdentity $
                runStateT
                    (M.runParserT p fp input)
                    (ConstructionData mempty PS.initScope PST.emptyTagHistory [] False allowSameInputExternalCollisionsMode)
