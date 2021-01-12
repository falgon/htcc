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
  , ConstructionDataState
  , Parser
) where

import           Control.Monad.Trans.State.Lazy         (StateT, runStateT)
import           Data.Functor.Identity
import qualified Data.Text                              as T
import           Data.Void
import           Htcc.Parser.AST.Type                   (ASTs)
import {-# SOURCE #-} Htcc.Parser.ConstructionData.Core
import qualified Htcc.Parser.ConstructionData.Scope     as PS
import qualified Htcc.Parser.ConstructionData.Scope.Var as PSV
import qualified Text.Megaparsec                        as M

type ConstructionDataState i = StateT (ConstructionData i) Identity
type Parser i = M.ParsecT Void T.Text (ConstructionDataState i)

runParser ::
    Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs i, PSV.GlobalVars i, PSV.Literals i)
runParser p fp input =
    (warns (snd result),, PSV.globals $ PS.vars $ scope $ snd result, PSV.literals $ PS.vars $ scope $ snd result)
        <$> fst result
    where
        result = runIdentity $ runStateT (M.runParserT p fp input) initConstructionData

