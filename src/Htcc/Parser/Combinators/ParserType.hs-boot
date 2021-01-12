{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, TupleSections #-}
module Htcc.Parser.Combinators.ParserType where

import           Control.Monad.Trans.State.Lazy         (StateT (..))
import           Data.Functor.Identity
import qualified Data.Text                              as T
import           Data.Void
import           Htcc.Parser.AST.Type                   (ASTs)
import {-# SOURCE #-} Htcc.Parser.ConstructionData.Core      (ConstructionData,
                                                              Warnings)
import qualified Htcc.Parser.ConstructionData.Scope.Var as PSV
import qualified Text.Megaparsec                        as M

type ConstructionDataState i = StateT (ConstructionData i) Identity
type Parser i = M.ParsecT Void T.Text (ConstructionDataState i)

runParser ::
    Parser i (ASTs i)
    -> FilePath
    -> T.Text
    -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs i, PSV.GlobalVars i, PSV.Literals i)
