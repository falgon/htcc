module Htcc.Parser.Parsing.Global where

import Data.Bits
import Htcc.Parser.AST
import Htcc.Parser.ConstructionData
import qualified Htcc.Tokenizer as HT

globalDef :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
