module Htcc.Parser.Parsing.Core where

import Data.Bits (Bits)
import Htcc.Tokenizer (TokenLC)
import Htcc.Parser.ConstructionData (ConstructionData)
import Htcc.Parser.AST (ATree, ASTConstruction)

conditional :: (Show i, Read i, Integral i, Bits i) => [TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i

