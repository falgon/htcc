module Htcc.Parser.Parsing.Core where

import Data.Bits (Bits)
import Htcc.Tokenizer (TokenLC)
import Htcc.Parser.ConstructionData (ConstructionData)
import Htcc.Parser.AST (ATree, ASTConstruction)
import qualified Htcc.Tokenizer as HT

stmt :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i

conditional :: (Show i, Read i, Integral i, Bits i) => [TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i

-- globalDef :: (Show i, Read i, Integral i, Bits i) => [HT.TokenLC i] -> ATree i -> ConstructionData i -> ASTConstruction i
