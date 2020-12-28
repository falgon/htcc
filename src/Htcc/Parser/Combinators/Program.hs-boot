module Htcc.Parser.Combinators.Program where

import           Data.Bits                    (Bits)
import           Htcc.Parser.AST              (ATree)
import           Htcc.Parser.Combinators.Core (Parser)

conditional :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i (ATree i)

compoundStmt :: (Ord i, Bits i, Read i, Show i, Integral i) => Parser i [ATree i]
