{-|
Module      : Htcc.Asm
Description : The modules of C Rules
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The executable module for compilation
-}
module Htcc.Asm (
    -- * Export modules
    module Htcc.Asm.Generate,
    casm
) where

import Data.Tuple.Extra (uncurry3)

import Htcc.Asm.Generate 
import Htcc.Parser (ASTs)
import Htcc.Parser.ConstructionData.Scope.Var (GlobalVars, Literals)
import qualified Htcc.Asm.Intrinsic.Structure.Internal as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text.Instruction as TI
import qualified Htcc.Asm.Intrinsic.Operand as O

-- | Generate full assembly code from string of C source code
casm :: (O.IsOperand i, TI.UnaryInstruction i, TI.BinaryInstruction i, Integral i) => (ASTs i, GlobalVars i, Literals i) -> IO ()
casm = SI.runAsm . uncurry3 casm'
