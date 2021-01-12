{-|
Module      : Htcc.Asm.Generate
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The executable module for compilation
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Generate (
    InputCCode,
    -- * Generator
    casm'
) where

import qualified Data.Text                                 as T
import           Htcc.Asm.Generate.Core
import           Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Structure              as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text as IT
import           Htcc.Parser                               (ASTs)
import           Htcc.Parser.ConstructionData.Scope.Var    (GlobalVars,
                                                            Literals)

-- | input string, C source code
type InputCCode = T.Text

data MessageType = ErrorMessage | WarningMessage
    deriving (Eq, Ord, Enum, Bounded)

instance Show MessageType where
    show ErrorMessage   = "error"
    show WarningMessage = "warning"

-- | Executor that receives information about the constructed AST,
-- global variables, and literals and composes assembly code
casm' :: (Integral e, Show e, Integral i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i)
    => ASTs i
    -> GlobalVars i
    -> Literals i
    -> SI.Asm SI.AsmCodeCtx e ()
casm' atl gvars lits = dataSection gvars lits >> textSection atl

