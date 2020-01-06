{-|
Module      : Htcc.Asm.Intrinsic.Structure.Section.Text
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
module Htcc.Asm.Intrinsic.Structure.Section.Text (
    module Htcc.Asm.Intrinsic.Structure.Section.Text.Directive,
    module Htcc.Asm.Intrinsic.Structure.Section.Text.Instruction,
    module Htcc.Asm.Intrinsic.Structure.Section.Text.Operations
) where

import Htcc.Asm.Intrinsic.Structure.Section.Text.Directive
import Htcc.Asm.Intrinsic.Structure.Section.Text.Instruction
import Htcc.Asm.Intrinsic.Structure.Section.Text.Operations
