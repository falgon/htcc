{-|
Module      : Htcc.Asm.Intrinsic
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
module Htcc.Asm.Intrinsic (
    -- * Export modules
    module Htcc.Asm.Intrinsic.Register,
    module Htcc.Asm.Intrinsic.Operand
) where

import           Htcc.Asm.Intrinsic.Operand
import           Htcc.Asm.Intrinsic.Register
