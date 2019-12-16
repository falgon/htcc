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

import Control.Monad (unless)
import Data.Tuple.Extra (uncurry3)

import Htcc.Asm.Generate
import qualified Htcc.Asm.Intrinsic.Structure.Internal as SI
import Htcc.Utils (fst4, dropFst4)

-- | Generate full assembly code from string of C source code
casm :: Bool -> InputCCode -> IO ()
casm supWarns ccode = flip (either (parsedErrExit ccode)) (buildAST ccode) $ \res ->
    unless supWarns (parsedWarn ccode $ fst4 res) >> SI.runAsm (uncurry3 casm' $ dropFst4 res)
