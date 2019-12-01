{-|
Module      : Htcc.Asm
<<<<<<< HEAD
Description : The modules of C Rules
=======
Description : Asm
>>>>>>> :sparkles: Add --visualize-ast option (not implemented)
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

<<<<<<< HEAD
The executable module for compilation
-}
module Htcc.Asm (
    -- * Export modules
    module Htcc.Asm.Generate
) where

import Htcc.Asm.Generate
=======
The tokenizer
-}
module Htcc.Asm (
    casm
) where

import Control.Monad (unless)
import qualified Data.Text.IO as T

import Htcc.Asm.Generate
import qualified Htcc.Asm.Intrinsic.Utils as I

-- | Generate full assembly code from string of C source code
casm :: Bool -> InputCCode -> IO ()
casm supWarns xs = flip (either (parsedErrExit xs)) (buildAST xs) $ \(warns, tk, gvars, lits) -> 
    unless supWarns (parsedWarn xs warns) >> T.putStr I.declIS >> dataSection gvars lits >> textSection tk
>>>>>>> :sparkles: Add --visualize-ast option (not implemented)
