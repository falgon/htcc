{-|
Module      : Htcc.Visualizer.Core
Description : Build AST from C source code
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Build AST from C source code
-}
module Htcc.Visualizer.Core (
    visualize
) where

import qualified Data.Text as T
import Control.Monad (unless)

import Htcc.Asm.Generate (buildAST, parsedErrExit, parsedWarn)
import Htcc.Utils (putStrLnErr)

-- | Build AST from C source code
visualize :: Bool -> T.Text -> IO ()
visualize supWarns xs = flip (either (parsedErrExit xs)) (buildAST xs) $ \(warns, tk, _, _) ->
    unless supWarns (parsedWarn xs warns) >> print (show tk)
