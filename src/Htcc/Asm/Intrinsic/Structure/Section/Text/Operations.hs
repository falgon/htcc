{-|
Module      : Htcc.Asm.Intrinsic.Structure.Section.Text.Operations
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
module Htcc.Asm.Intrinsic.Structure.Section.Text.Operations (
    incrLbl,
    applyCnt,
    applyBrk,
    bracketBrkCnt
) where

import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.Tuple.Extra ((&&&))

import qualified Htcc.Asm.Intrinsic.Structure.Internal as C
import Htcc.Asm.Intrinsic.Structure.Section.Text.Directive
import Htcc.Utils (bothM, (*^*))
import Control.Monad.Finally (MonadFinally (..))

-- | count up the internal label counter
incrLbl :: C.Asm TextLabelCtx e Int
incrLbl = C.Asm $ \x -> modifyIORef (C.lblCnt x) succ >> readIORef (C.lblCnt x)

-- | apply value to cntCnt from the current label number
applyCnt :: C.Asm ctx e ()
applyCnt = C.Asm $ \x -> readIORef (C.lblCnt x) >>= writeIORef (C.cntCnt x) . Just

-- | apply value to brkCnt from the current label number
applyBrk :: C.Asm ctx e ()
applyBrk = C.Asm $ \x -> readIORef (C.lblCnt x) >>= writeIORef (C.brkCnt x) . Just

-- | Apply values from lblCnt to brkCnt and cntCnt in function execution scope, 
-- and return values to their original state when exiting the scope
bracketBrkCnt :: C.Asm TextLabelCtx e () -> C.Asm TextLabelCtx e ()
bracketBrkCnt mc = bracket
    (C.Asm $ bothM readIORef . (C.brkCnt &&& C.cntCnt))
    (\y -> C.Asm $ \x -> (writeIORef (C.brkCnt x) *^* writeIORef (C.cntCnt x)) y) $ const mc
