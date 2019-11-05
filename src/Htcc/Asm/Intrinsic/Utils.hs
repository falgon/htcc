{-|
Module      : Htcc.Asm.Intrinsic.Utils
Description : The utilities of x86_64 assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

`Htcc.Asm.Intrinsic.Utils` exports utilities that help generating x86_64 assembly.
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Intrinsic.Utils (
    -- * Define and refer to begin and end local labels,
    defLLbl,
    refLLbl,
    defGLbl,
    defLbl,
    defBegin,
    defEnd,
    refBegin,
    refEnd,
    declIS
) where

import qualified Data.Text as T
import Data.Tuple.Extra (first, second, dupe)
import Htcc.Utils (tshow)

-- | Define the specified label
{-# INLINE defLLbl #-}
defLLbl :: (Show i, Integral i) => T.Text -> i -> T.Text
defLLbl = (.) (`T.append` ":\n") . (.) (T.append ".L") . flip (.) tshow . T.append

-- | Refer the specified label
{-# INLINE refLLbl #-}
refLLbl :: (Show i, Integral i) => T.Text -> i -> T.Text
refLLbl = (.) (T.append ".L") . flip (.) tshow . T.append

-- | Define the global label
{-# INLINE defGLbl #-}
defGLbl :: T.Text -> T.Text
defGLbl = uncurry T.append . first (flip T.append "\n" . T.append ".global ") . second (`T.append` ":\n") . dupe

-- | Define the local label
{-# INLINE defLbl #-}
defLbl :: T.Text -> T.Text
defLbl x = x <> ":\n"

-- | Define the .Lbeginx local label
{-# INLINE defBegin #-}
defBegin :: (Show i, Integral i) => i -> T.Text
defBegin = defLLbl ".begin."

-- | Define the .Lendx local label
{-# INLINE defEnd #-}
defEnd :: (Show i, Integral i) => i -> T.Text
defEnd = defLLbl ".end."

-- | Refer the .Lbeginx label
{-# INLINE refBegin #-}
refBegin :: (Show i, Integral i) => i -> T.Text
refBegin = refLLbl ".begin."

-- | Refer the .Lendx label
{-# INLINE refEnd #-}
refEnd :: (Show i, Integral i) => i -> T.Text
refEnd = refLLbl ".end."

-- | Declare intel syntax
{-# INLINE declIS #-}
declIS :: T.Text
declIS = ".intel_syntax noprefix\n"
