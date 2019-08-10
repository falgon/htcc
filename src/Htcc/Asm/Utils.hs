{-|
Module      : Htcc.Asm.Utils
Description : The utilities of x86_64 assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

`Htcc.Asm.Utils` exports utilities that help generating x86_64 assembly.
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Utils (
    -- * Define and refer to begin and end local labels
    defBegin,
    defEnd,
    refBegin,
    refEnd
) where

import qualified Data.Text as T
import Htcc.Utils (tshow)

-- | Define the .Lbeginx local label
defBegin :: (Show i, Integral i) => i -> T.Text
defBegin = flip T.append "\n" . T.append ".Lbegin" . flip T.append ":" . tshow

-- | Define the .Lendx local label
defEnd :: (Show i, Integral i) => i -> T.Text
defEnd = flip T.append "\n" . T.append ".Lend" . flip T.append ":" . tshow

-- | refer the .Lbeginx label
refBegin :: (Show i, Integral i) => i -> T.Text
refBegin = T.append ".Lbegin" . tshow

-- | refer the .Lendx label
refEnd :: (Show i, Integral i) => i -> T.Text
refEnd = T.append ".Lend" . tshow
