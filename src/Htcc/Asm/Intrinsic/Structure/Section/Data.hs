{-|
Module      : Htcc.Asm.Intrinsic.Structure.Section.Data
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Intrinsic.Structure.Section.Data (
    DataSectionCtx,
    DataLabelCtx,
    dAta,
    label,
    byte,
    zero
) where

import qualified Htcc.Asm.Intrinsic.Structure.Internal as C
import qualified Data.Text as T
import qualified Data.ByteString as B
import Numeric.Natural

import Htcc.Utils (tshow)

-- | the type representing the context inside the data section
data DataSectionCtx

-- | the type representing the context inside the data label
data DataLabelCtx

-- | data section
dAta :: C.Asm DataSectionCtx e a -> C.Asm C.AsmCodeCtx e a
dAta = C.section "data"

-- | label in data section
label :: T.Text -> C.Asm DataLabelCtx e a -> C.Asm DataSectionCtx e a
label lbl asm = C.putStrLnWithIndent (lbl <> ":") *> C.unCtx (C.labeled asm)

-- | @byte@ in data section
byte :: B.ByteString -> C.Asm DataLabelCtx e ()
byte = C.putStrLnWithIndent . T.append ".byte " . T.intercalate ", " . map tshow . B.unpack

-- | @zero@ in data section
zero :: Natural -> C.Asm DataLabelCtx e ()
zero = C.putStrLnWithIndent . T.append ".zero " . tshow
