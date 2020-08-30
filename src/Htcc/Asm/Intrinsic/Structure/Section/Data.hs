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
    sbyte,
    ascii,
    asciiz,
    zero,
    quad,
) where

import qualified Data.ByteString                       as B
import qualified Data.Text                             as T
import qualified Htcc.Asm.Intrinsic.Structure.Internal as C
import           Numeric.Natural

import           Htcc.Utils                            (tshow)

-- | the type representing the context inside the data section
data DataSectionCtx

-- | the type representing the context inside the data label
data DataLabelCtx

-- | data section
dAta :: C.Asm DataSectionCtx e a -> C.Asm C.AsmCodeCtx e a
dAta = C.section "data"

-- | label in data section.
label :: T.Text -> C.Asm DataLabelCtx e a -> C.Asm DataSectionCtx e a
label lbl asm = C.putStrLnWithIndent (lbl <> ":") *> C.unCtx (C.labeled asm)

-- | @byte@ in data section
byte :: B.ByteString -> C.Asm DataLabelCtx e ()
byte = C.putStrLnWithIndent . T.append ".byte " . T.intercalate ", " . map tshow . B.unpack

-- | @.x.byte@ in data section
sbyte :: (Num i, Show i) => Natural -> i -> C.Asm DataLabelCtx e ()
sbyte sz val
    | sz == 1 = C.putStrLnWithIndent $ ".byte " <> tshow val
    | otherwise = C.putStrLnWithIndent $ "." <> tshow sz <> "byte " <> tshow val

-- | @ascii@ in data section
ascii :: B.ByteString -> C.Asm DataLabelCtx e ()
ascii = C.putStrLnWithIndent . T.append ".ascii " . tshow

-- | @asciiz@ in data section
asciiz :: B.ByteString -> C.Asm DataLabelCtx e ()
asciiz = byte . (`B.append` "\0")

-- | @zero@ in data section
zero :: Natural -> C.Asm DataLabelCtx e ()
zero = C.putStrLnWithIndent . T.append ".zero " . tshow

-- | @quad@ in data section
quad :: T.Text -> C.Asm DataLabelCtx e ()
quad = C.putStrLnWithIndent . T.append ".quad "

