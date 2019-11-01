{-|
Module      : Htcc.CRules.LexicalElements
Description : LexicalElements of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

LexicalElements of C language
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.CRules.LexicalElements (
    charOps,
    strOps2,
    strOps3
) where

import qualified Data.Text as T

{-# INLINE charOps #-}
-- | Valid one characters as C language
charOps :: String
charOps = "+-*/()<>=;{},&|^%!~[].?:"

{-# INLINE strOps2 #-}
-- | Valid two characters as C language
strOps2 :: [T.Text]
strOps2 = [
    "<=",
    ">=",
    "==",
    "!=",
    "<<",
    ">>",
    "->",
    "++",
    "--",
    "+=",
    "-=",
    "*=",
    "/=",
    "&&",
    "||",
    "&=",
    "|=",
    "^="
    ]

{-# INLINE strOps3 #-}
-- | Valid three characters as C language
strOps3 :: [T.Text]
strOps3 = [
    "<<=",
    ">>="
    ]
