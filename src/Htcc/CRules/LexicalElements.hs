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
module Htcc.CRules.LexicalElements (
    charOps,
    strOps
) where

{-# INLINE charOps #-}
-- | Valid one characters as C language
charOps :: String
charOps = "+-*/()<>=;{},&|^%!~[]"

{-# INLINE strOps #-}
-- | Valid two characters as C language
strOps :: [String]
strOps = [
    "<=",
    ">=",
    "==",
    "!=",
    "<<",
    ">>"
    ]
