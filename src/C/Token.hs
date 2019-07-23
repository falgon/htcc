{-|
Module      : C.Token
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module C.Token (
    Token (..),
    tokenize
) where

import Data.Char (isDigit, isSpace)

-- | Token type
data Token i = TKReserved String | TKNum i deriving (Eq, Show)

{-# INLINE charOps #-}
charOps :: String -- Char
charOps = "+-*/()<>"

{-# INLINE strOps #-}
strOps :: [String]
strOps = [
    "<=",
    ">=",
    "==",
    "!="
    ]

-- | Tokenize from `String`. If it fails, the Left that wraps the value representing that point is returned.
tokenize :: Read i => String -> Either Int [Token i]
tokenize = tokenize' 0
    where
        tokenize' _ [] = Right []
        tokenize' n (x:xs)
            | isDigit x = let ts = takeWhile isDigit xs in 
                (TKNum (read (x:ts)) :) <$> tokenize' (succ (n + length ts)) (drop (length ts) xs)
            | isSpace x = tokenize' (succ n) xs
            | not (null xs) && [x, head xs] `elem` strOps = (TKReserved [x, head xs] :) <$> tokenize' (n + 2) (tail xs)
            | x `elem` charOps = (TKReserved [x] :) <$> tokenize' (succ n) xs
            | otherwise = Left n
