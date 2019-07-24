{-|
Module      : Htcc.Token
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module Htcc.Token (
    Token (..),
    tokenize
) where

import Data.Char (isDigit, isSpace, isAlpha)

-- | Token type
data Token i = TKReserved String -- ^ The reserved token
    | TKNum i -- ^ The number data
    | TKIdent String -- ^ The identifier
    | TKReturn -- ^ The return keyword
    | TKIf -- ^ The if keyword
    | TKElse -- ^ The else keyword
    | TKWhile -- ^ The while keyword
    | TKFor -- ^ The for keyword
    deriving (Eq, Show)

{-# INLINE charOps #-}
charOps :: String
charOps = "+-*/()<>=;"

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
        tokenize' n xxs@(x:xs)
            | isDigit x = let ts = takeWhile isDigit xs in 
                (TKNum (read (x:ts)) :) <$> tokenize' (succ (n + length ts)) (drop (length ts) xs)
            | isSpace x = tokenize' (succ n) xs
            | not (null xs) && [x, head xs] `elem` strOps = (TKReserved [x, head xs] :) <$> tokenize' (n + 2) (tail xs)
            | x `elem` charOps = (TKReserved [x] :) <$> tokenize' (succ n) xs
            | otherwise = let tk = takeWhile (\y -> isAlpha y || isDigit y || y == '_') xxs in 
                if null tk then Left n else let len = length tk; dxxs = drop len xxs in case tk of
                    "return" -> callNext tk (n + len) TKReturn $ drop len xxs 
                    "if" -> callNext tk (n + len) TKIf $ drop len xxs
                    "else" -> callNext tk (n + len) TKElse $ drop len xxs
                    "while" -> callNext tk (n + len) TKWhile $ drop len xxs
                    "for" -> callNext tk (n + len) TKFor $ drop len xxs
                    _ -> (TKIdent tk :) <$> tokenize' (succ (n + len)) dxxs
            where
                callNext tk nlen itk dxxs = (if not (null dxxs) && isSpace (head dxxs) then (itk :) else (TKIdent tk :)) <$> tokenize' (succ nlen) dxxs
