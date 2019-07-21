module C.Token (
    Token (..),
    tokenize
) where

import Data.Char (isDigit, isSpace)

data Token = TKReserved Char | TKNum Int deriving (Eq, Show)

tokenize :: String -> Either Int [Token]
tokenize zs = tokenize' 0 zs
    where
        tokenize' _ [] = Right []
        tokenize' n xxs@(x:xs)
            | isDigit x = let t = takeWhile isDigit xxs in 
                (TKNum (read t) :) <$> tokenize' (n + length t) (drop (length t) xxs)
            | isSpace x = tokenize' (succ n) xs
            | x == '+' || x == '-' = (TKReserved x :) <$> tokenize' (succ n) xs
            | otherwise = Left n
