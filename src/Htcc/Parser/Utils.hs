{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns #-}
{-|
Module      : Htcc.Parser.Utils
Description : The AST data type and its utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The utilities of parsing
-}
module Htcc.Parser.Utils (
    -- * General utilities of parser
    expectedMessage,
    -- * Utilities of the token
    takeBrace,
    takeExps
) where

import qualified Data.Text as T
import Data.Tuple.Extra (first)

import Htcc.Utils (tshow, maybe', lastInit)
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)
import qualified Htcc.Tokenizer.Token as HT

-- | "expected" error message
expectedMessage :: Show i => T.Text -> HT.TokenLC i -> [HT.TokenLC i] -> ASTError i
expectedMessage x t xs
    | length xs > 1 = ("expected '" <> x <> "' token before '" <> tshow (snd (xs !! 1)) <> "'", head xs)
    | otherwise = ("expected '" <> x <> "' token", if null xs then t else head xs)

-- | Extract the partial token enclosed in parentheses from the token sequence. If it is invalid, `takeBrace` returns @(i, Text)@ indicating the error location.
-- Otherwise, `takeBrace` returns a partial token enclosed in parentheses and subsequent tokens.
takeBrace :: forall i. (Integral i, Read i, Show i) => T.Text -> T.Text -> [HT.TokenLC i] -> Maybe (Either (HT.TokenLC i) ([HT.TokenLC i], [HT.TokenLC i]))
takeBrace leftb rightb xxs@((_, HT.TKReserved y):_) 
    | y == leftb = Just $ f 0 0 xxs
    | otherwise = Nothing
    where
        f :: Int -> Int -> [HT.TokenLC i] -> Either (HT.TokenLC i) ([HT.TokenLC i], [HT.TokenLC i])
        f !l !r []
            | l /= r = Left $ head xxs 
            | otherwise = Right ([], [])
        f !l !r (c@(p, HT.TKReserved x):xs') 
            | x == rightb = if l == succ r then Right ([c], xs') else g l (succ r) xs'
            | x == leftb = if succ l == r then Right ([c], xs') else g (succ l) r xs'
            | otherwise = g l r xs'
            where
                g = (.) (fmap (first ((p, HT.TKReserved x):)) .) . f
        f !l !r ((p, x):xs') = first ((:) (p, x)) <$> f l r xs'
takeBrace _ _ _ = Nothing

-- | Get an argument from list of `Htcc.Tokenizer.Token` (e.g: Given the token of @f(g(a, b)), 42@, return the token of @f(g(a, b))@).
readFn :: Eq i => [HT.TokenLC i] -> Maybe ([HT.TokenLC i], [HT.TokenLC i])
readFn = readFn' 0 (0 :: Int)
    where
        readFn' !li !ri (cur@(_, HT.TKReserved ","):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li ri xs
        readFn' !li !ri (cur@(_, HT.TKReserved ")"):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li (succ ri) xs
        readFn' !li !ri (cur@(_, HT.TKReserved "("):xs) = first (cur:) <$> readFn' (succ li) ri xs
        readFn' !li !ri []
            | li == ri = Just ([], [])
            | otherwise = Nothing
        readFn' !li !ri (x:xs) = first (x:) <$> readFn' li ri xs

-- | Get arguments from list of `Htcc.Tokenizer.Token` (e.g: Given the token of @f(f(g(a, b)), 42);@, 
-- return expressions that are the token of "f(g(a, b))" and the token of "42".
takeExps :: Eq i => [HT.TokenLC i] -> Maybe [[HT.TokenLC i]]
takeExps ((_, HT.TKIdent _):(_, HT.TKReserved "("):xs) = maybe' Nothing (lastInit ((==HT.TKReserved ")") . snd) xs) $ fmap (filter (not . null)) . f
    where
        f [] = Just []
        f args = maybe Nothing (\(ex, ds) -> (ex:) <$> f ds) $ readFn args
takeExps _ = Nothing

