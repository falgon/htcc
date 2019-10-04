{-|
Module      : Htcc.Token.Utils
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Token.Utils (
    -- * Utilities of the token
    takeBrace,
    takeExps,
    makeTypes,
    arrayDeclSuffix
) where

import qualified Data.Text as T
import Data.Tuple.Extra (first)
import Data.Maybe (fromJust)

import qualified Htcc.CRules as CR
import Htcc.Utils (lastInit, spanLen, dropSnd, tshow, toNatural)
import Htcc.Token.Core

-- | Extract the partial token enclosed in parentheses from the token sequence. If it is invalid, `takeBrace` returns @(i, Text)@ indicating the error location.
-- Otherwise, `takeBrace` returns a partial token enclosed in parentheses and subsequent tokens.
takeBrace :: forall i. (Integral i, Read i, Show i) => T.Text -> T.Text -> [TokenIdx i] -> Maybe (Either (TokenIdx i) ([TokenIdx i], [TokenIdx i]))
takeBrace leftb rightb xxs@((_, TKReserved y):_) 
    | y == leftb = Just $ f 0 0 xxs
    | otherwise = Nothing
    where
        f :: Int -> Int -> [TokenIdx i] -> Either (TokenIdx i) ([TokenIdx i], [TokenIdx i])
        f l r []
            | l /= r = Left $ head xxs 
            | otherwise = Right ([], [])
        f l r (c@(p, TKReserved x):xs') 
            | x == rightb = if l == succ r then Right ([c], xs') else g l (succ r) xs'
            | x == leftb = if succ l == r then Right ([c], xs') else g (succ l) r xs'
            | otherwise = g l r xs'
            where
                g = (.) (fmap (first ((p, TKReserved x):)) .) . f
        f l r ((p, x):xs') = first ((:) (p, x)) <$> f l r xs'
takeBrace _ _ _ = Nothing

-- | Get an argument from list of `Token` (e.g: Given the token of @f(g(a, b)), 42@, return the token of @f(g(a, b))@).
readFn :: Eq i => [TokenIdx i] -> Maybe ([TokenIdx i], [TokenIdx i])
readFn = readFn' 0 (0 :: Int)
    where
        readFn' li ri (cur@(_, TKReserved ","):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li ri xs
        readFn' li ri (cur@(_, TKReserved ")"):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li (succ ri) xs
        readFn' li ri (cur@(_, TKReserved "("):xs) = first (cur:) <$> readFn' (succ li) ri xs
        readFn' li ri []
            | li == ri = Just ([], [])
            | otherwise = Nothing
        readFn' li ri (x:xs) = first (x:) <$> readFn' li ri xs

-- | Get arguments from list of `Token` (e.g: Given the token of @f(f(g(a, b)), 42);@, 
-- return expressions that are the token of "f(g(a, b))" and the token of "42".
takeExps :: Eq i => [TokenIdx i] -> Maybe [[TokenIdx i]]
takeExps ((_, TKIdent _):(_, TKReserved "("):xs) = flip (maybe Nothing) (lastInit ((==TKReserved ")") . snd) xs) $ fmap (filter (not . null)) . f
    where
        f [] = Just []
        f args = maybe Nothing (\(ex, ds) -> (ex:) <$> f ds) $ readFn args
takeExps _ = Nothing

-- | `makeTypes` returns a pair of type (including pointer type) and the remaining tokens wrapped in 
-- `Just` only if the token starts with `TKType`.
-- Otherwise `Nothing` is returned.
makeTypes :: Eq i => [TokenIdx i] -> Maybe (CR.TypeKind, [TokenIdx i])
makeTypes ((_, TKType tktype):xs) = Just $ first (flip CR.makePtr tktype . toNatural) $ dropSnd $ spanLen ((==TKReserved "*") . snd) xs
makeTypes _ = Nothing

-- | For a number \(n\in\mathbb{R}\), let \(k\) be the number of consecutive occurrences of
-- @TKReserved "[", n, TKReserved "]"@ from the beginning of the token sequence.
-- `arrayDeclSuffix` constructs an array type of the given type @t@ based on 
-- the token sequence if \(k\leq 1\), wraps it in `Right` and `Just` and returns it with the rest of the token sequence.
-- If the token @TKReserved "["@ exists at the beginning of the token sequence, 
-- but the subsequent token sequence is invalid as an array declaration in C programming language,
-- an error mesage and the token at the error location are returned wrapped in
-- `Left` and `Just`. When \(k=0\), `Nothing` is returned.
arrayDeclSuffix :: forall i. (Integral i, Show i) => CR.TypeKind -> [TokenIdx i] -> Maybe (Either (T.Text, TokenIdx i) (CR.TypeKind, [TokenIdx i]))
arrayDeclSuffix t ((_, TKReserved "["):(_, TKNum n):(_, TKReserved "]"):xs) = flip (maybe (Just $ Right (CR.CTArray (toNatural n) t, xs))) (arrayDeclSuffix t xs) $
    Just . fmap (first $ fromJust . CR.concatCTArray (CR.CTArray (toNatural n) t))
arrayDeclSuffix _ ((_, TKReserved "["):cur@(_, TKNum n):xs) = let mes = "expected ']' " in 
    Just $ Left $ if null xs then (mes <> "after '" <> tshow n <> "' token", cur) else (mes <> "before '" <> tshow (head xs) <> "' token", head xs)
arrayDeclSuffix _ (cur@(_, TKReserved "["):_) = Just $ Left ("expected storage size after '[' token", cur)
arrayDeclSuffix _ _ = Nothing

