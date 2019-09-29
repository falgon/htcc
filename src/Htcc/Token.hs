{-|
Module      : Htcc.Token
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Token (
    -- * Token data types
    Token (..),
    TokenIdx,
    -- * Tokenizer
    tokenize,
    -- * Helper functions
    takeBrace,
    takeExps,
    isTKIdent,
    isTKNum,
    isTKReserved,
    isTKType,
    makeTypes,
    arrayDeclSuffix
) where

import qualified Data.ByteString as B
import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Extra (first)
import Data.List (find, elemIndex, splitAt)
import Data.Maybe (fromJust)

import qualified Htcc.CRules as CR
import Htcc.Utils (lastInit, spanLen, dropSnd, first3, tshow, toNatural)

-- | Token type
data Token i = TKReserved String -- ^ The reserved token
    | TKNum i -- ^ The number data
    | TKIdent String -- ^ The identifier
    | TKReturn -- ^ The @return@ keyword
    | TKIf -- ^ The @if@ keyword
    | TKElse -- ^ The @else@ keyword
    | TKWhile -- ^ The @while@ keyword
    | TKFor -- ^ The @for@ keyword
    | TKSizeof -- ^ The @sizeof@ keyword
    | TKType CR.TypeKind -- ^ Types
    | TKString B.ByteString -- ^ The string literal
    | TKEmpty -- ^ The empty token (This is not used by `tokenize`, but when errors are detected during parsing, the token at error locations cannot be specified)
    deriving Eq

instance Show i => Show (Token i) where
    show (TKReserved s) = s
    show (TKNum i) = show i
    show (TKIdent s) = s
    show TKReturn = "return"
    show TKIf = "if"
    show TKElse = "else"
    show TKWhile = "while"
    show TKFor = "for"
    show TKSizeof = "sizeof"
    show (TKType x) = show x
    show (TKString s) = "\"" ++ T.unpack (T.decodeUtf8 s) ++ "\""
    show TKEmpty = ""

-- | Lookup keyword from `String`. If the specified `String` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: Show i => String -> Maybe (Token i)
lookupKeyword s = find ((==) s . show) [TKReturn, TKWhile, TKIf, TKElse, TKFor, TKSizeof, TKType CR.CTInt, TKType CR.CTChar]

-- | `Token` and its index.
type TokenIdx i = (i, Token i)

{-# INLINE isTKIdent #-}
-- | Utility for `TKIdent`. When the argument is `TKIdent`, it returns `True`, otherwise `False`.
isTKIdent :: Token i -> Bool
isTKIdent (TKIdent _) = True
isTKIdent _ = False

{-# INLINE isTKNum #-}
-- | Utility for `TKNum`. When the argument is `TKNum`, it returns `True`, otherwise `False`.
isTKNum :: Token i -> Bool
isTKNum (TKNum _) = True
isTKNum _ = False

{-# INLINE isTKReserved #-}
-- | Utility for `TKReserved`. When the argument is `TKReserved`, it returns `True`, otherwise `False`.
isTKReserved :: Token i -> Bool
isTKReserved (TKReserved _) = True
isTKReserved _ = False

{-# INLINE isTKType #-}
-- | Utility for `TKType`. When the argument is `TKType`, it returns `True`, otherwise `False`.
isTKType :: Token i -> Bool
isTKType (TKType _) = True
isTKType _ = False

-- | The core function of `tokenize`
tokenize' :: (Integral i, Read i, Show i) => i -> String -> Either (i, T.Text) [TokenIdx i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd $ spanLen isSpace xs
    where
        f _ (_, []) = Right []
        f n' (rssize, xxs@(x:xs'))
            | isDigit x = let (n'', ts, ds) = first3 fromIntegral $ spanLen isDigit xs'; cur = rssize + n'; next = succ cur + n''; num = x:ts in 
                ((cur, TKNum $ read num):) <$> tokenize' next ds
            | x == '"' = let cur = rssize + n' in flip (maybe (Left (cur, "\""))) (elemIndex '"' xs') $ \ind -> let (ts, ds) = splitAt ind xs'; next = 2 + cur + fromIntegral ind in
                ((cur, TKString (T.encodeUtf8 $ T.pack (ts ++ "\0"))):) <$> tokenize' next (tail ds)
            | not (null xs') && [x, head xs'] `elem` CR.strOps = let cur = rssize + n'; next = cur + 2; op = [x, head xs'] in
                ((cur, TKReserved op):) <$> tokenize' next (tail xs')
            | x `elem` CR.charOps = let cur = rssize + n'; next = succ cur in ((cur, TKReserved [x]):) <$> tokenize' next xs'
            | otherwise = let (len, tk, ds) = spanLen CR.isValidChar xxs; cur = n' + rssize in 
                if len == 0 then Left (cur, T.pack $ takeWhile (not . CR.isValidChar) ds) else let next = cur + fromIntegral len in 
                    maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk

-- | Tokenize the `String`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, @[TokenIdx i]@ is returned.
tokenize :: (Integral i, Read i, Show i) => String -> Either (i, T.Text) [TokenIdx i]
tokenize = tokenize' 1

-- | Extract the partial token enclosed in parentheses from the token sequence. If it is invalid, `takeBrace` returns @(i, Text)@ indicating the error location.
-- Otherwise, `takeBrace` returns a partial token enclosed in parentheses and subsequent tokens.
takeBrace :: forall i. (Integral i, Read i, Show i) => String -> String -> [TokenIdx i] -> Maybe (Either (TokenIdx i) ([TokenIdx i], [TokenIdx i]))
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

