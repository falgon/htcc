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
{-# LANGUAGE ScopedTypeVariables #-}
module Htcc.Token (
    -- * Token data types
    Token (..),
    TokenIdx,
    -- * Tokenizer
    tokenize,
    -- * Helper functions
    takeBrace,
    isTKIdent,
    isTKNum,
    isTKReserved
) where

import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import Data.Tuple.Extra (first)
import Data.List (find)

import qualified Htcc.CRules as CR
import Htcc.Utils (spanLen, dropSnd, first3)

-- | Token type
data Token i = TKReserved String -- ^ The reserved token
    | TKNum i -- ^ The number data
    | TKIdent String -- ^ The identifier
    | TKReturn -- ^ The return keyword
    | TKIf -- ^ The if keyword
    | TKElse -- ^ The else keyword
    | TKWhile -- ^ The while keyword
    | TKFor -- ^ The for keyword
    | TKEmpty -- ^ The empty token (This is not used by tokenize, but when errors are detected during parsing, the token at error locations cannot be specified)
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
    show TKEmpty = ""

-- | Lookup keyword from `String`. If the specified `String` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: Show i => String -> Maybe (Token i)
lookupKeyword s = find ((==) s . show) [TKReturn, TKWhile, TKIf, TKElse, TKFor]

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

-- | The core function of `tokenize`
tokenize' :: (Integral i, Read i, Show i) => i -> String -> Either (i, T.Text) [TokenIdx i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd $ spanLen isSpace xs
    where
        f _ (_, []) = Right []
        f n' (rssize, xxs@(x:xs'))
            | isDigit x = let (n'', ts, ds) = first3 fromIntegral $ spanLen isDigit xs'; cur = rssize + n'; next = succ cur + n''; num = x:ts in 
                ((cur, TKNum $ read num):) <$> tokenize' next ds
            | not (null xs') && [x, head xs'] `elem` CR.strOps = let cur = rssize + n'; next = cur + 2; op = [x, head xs'] in
                ((cur, TKReserved op):) <$> tokenize' next (tail xs')
            | x `elem` CR.charOps = let cur = rssize + n'; next = succ cur in ((cur, TKReserved [x]):) <$> tokenize' next xs'
            | otherwise = let (len, tk, ds) = spanLen CR.isValidChar xxs; cur = n' + rssize in 
                if len == 0 then Left (cur, T.pack $ takeWhile (not . CR.isValidChar) ds) else let next = cur + fromIntegral len in 
                    maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk

-- | Tokenize the `String`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, `[TokenIdx]` is returned.
tokenize :: (Integral i, Read i, Show i) => String -> Either (i, T.Text) [TokenIdx i]
tokenize = tokenize' 1

-- | Extract the partial token enclosed in parentheses from the token sequence. If it is invalid, `takeBrace` returns @(i, Text)@ indicating the error location.
-- Otherwise, `takeBrace` returns a partial token enclosed in parentheses and subsequent tokens.
takeBrace :: forall i. (Integral i, Read i, Show i) => String -> String -> [TokenIdx i] -> Maybe (Either (TokenIdx i) ([TokenIdx i], [TokenIdx i]))
takeBrace leftb rightb xxs@((_, TKReserved y):_) 
    | y == leftb = Just $ f 0 0 xxs
    | otherwise = Nothing
    where
        f :: Int -> Int -> [TokenIdx i] -> Either {- (i, T.Text) -} (TokenIdx i) ([TokenIdx i], [TokenIdx i])
        f l r []
            | l /= r = Left $ head xxs -- (second tshow $ head xxs)
            | otherwise = Right ([], [])
        f l r (c@(p, TKReserved x):xs') 
            | x == rightb = if l == succ r then Right ([c], xs') else g l (succ r) xs'
            | x == leftb = if succ l == r then Right ([c], xs') else g (succ l) r xs'
            | otherwise = g l r xs'
            where
                g = (.) (fmap (first ((p, TKReserved x):)) .) . f
        f l r ((p, x):xs') = first ((:) (p, x)) <$> f l r xs'
takeBrace _ _ _ = Nothing
