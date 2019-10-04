{-|
Module      : Htcc.Token.Core
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Token.Core (
    -- * Token data types
    Token (..),
    TokenIdx,
    -- * Tokenizer
    tokenize,
    -- * Utilities for accessing to token data
    isTKNum,
    isTKType,
    isTKIdent,
    isTKReserved
) where

import qualified Data.ByteString as B
import Data.Char (isDigit, isSpace, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Map as M
import Data.Tuple.Extra (first)
import Data.List (find)

import qualified Htcc.CRules as CR
import Htcc.Utils (spanLenT, dropSnd, first3, tshow)

-- | Token type
data Token i = TKReserved T.Text -- ^ The reserved token
    | TKNum i -- ^ The number data
    | TKIdent T.Text -- ^ The identifier
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
    show (TKReserved s) = T.unpack s
    show (TKNum i) = show i
    show (TKIdent s) = T.unpack s
    show TKReturn = "return"
    show TKIf = "if"
    show TKElse = "else"
    show TKWhile = "while"
    show TKFor = "for"
    show TKSizeof = "sizeof"
    show (TKType x) = show x
    show (TKString s) = "\"" ++ T.unpack (T.decodeUtf8 s) ++ "\""
    show TKEmpty = ""

-- | Lookup keyword from `T.Text`. If the specified `T.Text` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: Show i => T.Text -> Maybe (Token i)
lookupKeyword s = find ((==) s . tshow) [TKReturn, TKWhile, TKIf, TKElse, TKFor, TKSizeof, TKType CR.CTInt, TKType CR.CTChar]

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

-- | `escapeChar` converts escape characters in the input `T.Text` to correct escape characters
escapeChar :: T.Text -> T.Text
escapeChar xxs = case T.uncons xxs of
    Just (x, xs)
        | x == '\\' && not (T.null xs) -> maybe (escapeChar xs) (flip T.cons (escapeChar (T.tail xs))) (M.lookup (T.head xs) mp)
        | otherwise -> T.cons x $ escapeChar xs
    _ -> T.empty
    where
        mp = M.fromList [('\\', '\\'), ('a', '\a'), ('b', '\b'), ('t', '\t'), ('n', '\n'), ('v', '\v'), ('f', '\f'), ('r', '\r'), ('e', chr 27), ('0', '\0')]


-- | The core function of `tokenize`
{-
tokenize' :: (Integral i, Read i, Show i) => i -> T.Text -> Either (i, T.Text) [TokenIdx i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd $ spanLen isSpace xs
    where
        f _ (_, []) = Right []
        f n' (rssize, xxs@(x:xs'))
            | isDigit x = let (n'', ts, ds) = first3 fromIntegral $ spanLen isDigit xs'; cur = rssize + n'; next = succ cur + n''; num = x:ts in 
                ((cur, TKNum $ read num):) <$> tokenize' next ds
            | x == '"' = let cur = rssize + n' in flip (maybe (Left (cur, "\""))) (elemIndex '"' xs') $ \ind -> let (ts, ds) = splitAt ind xs'; next = 2 + cur + fromIntegral ind in
                ((cur, TKString (T.encodeUtf8 $ T.pack (escapeChar ts ++ "\0"))):) <$> tokenize' next (tail ds)
            | not (null xs') && [x, head xs'] `elem` CR.strOps = let cur = rssize + n'; next = cur + 2; op = [x, head xs'] in
                ((cur, TKReserved op):) <$> tokenize' next (tail xs')
            | x `elem` CR.charOps = let cur = rssize + n'; next = succ cur in ((cur, TKReserved [x]):) <$> tokenize' next xs'
            | otherwise = let (len, tk, ds) = spanLen CR.isValidChar xxs; cur = n' + rssize in 
                if len == 0 then Left (cur, T.pack $ takeWhile (not . CR.isValidChar) ds) else let next = cur + fromIntegral len in 
                    maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk
-}

tokenize' :: (Integral i, Read i, Show i) => i -> T.Text -> Either (i, T.Text) [TokenIdx i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd $ spanLenT isSpace xs
    where
        f n' (rssize, xxs) = case T.uncons xxs of
            Just (x, xs')
                | isDigit x -> let (n'', ts, ds) = first3 fromIntegral $ spanLenT isDigit xs'; cur = rssize + n''; next = succ cur + n''; num = T.cons x ts in
                    flip (either (const $ Left (cur, T.singleton x))) (T.decimal num) $ \(nu, _) -> ((cur, TKNum nu):) <$> tokenize' next ds
                | x == '"' -> let cur = rssize + n' in flip (maybe (Left (cur, "\""))) (T.findIndex (=='"') xs') $ \ind -> let (ts, ds) = T.splitAt ind xs'; next = 2 + cur + fromIntegral ind in
                    ((cur, TKString (T.encodeUtf8 $ T.append (escapeChar ts) "\0")):) <$> tokenize' next (T.tail ds) 
                | not (T.null xs') && (T.take 2 xxs) `elem` CR.strOpsT -> let cur = rssize + n'; next = 2 + cur; op = T.take 2 xxs in
                    ((cur, TKReserved op):) <$> tokenize' next (T.tail xs')
                | elem x CR.charOps -> let cur = rssize + n'; next = succ cur in ((cur, TKReserved (T.singleton x)):) <$> tokenize' next xs'
                | otherwise -> let (len, tk, ds) = spanLenT CR.isValidChar xxs; cur = n' + rssize in
                    if len == 0 then Left (cur, T.takeWhile (not . CR.isValidChar) ds) else let next = cur + fromIntegral len in
                        maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk
            _ -> Right []

-- | Tokenize the `T.Text`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, @[TokenIdx i]@ is returned.
tokenize :: (Integral i, Read i, Show i) => T.Text -> Either (i, T.Text) [TokenIdx i]
tokenize = tokenize' 1
