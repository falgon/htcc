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
    TokenLCNums (..),
    TokenLC,
    Token (..),
    -- * Tokenizer
    tokenize,
    -- * Utilities for accessing to token data
    length,
    isTKNum,
    isTKType,
    isTKIdent,
    isTKReserved
) where

import Prelude hiding (length)
import qualified Prelude as P (length)
import qualified Data.ByteString as B
import Data.Char (isDigit, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Map as M
import Data.Tuple.Extra (first)
import Data.List (find)

import qualified Htcc.CRules as CR
import Htcc.Utils (spanLenT, subTextIndex, dropSnd, first3, tshow, isStrictSpace, lor)

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

{-# INLINE length #-}
-- | `length` returns the token length
length :: Show i => Token i -> Int
length (TKReserved s) = T.length s
length (TKNum i) = P.length $ show i
length (TKIdent i) = T.length i
length TKReturn = 6
length TKIf = 2
length TKElse = 4
length TKWhile = 5
length TKFor = 3
length TKSizeof = 6
length (TKType tk) = P.length $ show tk
length (TKString s) = B.length s
length TKEmpty = 0

-- | Lookup keyword from `T.Text`. If the specified `T.Text` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: Show i => T.Text -> Maybe (Token i)
lookupKeyword s = find ((==) s . tshow) [TKReturn, TKWhile, TKIf, TKElse, TKFor, TKSizeof, TKType CR.CTInt, TKType CR.CTChar]

-- | `TokenLCNums` is data structure for storing the line number and character number of each token
data TokenLCNums i = TokenLCNums -- ^ The constructor of `TokenLCNums`
    { 
        tkLn :: i, -- ^ line number
        tkCn :: i -- ^ character number
    } deriving Eq

instance Show i => Show (TokenLCNums i) where
    show (TokenLCNums ln cn) = show ln ++ ":" ++ show cn

-- | `Token` and its `TokenLCNums`.
type TokenLC i = (TokenLCNums i, Token i)

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
        | x == '\\' && not (T.null xs) -> maybe (escapeChar xs) (`T.cons` escapeChar (T.tail xs)) (M.lookup (T.head xs) mp)
        | otherwise -> T.cons x $ escapeChar xs
    _ -> T.empty
    where
        mp = M.fromList [('\\', '\\'), ('a', '\a'), ('b', '\b'), ('t', '\t'), ('n', '\n'), ('v', '\v'), ('f', '\f'), ('r', '\r'), ('e', chr 27), ('0', '\0')]


-- | The core function of `tokenize`
tokenize' :: (Integral i, Read i, Show i) => TokenLCNums i -> T.Text -> Either (TokenLCNums i, T.Text) [TokenLC i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd $ spanLenT isStrictSpace xs
    where
        f n' (rssize, xxs) = case T.uncons xxs of
            Just (x, xs')
                | lor [(=='\n'), (=='\r')] x -> tokenize' (TokenLCNums (succ $ tkLn n') 1) xs' -- for new line
                | not (T.null xs') && x == '/' && T.head xs' == '/' -> tokenize' (TokenLCNums (succ $ tkLn n') 1) $ T.dropWhile (/='\n') (T.tail xs') -- for line comment
                | not (T.null xs') && x == '/' && T.head xs' == '*' -> let xs'' = T.tail xs'; cur = n' { tkCn = rssize + tkCn n' } in -- for block comment
                    flip (maybe (Left (cur, "*/"))) (subTextIndex "*/" xs'') $ \ind -> let next = n' { tkCn = tkCn cur + fromIntegral ind + 2 } in tokenize' next $ T.drop (ind + 3) xs''
                | isDigit x -> let (n'', ts, ds) = first3 fromIntegral $ spanLenT isDigit xs'; cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = tkCn cur + n'' }; num = T.cons x ts in
                    flip (either (const $ Left (cur, T.singleton x))) (T.decimal num) $ \(nu, _) -> ((cur, TKNum nu):) <$> tokenize' next ds
                | x == '"' -> let cur = n' { tkCn = rssize + tkCn n' } in flip (maybe (Left (cur, "\""))) (T.findIndex (=='"') xs') $ \ind -> 
                    let (ts, ds) = T.splitAt ind xs'; next = n' { tkCn = 2 + tkCn cur + fromIntegral ind } in 
                        ((cur, TKString (T.encodeUtf8 $ T.append (escapeChar ts) "\0")):) <$> tokenize' next (T.tail ds)
                | not (T.null xs') && T.take 2 xxs `elem` CR.strOps -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = 2 + tkCn cur }; op = T.take 2 xxs in
                    ((cur, TKReserved op):) <$> tokenize' next (T.tail xs')
                | x `elem` CR.charOps -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = succ (tkCn cur) } in ((cur, TKReserved (T.singleton x)):) <$> tokenize' next xs'
                | otherwise -> let (len, tk, ds) = spanLenT CR.isValidChar xxs; cur = n' { tkCn = tkCn n' + rssize } in
                    if len == 0 then Left (cur, T.takeWhile (not . CR.isValidChar) ds) else let next = n' { tkCn = tkCn cur + fromIntegral len } in
                        maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk
            _ -> Right []

-- | Tokenize the `T.Text`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, @[TokenIdx i]@ is returned.
tokenize :: (Integral i, Read i, Show i) => T.Text -> Either (TokenLCNums i, T.Text) [TokenLC i]
tokenize = tokenize' (TokenLCNums 1 1)
