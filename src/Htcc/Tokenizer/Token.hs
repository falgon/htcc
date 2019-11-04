{-|
Module      : Htcc.Tokenizer.Token
Description : Types used in lexical analysis and their utility functions
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Types used in lexical analysis and their utility functions
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric #-}
module Htcc.Tokenizer.Token (
    -- * Token data types
    TokenLCNums (..),
    TokenLC,
    Token (..),
    -- * Utilities for accessing to token data
    length,
    emptyToken,
    isTKNum,
    isTKType,
    isTKStruct,
    isTKEnum,
    isTKIdent,
    isTKReserved,
    spanStrLiteral,
    spanCharLiteral,
    lookupKeyword
) where

import Prelude hiding (length)
import GHC.Generics (Generic, Generic1)
import qualified Prelude as P (length)

import Control.DeepSeq (NFData (..), NFData1 (..))
import qualified Data.ByteString as B
import Data.Char (isDigit, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Data.Tuple.Extra (first, second)
import Data.List (find)

import qualified Htcc.CRules as CR
import Htcc.Utils (spanLen, dropFst3, tshow, maybe')
import Htcc.CRules.Types.StorageClass

-- | Token type
data Token i = TKReserved T.Text -- ^ The reserved token
    | TKNum !i -- ^ The number data
    | TKIdent T.Text -- ^ The identifier
    | TKReturn -- ^ The @return@ keyword
    | TKIf -- ^ The @if@ keyword
    | TKElse -- ^ The @else@ keyword
    | TKWhile -- ^ The @while@ keyword
    | TKFor -- ^ The @for@ keyword
    | TKEnum -- ^ The @enum@ keyword
    | TKSizeof -- ^ The @sizeof@ keyword
    | TKAlignof -- ^ The @_Alignof@ keyword
    | TKStruct -- ^ The @struct@ keyword
    | TKType (CR.TypeKind i) -- ^ Types
    | TKTypedef -- ^ The @typedef@ keyword
    | TKString B.ByteString -- ^ The string literal
    | TKEmpty -- ^ The empty token (This is not used by `Htcc.Tokenizer.Core.tokenize`, but when errors are detected during parsing, the token at error locations cannot be specified)
    deriving (Eq, Generic)

instance NFData i => NFData (Token i)

instance Show i => Show (Token i) where
    show (TKReserved s) = T.unpack s
    show (TKNum i) = show i
    show (TKIdent s) = T.unpack s
    show TKReturn = "return"
    show TKIf = "if"
    show TKElse = "else"
    show TKWhile = "while"
    show TKFor = "for"
    show TKEnum = "enum"
    show TKStruct = "struct"
    show TKSizeof = "sizeof"
    show TKAlignof = "_Alignof"
    show TKTypedef = "typedef"
    show (TKType x) = show x
    show (TKString s) = "\"" ++ T.unpack (T.decodeUtf8 s) ++ "\""
    show TKEmpty = ""

instance Read i => Read (Token i) where
    readsPrec _ xxs@(x:xs) 
        | isDigit x = [first (TKNum . (read :: String -> i) . (x:)) $ dropFst3 $ spanLen isDigit xs]
        | x == '\"' = [maybe' (error "No parse: string literal was not closed") (spanStrLiteral $ T.pack xs) $ first (TKString . T.encodeUtf8 . flip T.append "\0") . second T.unpack]
        | P.length xxs > 2 && T.pack (take 3 xxs) `elem` CR.strOps3 = [(TKReserved $ T.pack $ take 3 xxs, drop 3 xxs)]
        | not (null xs) && T.pack (take 2 xxs) `elem` CR.strOps2 = [(TKReserved $ T.pack $ take 2 xxs, drop 2 xxs)]
        | x `elem` CR.charOps = [(TKReserved (T.singleton x), xs)]
        | otherwise = [first (TKIdent . T.pack) $ dropFst3 $ spanLen CR.isValidChar xxs]
    readsPrec _ _ = [(TKEmpty, [])]

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
length TKEnum = 4
length TKStruct = 6
length TKSizeof = 6
length TKAlignof = 8
length TKTypedef = 7
length (TKType tk) = P.length $ show tk
length (TKString s) = B.length s
length TKEmpty = 0

-- | Lookup keyword from `T.Text`. If the specified `T.Text` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: Show i => T.Text -> Maybe (Token i)
lookupKeyword s = find ((==) s . tshow) [
    TKReturn, 
    TKWhile,
    TKIf, 
    TKElse,
    TKFor,
    TKEnum,
    TKStruct,
    TKSizeof,
    TKAlignof, 
    TKTypedef, 
    TKType CR.CTInt,
    TKType CR.CTChar, 
    TKType $ CR.CTSigned CR.CTUndef,
    TKType $ CR.CTShort CR.CTUndef,
    TKType $ CR.CTLong CR.CTUndef,
    TKType CR.CTVoid, 
    TKType CR.CTBool,
    TKReserved $ T.pack $ show SCAuto,
    TKReserved $ T.pack $ show SCStatic,
    TKReserved $ T.pack $ show SCRegister
    ]

-- | `TokenLCNums` is data structure for storing the line number and character number of each token
data TokenLCNums i = TokenLCNums -- ^ The constructor of `TokenLCNums`
    { 
        tkLn :: !i, -- ^ line number
        tkCn :: !i -- ^ character number
    } deriving (Eq, Generic, Generic1)

instance Show i => Show (TokenLCNums i) where
    show (TokenLCNums ln cn) = show ln ++ ":" ++ show cn

instance NFData i => NFData (TokenLCNums i)
instance NFData1 TokenLCNums

-- | `Htcc.Tokenizer.Token.Token` and its `TokenLCNums`.
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

{-# INLINE isTKStruct #-}
-- | Utility for `TKStruct`. When the argument is `TKStruct`, it returns `True`, otherwise `False`.
isTKStruct :: Token i -> Bool
isTKStruct TKStruct = True
isTKStruct _ = False

{-# INLINE isTKEnum #-}
-- | Utility for `TKEnum`. When the argument is `TKEnum`, it returns `True`, otherwise `False`.
isTKEnum :: Token i -> Bool
isTKEnum TKEnum = True
isTKEnum _ = False

-- `Htcc.Tokenizer.Token.escapeChar` converts escape characters in the input `T.Text` to correct escape characters
escapeChar :: T.Text -> T.Text
escapeChar xxs = case T.uncons xxs of
    Just (x, xs)
        | x == '\\' && not (T.null xs) -> maybe (escapeChar xs) (`T.cons` escapeChar (T.tail xs)) $ M.lookup (T.head xs) mp
        | otherwise -> T.cons x $ escapeChar xs
    _ -> T.empty
    where
        mp = M.fromList [
            ('\\', '\\'), 
            ('a', '\a'), 
            ('b', '\b'), 
            ('t', '\t'),
            ('n', '\n'), 
            ('v', '\v'), 
            ('f', '\f'), 
            ('r', '\r'), 
            ('e', chr 27), 
            ('0', '\0')]

spanLiteral :: Char -> T.Text -> Maybe (T.Text, T.Text)
spanLiteral c ts = first escapeChar <$> f ts
    where
        f ts' = case T.uncons ts' of
            Just (x, xs)
                | x == '\\' && not (T.null xs) && T.head xs == '"' -> first (T.cons '"') <$> f (T.tail xs)
                | x == '\\' && not (T.null xs) && T.head xs == '\\' -> first (T.append "\\\\") <$> f (T.tail xs)
                | x == c -> Just (T.empty, xs)
                | otherwise -> first (T.cons x) <$> f xs
            Nothing -> Nothing


-- | `spanStrLiteral` separate the string literal part and the non-string literal part from the input text
spanStrLiteral :: T.Text -> Maybe (T.Text, T.Text)
spanStrLiteral = spanLiteral '"'


-- | `spanCharLiteral` separate the string literal part and the non-string literal part from the input text
spanCharLiteral :: T.Text -> Maybe (T.Text, T.Text)
spanCharLiteral = spanLiteral '\''

-- | `emptyToken` is used when it cannot be referenced
emptyToken :: Num i => TokenLC i
emptyToken = (TokenLCNums 0 0, TKEmpty)
