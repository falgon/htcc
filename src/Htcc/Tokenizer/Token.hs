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
{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables,
             TupleSections #-}
module Htcc.Tokenizer.Token (
    -- * Token data types
    TokenLCNums (..),
    TokenLC,
    Token (..),
    -- * Utilities for accessing to token data
    keywordsTokens,
    length,
    emptyToken,
    isTKNum,
    isTKType,
    isTKStruct,
    isTKEnum,
    isTKIdent,
    isTKReserved,
    isTKMacro,
    isTKString,
    spanStrLiteral,
    spanCharLiteral,
    spanIntLit,
    lookupKeyword,
    altEmptyToken
) where

import           Control.DeepSeq                      (NFData (..),
                                                       NFData1 (..))
import qualified Data.ByteString                      as B
import           Data.Char                            (chr, isDigit, ord)
import           Data.List                            (find)
import qualified Data.Map                             as M
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Tuple.Extra                     (first, second)
import           GHC.Generics                         (Generic, Generic1)
import           Numeric                              (readDec, readHex,
                                                       readInt, readOct)
import           Numeric.Natural
import           Prelude                              hiding (length)
import qualified Prelude                              as P (length)

import qualified Htcc.CRules                          as CR
import qualified Htcc.CRules.Preprocessor.Punctuators as CP
import           Htcc.Utils                           (dropFst3, lor, maybe',
                                                       spanLen, tshow)

-- | Token type
data Token i = TKReserved T.Text -- ^ The reserved token
    | TKNum !i -- ^ The number data
    | TKIdent T.Text -- ^ The identifier
    | TKReturn -- ^ The @return@ keyword
    | TKIf -- ^ The @if@ keyword
    | TKSwitch -- ^ The @switch@ keyword
    | TKCase -- ^ The @case@ keyword
    | TKDefault -- ^ The @default@ keyword
    | TKElse -- ^ The @else@ keyword
    | TKWhile -- ^ The @while@ keyword
    | TKFor -- ^ The @for@ keyword
    | TKBreak -- ^ The @break@ keyword
    | TKContinue -- ^ The @continue@ keyword
    | TKEnum -- ^ The @enum@ keyword
    | TKSizeof -- ^ The @sizeof@ keyword
    | TKAlignof -- ^ The @_Alignof@ keyword
    | TKStruct -- ^ The @struct@ keyword
    | TKGoto -- ^ THe @goto@ keyword
    | TKType (CR.StorageClass i) -- ^ Types
    | TKTypedef -- ^ The @typedef@ keyword
    | TKString B.ByteString -- ^ The string literal
    | TKMacro CP.Macros T.Text -- ^ The C macro
    | TKEmpty -- ^ The empty token (This is not used by `Htcc.Tokenizer.Core.tokenize`, but when errors are detected during parsing, the token at error locations cannot be specified)
    deriving (Eq, Generic)

instance NFData i => NFData (Token i)

instance Show i => Show (Token i) where
    show (TKReserved s) = T.unpack s
    show (TKNum i)      = show i
    show (TKIdent s)    = T.unpack s
    show TKReturn       = "return"
    show TKIf           = "if"
    show TKSwitch       = "switch"
    show TKCase         = "case"
    show TKDefault      = "default"
    show TKElse         = "else"
    show TKWhile        = "while"
    show TKFor          = "for"
    show TKBreak        = "break"
    show TKContinue     = "continue"
    show TKEnum         = "enum"
    show TKStruct       = "struct"
    show TKSizeof       = "sizeof"
    show TKGoto         = "goto"
    show TKAlignof      = "_Alignof"
    show TKTypedef      = "typedef"
    show (TKMacro m st) = ('#' : show m) ++ T.unpack st
    show (TKType x)     = show x
    show (TKString s)   = "\"" ++ T.unpack (T.decodeUtf8 s) ++ "\""
    show TKEmpty        = ""

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
length (TKNum i)      = P.length $ show i
length (TKIdent i)    = T.length i
length TKReturn       = 6
length TKIf           = 2
length TKSwitch       = 6
length TKCase         = 4
length TKDefault      = 7
length TKElse         = 4
length TKWhile        = 5
length TKBreak        = 5
length TKContinue     = 8
length TKFor          = 3
length TKEnum         = 4
length TKStruct       = 6
length TKSizeof       = 6
length TKAlignof      = 8
length TKTypedef      = 7
length TKGoto         = 4
length (TKType tk)    = P.length $ show tk
length (TKString s)   = B.length s
length (TKMacro m t)  = CP.length m + T.length t
length TKEmpty        = 0

keywordsTokens :: [Token i]
keywordsTokens = [
    TKReturn
  , TKWhile
  , TKIf
  , TKSwitch
  , TKCase
  , TKDefault
  , TKElse
  , TKFor
  , TKBreak
  , TKContinue
  , TKEnum
  , TKStruct
  , TKSizeof
  , TKGoto
  , TKAlignof
  , TKTypedef
  , TKType $ CR.SCUndef CR.CTInt
  , TKType $ CR.SCUndef CR.CTChar
  , TKType $ CR.SCUndef $ CR.CTSigned CR.CTUndef
  , TKType $ CR.SCUndef $ CR.CTShort CR.CTUndef
  , TKType $ CR.SCUndef $ CR.CTLong CR.CTUndef
  , TKType $ CR.SCUndef CR.CTVoid
  , TKType $ CR.SCUndef CR.CTBool
  , TKType $ CR.SCAuto CR.CTUndef
  , TKType $ CR.SCStatic CR.CTUndef
  , TKType $ CR.SCRegister CR.CTUndef
  ]

-- | Lookup keyword from `T.Text`. If the specified `T.Text` is not keyword as C language, `lookupKeyword` returns `Nothing`.
lookupKeyword :: forall i. (Show i) => T.Text -> Maybe (Token i)
lookupKeyword s = find ((==) s . tshow) keywordsTokens

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
isTKIdent _           = False

{-# INLINE isTKNum #-}
-- | Utility for `TKNum`. When the argument is `TKNum`, it returns `True`, otherwise `False`.
isTKNum :: Token i -> Bool
isTKNum (TKNum _) = True
isTKNum _         = False

{-# INLINE isTKReserved #-}
-- | Utility for `TKReserved`. When the argument is `TKReserved`, it returns `True`, otherwise `False`.
isTKReserved :: Token i -> Bool
isTKReserved (TKReserved _) = True
isTKReserved _              = False

{-# INLINE isTKType #-}
-- | Utility for `TKType`. When the argument is `TKType`, it returns `True`, otherwise `False`.
isTKType :: Token i -> Bool
isTKType (TKType _) = True
isTKType _          = False

{-# INLINE isTKStruct #-}
-- | Utility for `TKStruct`. When the argument is `TKStruct`, it returns `True`, otherwise `False`.
isTKStruct :: Token i -> Bool
isTKStruct TKStruct = True
isTKStruct _        = False

{-# INLINE isTKEnum #-}
-- | Utility for `TKEnum`. When the argument is `TKEnum`, it returns `True`, otherwise `False`.
isTKEnum :: Token i -> Bool
isTKEnum TKEnum = True
isTKEnum _      = False

{-# INLINE isTKMacro #-}
-- | Utility for `TKMacro`. When the argument is `TKMacro`, it returns `True`, otherwise `False`.
isTKMacro :: Token i -> Bool
isTKMacro (TKMacro _ _) = True
isTKMacro _             = False

{-# INLINE isTKString #-}
-- | Utility for `TKString`. When the argument is `TKString`, it returns `True`, otherwise `False`.
isTKString :: Token i -> Bool
isTKString (TKString _) = True
isTKString _            = False

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


-- | Take the integer literal from given text.
spanIntLit :: (Eq i, Num i, Read i) => T.Text -> Maybe (Natural, Token i, T.Text)
spanIntLit ts = case T.uncons ts of
    Just (x, xs)
        | T.length xs > 1 && x == '0' && T.head xs == 'x' || T.head xs == 'X' -> let (ntk, ds) = T.span (\c -> isDigit c || 'a' <= c && 'f' >= c || 'A' <= c && 'F' >= c) (T.tail xs) in
            (fromIntegral $ T.length ntk,,ds) . TKNum <$> sh (readHex $ T.unpack ntk)
        | T.length xs > 1 && x == '0' && T.head xs == 'b' || T.head xs == 'B' -> let (ntk, ds) = T.span isDigit (T.tail xs) in
            (fromIntegral $ T.length ntk,,ds) . TKNum <$> sh (readBin $ T.unpack ntk)
        | x == '0' && not (T.null xs) -> let (ntk, ds) = T.span isDigit (x `T.cons` xs) in Just (fromIntegral $ T.length ntk, TKNum $ fst $ head $ readOct $ T.unpack ntk, ds)
        | isDigit x -> let (ntk, ds) = T.span isDigit (x `T.cons` xs) in Just (fromIntegral $ T.length ntk, TKNum $ fst $ head $ readDec $ T.unpack ntk, ds)
        | otherwise -> Nothing
    Nothing -> Nothing
    where
        readBin = readInt 2 (lor [(=='0'), (=='1')]) $ (+) (negate (ord '0')) . ord
        sh ys | null ys = Nothing | otherwise = Just $ fst $ head ys

-- | `emptyToken` is used when it cannot be referenced
emptyToken :: Num i => TokenLC i
emptyToken = (TokenLCNums 0 0, TKEmpty)

-- | `altEmptyToken` returns `emptyToken` if the first token is empty. Otherwise, returns the first token in the token sequence.
altEmptyToken :: Num i => [TokenLC i] -> TokenLC i
altEmptyToken []    = emptyToken
altEmptyToken (x:_) = x
