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
    isTKFor,
    isTKIdent,
    isTKNum,
    isTKReserved,
    TokenFor (..),
    Token (..),
    tokenize
) where

import Data.Char (isDigit, isSpace)
import Data.List.Split (endBy)
import Data.Maybe (catMaybes)
import Data.Either (lefts, rights)

import qualified Htcc.CRules.Char as CRC

-- | Specially for token data type
data TokenFor i = TKForkw -- ^ The for keyword
    | TKForInit [Token i] -- ^ The initial section of for statement
    | TKForCond [Token i] -- ^ The conditional section of for statement
    | TKForIncr [Token i] -- ^ The incremental section of for statement
    deriving (Show, Eq)

-- | Token type
data Token i = TKReserved String -- ^ The reserved token
    | TKNum i -- ^ The number data
    | TKIdent String -- ^ The identifier
    | TKReturn -- ^ The return keyword
    | TKIf -- ^ The if keyword
    | TKElse -- ^ The else keyword
    | TKWhile -- ^ The while keyword
    | TKFor (TokenFor i) -- ^ The for keyword
    deriving (Show, Eq)

{-# INLINE isTKFor #-}
-- | Utility for `TKFor`. When the argument is `TKFor`, it returns `True`, otherwise `False`.
isTKFor :: Token i -> Bool
isTKFor (TKFor _) = True
isTKFor _ = False

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

-- | Utility for `TKReserved`. When the argument is `TKReserved`, it returns `True`, otherwise `False`.
{-# INLINE isTKReserved #-}
isTKReserved :: Token i -> Bool
isTKReserved (TKReserved _) = True
isTKReserved _ = False

{-# INLINE charOps #-}
charOps :: String
charOps = "+-*/()<>=;{},&|^%!~"

{-# INLINE strOps #-}
strOps :: [String]
strOps = [
    "<=",
    ">=",
    "==",
    "!="
    ]

{-# INLINE forSect #-}
forSect :: String -> (Int, String)
forSect xs = let ds = dropWhile isSpace xs; ts = takeWhile (/=')') ds in (length (takeWhile isSpace xs) + length ts + 1, tail ts)


-- | Tokenize from `String`. If it fails, the Left that wraps the value representing that point is returned.
tokenize :: Read i => String -> Either Int [Token i]
tokenize = tokenize' 0
    where
        tokenize' _ [] = Right []
        tokenize' n xxs@(x:xs)
            | isDigit x = let ts = takeWhile isDigit xs in 
                (TKNum (read (x:ts)) :) <$> tokenize' (succ (n + length ts)) (drop (length ts) xs)
            | isSpace x = tokenize' (succ n) xs
            | not (null xs) && [x, head xs] `elem` strOps = (TKReserved [x, head xs]:) <$> tokenize' (n + 2) (tail xs)
            | x `elem` charOps = (TKReserved [x]:) <$> tokenize' (succ n) xs
            | otherwise = let tk = takeWhile CRC.isValidChar xxs in 
                if null tk then Left n else let len = length tk; dxxs = drop len xxs; cn = callNext tk (n + len) dxxs in case tk of
                    "return" -> cn TKReturn
                    "while" -> cn TKWhile 
                    "if" -> cn TKIf
                    "else" -> (if not (null dxxs) && isSpace (head dxxs) then (TKElse:) else (TKIdent tk:)) <$> tokenize' (succ (len + n)) dxxs
                    "for" -> 
                        let (fslen, fs) = forSect dxxs
                            forSts = catMaybes $ zipWith (\ts y -> if null ts then Nothing else Just $ TKFor . y <$> tokenize' n ts) (endBy ";" fs) [TKForInit, TKForCond, TKForIncr]
                            lef = lefts forSts in
                                if null lef then ((TKFor TKForkw:rights forSts) ++) <$> tokenize' (succ (fslen + n)) (drop fslen dxxs) else Left $ head lef
                    _ -> (TKIdent tk :) <$> tokenize' (succ (n + len)) dxxs
            where
                callNext tk nlen dxxs itk = (if not (null dxxs) && (isSpace (head dxxs) || '(' == head dxxs) then (itk:) else (TKIdent tk:)) <$> tokenize' (succ nlen) dxxs
