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
    TokenFor (..),
    Token (..),
    -- * Tokenizer
    tokenize,
    -- * Helper functions
    isTKFor,
    isTKIdent,
    isTKNum,
    isTKReserved
) where

import Data.Bool (bool)
import Data.Char (isDigit, isSpace)
import Data.List.Split (endBy)
import Data.Maybe (catMaybes)
import Data.Either (lefts, rights)
import qualified Data.Text as T
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Control.Monad.ST (runST)
import Control.Monad (zipWithM)

import qualified Htcc.CRules.Char as CRC
import qualified Htcc.ErrorTrack as E
import Htcc.Utils (takeWhileLen, spanLen)

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

{-# INLINE isTKReserved #-}
-- | Utility for `TKReserved`. When the argument is `TKReserved`, it returns `True`, otherwise `False`.
isTKReserved :: Token i -> Bool
isTKReserved (TKReserved _) = True
isTKReserved _ = False

{-# INLINE charOps #-}
-- | Valid one characters as C language
charOps :: String
charOps = "+-*/()<>=;{},&|^%!~"

{-# INLINE strOps #-}
-- | Valid two characters as C language
strOps :: [String]
strOps = [
    "<=",
    ">=",
    "==",
    "!=",
    "<<",
    ">>"
    ]

-- | Tokenize @for@ statement
forSt :: (Integral i, Read a) => E.ErrTracker i -> String -> Either (E.ErrTracker i) [Token a]
forSt n xs = runST $ do
    ern <- newSTRef n
    c <- fmap catMaybes $ flip (`zipWithM` (endBy ";" fs)) [TKForInit, TKForCond, TKForIncr] $ \ts y ->     
        modifySTRef ern (E.advance $ succ $ length ts) *>
        flip bool (return Nothing) (Just . fmap (TKFor . y) . flip tokenize' ts <$> readSTRef ern) (null ts)
    let lef = lefts c
    return (if null lef then ((TKFor TKForkw:rights c) ++) <$> tokenize' (E.advance (succ fslen) n) (drop fslen xs) else Left $ head lef)
    where
        (fslen, fs) = let (n', _, ds) = spanLen isSpace xs; (n'', ts) = takeWhileLen (/=')') ds in (succ $ n' + n'', tail ts)
        
-- | Core function of `tokenize`
tokenize' :: forall i a. (Integral i, Read a) => E.ErrTracker i -> String -> Either (E.ErrTracker i) [Token a]
tokenize' _ [] = Right []
tokenize' n xxs@(x:xs)
    | isDigit x = let (n', ts, ds) = spanLen isDigit xs in (TKNum (read (x:ts)) :) <$> tokenize' (E.advance (succ n') n) ds
    | isSpace x = tokenize' (succ n) xs
    | not (null xs) && [x, head xs] `elem` strOps = (TKReserved [x, head xs]:) <$> tokenize' (E.advance (2 :: i) n) (tail xs)
    | x `elem` charOps = (TKReserved [x]:) <$> tokenize' (succ n) xs
    | otherwise = let (len, tk, ds) = spanLen CRC.isValidChar xxs in 
        if null tk then Left (n { E.str = T.pack $ takeWhile (not . CRC.isValidChar) ds }) else let cn = callNext tk (E.advance len n) ds in case tk of
            "return" -> cn TKReturn
            "while" -> cn TKWhile 
            "if" -> cn TKIf
            "else" -> (if not (null ds) && isSpace (head ds) then (TKElse:) else (TKIdent tk:)) <$> tokenize' (E.advance len n) ds
            "for" -> forSt (E.advance len n) ds
            _ -> (TKIdent tk:) <$> tokenize' (E.advance (succ len) n) ds
        where
            -- | Partial function for reading the next token (@while@, @if@, @return@).
            callNext tk nlen ds itk = (if not (null ds) && (isSpace (head ds) || '(' == head ds) then (itk:) else (TKIdent tk:)) <$> tokenize' (succ nlen) ds

-- | Tokenize from `String`. If it fails, the Left that wraps the value representing that point is returned.
tokenize :: (Integral i, Read a) => String -> Either (E.ErrTracker i) [Token a]
tokenize = tokenize' E.init
