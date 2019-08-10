{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-|
Module      : Htcc.CRules.Char
Description : Characters rules of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Characters rules of C language
-}
module Htcc.CRules.Char (
    -- * The definition of characters rules 
    isValidChar,
    -- * The helper class for some string types
    GenericStr (..)
) where

import Data.Char (isDigit, isAlpha)
import qualified Data.Text as T

import Htcc.Utils (lor, sop, sopText)

-- | Return `True` if it is a valid character.
isValidChar :: Char -> Bool
isValidChar = lor [isAlpha, (=='_'), isDigit]

-- | Class of type that can be treated as a set of characters.
class GenericStr a where
    -- | Returns `True` if the set of characters is 
    -- a valid C language characters.
    isValid :: a -> Bool 

instance GenericStr String where
    isValid [] = False
    isValid (x:xs) = [isAlpha, (=='_')] `lor` x && [isAlpha, (=='_'), isDigit] `sop` xs

instance GenericStr T.Text where
    isValid xs 
        | T.null xs = False 
        | otherwise = [isAlpha, (=='_')] `lor` T.head xs && [isAlpha, (=='_'), isDigit] `sopText` T.tail xs
