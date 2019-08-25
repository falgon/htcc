{-|
Module      : Htcc.ErrorTrack
Description : The utility for tracking the location of the error
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The utility for tracking the location of the error
-}
module Htcc.ErrorTrack (
    -- * Utilities of error tracking
    ErrTracker (..),
    init,
    advance
) where

import Prelude hiding (init)
import qualified Data.Text as T

-- | The data type for tracking error.
data ErrTracker i = ErrTracker {
    pos :: i, -- ^ The character position of error.
    str :: T.Text -- ^ The string of error.
} deriving (Eq, Ord, Show)

instance Integral i => Enum (ErrTracker i) where
    toEnum = flip ErrTracker T.empty . fromIntegral
    fromEnum = fromIntegral . pos
    succ (ErrTracker p s) = ErrTracker (succ p) s
    pred (ErrTracker p s) = ErrTracker (pred p) s

-- | The initial state of `ErrTracker`.
init :: Integral i => ErrTracker i
init = ErrTracker 0 T.empty

-- | Advances the position by the specified integer value.
advance :: (Integral a, Integral b) => a -> ErrTracker b -> ErrTracker b
advance x e = ErrTracker (pos e + fromIntegral x) (str e)
