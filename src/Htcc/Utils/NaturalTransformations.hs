{-|
Module      : Htcc.Utils.NaturalTransformations
Description : Utilities of natural transformations
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Utilities of natural transformations
-}
{-# LANGUAGE Rank2Types, TypeOperators #-}
module Htcc.Utils.NaturalTransformations (
    maybeToRight
) where

import           Control.Natural (type (~>))

-- | Natural transformation from @Maybe@ functor to @Either e@ functor
maybeToRight :: e -> Maybe ~> Either e
maybeToRight s Nothing  = Left s
maybeToRight _ (Just x) = Right x
