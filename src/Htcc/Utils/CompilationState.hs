{-|
Module      : Htcc.Utils.CompilationState
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

General-purpose utilities
-}
module Htcc.Utils.CompilationState (
    CompilationState,
    itemP,
    itemsP,
    itemC,
    itemsC,
    curCD,
    itemCWhen,
    itemsCWhen,
    isSatisfied
) where

import           Control.Monad                                   (replicateM)
import           Control.Monad.Loops                             (unfoldM)
import           Control.Monad.State                             (StateT, get,
                                                                  gets, put)
import           Data.Bool                                       (bool)
import           Data.Maybe                                      (catMaybes)
import           Data.MonoTraversable                            (Element, MonoFoldable (..),
                                                                  headMay)
import qualified Data.Sequences                                  as S
import           Data.Tuple.Extra                                (first, second)

import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)

-- | The state type handled during compilation.
-- It has informations required during the compilation process and input data consumed.
type CompilationState cd inp i r = StateT (cd, inp) (Either (ASTError i)) r

{-# INLINE itemP #-}
-- | `itemP` peeks at one item from input data
itemP :: MonoFoldable mono => CompilationState cd mono i (Maybe (Element mono))
itemP = gets (headMay . snd)

{-# INLINE itemsP #-}
-- | `itemsP` peeks at items from input data
itemsP :: (MonoFoldable mono, S.IsSequence mono) => S.Index mono -> CompilationState cd mono i (Maybe mono)
itemsP n = do
    x <- gets (S.take n . snd)
    return $ if olength x == fromIntegral n then Just x else Nothing

{-# INLINE itemC #-}
-- | `itemC` consumes at one item from input data.
-- Defines information updates by providing a function that
-- accepts the current information and one item to be consumed and returns the information
itemC :: S.IsSequence mono => (cd -> Element mono -> cd) -> CompilationState cd mono i (Maybe (Element mono))
itemC f = itemP >>= maybe (return Nothing) (\itp -> Just itp <$ (get >>= put . first (`f` itp) . second S.tailEx))

{-# INLINE itemsC #-}
-- | `itemsC` consumes at items from input data.
-- Defines information updates by providing a function that
-- accepts the current information and one item to be consumed and returns the information
itemsC :: S.IsSequence mono => (cd -> Element mono -> cd) -> Int -> CompilationState cd mono i (Maybe mono)
itemsC f n = do
    x <- catMaybes <$> replicateM n (itemC f)
    return $ if length x == n then Just $ S.pack x else Nothing

{-# INLINE curCD #-}
-- | `curCD` gets current information
curCD :: CompilationState cd mono i cd
curCD = gets fst

{-# INLINE itemCWhen #-}
-- | `itemCWhen` consumes an item when the unary function satisfies the given condition.
-- Defines information updates by providing a function that
-- accepts the current information and one item to be consumed and returns the information
itemCWhen :: (MonoFoldable mono, S.IsSequence mono) => (cd -> Element mono -> cd) -> (Element mono -> Bool) -> CompilationState cd mono i (Maybe (Element mono))
itemCWhen cf f = itemP >>= maybe (return Nothing) (bool (return Nothing) (itemC cf) . f)

{-# INLINE itemsCWhen #-}
-- | `itemsCWhen` consumes items when the unary function satisfies the given condition.
-- Defines information updates by providing a function that
-- accepts the current information and one item to be consumed and returns the information
itemsCWhen :: (MonoFoldable mono, S.IsSequence mono) => (cd -> Element mono -> cd) -> (Element mono -> Bool) -> CompilationState cd mono i mono
itemsCWhen cf f = fmap S.pack $ unfoldM $ itemCWhen cf f

{-# INLINE isSatisfied #-}
-- | `isSatisfied` returns `True` if the input data satisfies the condition of given unary function, otherwise returns `False`.
isSatisfied :: (mono -> Bool) -> CompilationState cd mono i Bool
isSatisfied f = gets (f . snd)

