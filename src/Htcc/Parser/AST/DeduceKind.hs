{-|
Module      : Htcc.Parser.AST.DeduceKind
Description : Data types and type synonyms used during AST construction
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Data types and type synonyms used during AST construction
-}
module Htcc.Parser.AST.DeduceKind (
    addKind,
    subKind
) where

import           Data.Maybe           (isJust)

import qualified Htcc.CRules.Types    as CT
import           Htcc.Parser.AST.Core (ATKind (..), ATree (..))

{-# INLINE addKind #-}
-- | Constructs a numeric addition or pointer addition node according to the C language implicit conversion rules
addKind :: (Eq i, Ord i, Show i) => ATree i -> ATree i -> Maybe (ATree i)
addKind lhs rhs
    | all (CT.isFundamental . atype) [lhs, rhs] = Just $ ATNode ATAdd (CT.conversion (atype lhs) (atype rhs)) lhs rhs
    | isJust (CT.deref $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATAddPtr (atype lhs) lhs rhs
    | CT.isFundamental (atype lhs) && isJust (CT.deref $ atype rhs) = Just $ ATNode ATAddPtr (atype rhs) rhs lhs
    | otherwise = Nothing

{-# INLINE subKind #-}
-- | Constructs a number subtraction or pointer subtraction node according to the C language implicit conversion rules
subKind :: (Eq i, Ord i) => ATree i -> ATree i -> Maybe (ATree i)
subKind lhs rhs
    | all (CT.isFundamental . atype) [lhs, rhs] = Just $ ATNode ATSub (CT.conversion (atype lhs) (atype rhs)) lhs rhs
    | isJust (CT.deref $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATSubPtr (atype lhs) lhs rhs
    | all (isJust . CT.deref . atype) [lhs, rhs] = Just $ ATNode ATPtrDis (atype lhs) lhs rhs
    | otherwise = Nothing
