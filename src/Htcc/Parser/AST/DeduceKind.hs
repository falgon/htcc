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
    | isJust (CT.deref $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATAddPtr (pointerOperandType lhs) lhs rhs
    | CT.isFundamental (atype lhs) && isJust (CT.deref $ atype rhs) = Just $ ATNode ATAddPtr (pointerOperandType rhs) rhs lhs
    | otherwise = Nothing

{-# INLINE subKind #-}
-- | Constructs a number subtraction or pointer subtraction node according to the C language implicit conversion rules
subKind :: (Eq i, Ord i) => ATree i -> ATree i -> Maybe (ATree i)
subKind lhs rhs
    | all (CT.isFundamental . atype) [lhs, rhs] = Just $ ATNode ATSub (CT.conversion (atype lhs) (atype rhs)) lhs rhs
    | isJust (CT.deref $ atype lhs) && CT.isFundamental (atype rhs) = Just $ ATNode ATSubPtr (pointerOperandType lhs) lhs rhs
    | all (isJust . CT.deref . atype) [lhs, rhs] = Just $ ATNode ATPtrDis (CT.SCAuto $ CT.CTLong CT.CTInt) lhs rhs
    | otherwise = Nothing

pointerOperandType :: Ord i => ATree i -> CT.StorageClass i
pointerOperandType expr = case CT.toTypeKind ty of
    CT.CTArray _ _ ->
        maybe ty (CT.mapTypeKind CT.CTPtr) $ CT.deref ty
    CT.CTIncomplete (CT.IncompleteArray elemTy) ->
        CT.mapTypeKind (const $ CT.CTPtr elemTy) ty
    _ ->
        ty
    where
        ty = atype expr
