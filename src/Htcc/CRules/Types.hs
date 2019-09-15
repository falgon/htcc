{-|
Module      : Htcc.CRules.Types
Description : The types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The types of C language
-}
module Htcc.CRules.Types (
    TypeKind (..),
    sizeof,
    derefMaybe
) where

import Numeric.Natural

-- | The kinds of types in C language.
data TypeKind = CTInt -- ^ @int@
    | CTPtr TypeKind -- ^ The pointer type of `TypeKind`
    | CTUndef -- ^ Undefined type
    deriving Eq

instance Show TypeKind where
    show CTInt = "int"
    show (CTPtr x) = show x ++ "*"
    show CTUndef = "undefined"

instance Ord TypeKind where
    compare x = compare (sizeof x) . sizeof

-- | `sizeof` returns the byte size of the type defined by C language.
sizeof :: TypeKind -> Natural
sizeof CTInt = 4
sizeof (CTPtr _) = 8
sizeof CTUndef = 0

-- | `derefMaybe` returns @Just x@ for the underlying type @x@ only if `TypeKind` is `CTPtr`. Otherwise returns `Nothing`. 
derefMaybe :: TypeKind -> Maybe TypeKind
derefMaybe (CTPtr x) = Just x
derefMaybe _ = Nothing
