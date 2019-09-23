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
    -- * TypeKind data type
    TypeKind (..),
    -- * Utilities of TypeKinds
    sizeof,
    derefMaybe,
    makePtr,
    makeArray,
    concatCTArray,
    isCTArray,
    -- * Type traits
    removeAllExtents
) where

import Numeric.Natural
import Data.List (foldl')

-- | The kinds of types in C language.
data TypeKind = CTInt -- ^ The type @int@ as C language
    | CTPtr TypeKind -- ^ The pointer type of `TypeKind`
    | CTArray Natural TypeKind -- ^ The array type
    | CTUndef -- ^ Undefined type
    deriving Eq

instance Show TypeKind where
    show CTInt = "int"
    show (CTPtr x) = show x ++ "*"
    show (CTArray v t) = show t ++ "[" ++ show v ++ "]"
    show CTUndef = "undefined"

instance Ord TypeKind where
    compare x = compare (sizeof x) . sizeof

-- | `isCTArray` returns `True` when the given argument is `CTArray`. Otherwise, returns `False`.
{-# INLINE isCTArray #-}
isCTArray :: TypeKind -> Bool
isCTArray (CTArray _ _) = True
isCTArray _ = False

-- | `sizeof` returns the byte size of the type defined by C language.
sizeof :: TypeKind -> Natural
sizeof CTInt = 8 -- TODO: 8 is workaround. it should be 4 byte.
sizeof (CTPtr _) = 8
sizeof (CTArray v t) = v * sizeof t
sizeof CTUndef = 0

-- | `derefMaybe` returns @Just x@ for the underlying type @x@ only if `TypeKind` is `CTPtr` or `CTArray`. Otherwise returns `Nothing`. 
derefMaybe :: TypeKind -> Maybe TypeKind
derefMaybe (CTPtr x) = Just x
derefMaybe (CTArray _ x) = Just x
derefMaybe _ = Nothing

-- | `makePtr` returns a pointer of @n@-dimensional type @t@. e.g:
-- >>> makePtr 2 CTInt
-- int**
-- >>> makePtr 2 (CTPtr CTInt)
-- int***
-- >>> makePtr 2 (makePtr 3 CTInt)
-- int*****
makePtr :: Natural -> TypeKind -> TypeKind
makePtr n t = foldr id t $ replicate (fromIntegral n) CTPtr

-- | `makeArray` returns a multidimensional arary based on the arguments (list of each dimension). e.g:
-- >>> makeArray [1,2] CTInt
-- int[1][2]
-- >>> makeArray [1,2] (CTArray 2 CTInt)
-- int[2][1][2]
makeArray :: [Natural] -> TypeKind -> TypeKind
makeArray ns t = foldl' (flip CTArray) t ns

-- | Only if both arguments is CTArray, `concatCTArray` returns a new multidimensional arary by combining the types of multidimensional arrays as follows.
-- Otherwise, `Nothing` is returned:
-- >>> makeArray [1,2] CTInt `concatCTArray` makeArray [3,4] CTInt -- Combine 2D array of 1D array and 4D array of 3D array to make 4D array of 3D array of 2D array of 1D array 
-- Just int[1][2][3][4]
-- >>> CTInt `concatCTArray` CTArray 2 CTInt
-- Nothing
concatCTArray :: TypeKind -> TypeKind -> Maybe TypeKind
concatCTArray l@(CTArray _ _) r@(CTArray n r')
    | removeAllExtents l == removeAllExtents r = Just $ CTArray n $ f l r'
    | otherwise = Nothing
    where
        f l'@(CTArray _ _) (CTArray n'' r'') = CTArray n'' $ f l' r''
        f l' _ = l'
concatCTArray _ _ = Nothing

-- | `removeAllExtents` is the same as @std::remove_all_extent@ defined in C++11 @<type_traits>@ 
-- (See also: <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3337.pdf N3337>/§ 20.9.7.4) header.
-- If type @T@ is a multidimensional array of type @X@, type @X@ is returned.
-- Otherwise, it returns type @T@.
removeAllExtents :: TypeKind -> TypeKind
removeAllExtents (CTArray _ t) = removeAllExtents t
removeAllExtents x = x
