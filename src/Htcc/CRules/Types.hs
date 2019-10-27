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
{-# LANGUAGE DeriveGeneric, BangPatterns #-}
module Htcc.CRules.Types (
    -- * TypeKind data type
    StructMember (..),
    TypeKind (..),
    -- * Utilities of TypeKinds
    sizeof,
    derefMaybe,
    ctorPtr,
    dctorPtr,
    makeArray,
    concatCTArray,
    lookupMember,
    isCTArray,
    isCTStruct,
    isCTUndef,
    -- * Utilities of align
    alignof,
    alignas,
    -- * Type traits
    removeAllExtents,
    isPtr,
    isArray,
    isFundamental,
    isQualifier,
    isQualifiable,
    qualify
) where

import Prelude hiding (toInteger)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData (..))
import Numeric.Natural
import Data.Tuple.Extra (first, second)
import Data.List (foldl', maximumBy, find)
import Data.Bits ((.&.), complement, Bits (..))
import qualified Data.Map as M
import qualified Data.Text as T

import Htcc.CRules.Char
import Htcc.Utils (toNatural, toInteger, dropFst3, spanLen, maybe', dropSnd3)

-- | The type and offset value of a data member.
data StructMember = StructMember -- ^ `StructMember` constructor
    {
        smType :: TypeKind, -- ^ The type of a data member
        smOffset :: Natural -- ^ The offset of a data member
    } deriving (Eq, Show, Read, Generic)

instance NFData StructMember

-- | The kinds of types in C language.
data TypeKind = CTInt -- ^ The type @int@ as C language
    | CTChar -- ^ The type @char@ as C language
    | CTShort -- ^ The type @short@ as C language
    | CTLong -- ^ The type @long@ as C language
    | CTPtr TypeKind -- ^ The pointer type of `TypeKind`
    | CTArray Natural TypeKind -- ^ The array type
    | CTStruct (M.Map T.Text StructMember) -- ^ The struct, has its members and their names.
    | CTUndef -- ^ Undefined type
    deriving (Eq, Generic)

instance Show TypeKind where
    show CTInt = "int"
    show CTChar = "char"
    show CTShort = "short"
    show CTLong = "long"
    show (CTPtr x) = show x ++ "*"
    show (CTArray v t) = show t ++ "[" ++ show v ++ "]"
    show (CTStruct m) = "struct { " ++ concatMap (\(v, inf) -> show (smType inf) ++ " " ++ T.unpack v ++ "; ") (M.toList m) ++ "}"
    show CTUndef = "undefined"

instance Read TypeKind where
    readsPrec _ xs = let (ys, ds) = dropFst3 $ spanLen isValidChar xs in
        maybe' (error "no parse pattern by TypeKind") (find ((ys==) . show) fundamental) $ \x ->
            [first (flip id x . ctorPtr . toNatural) $ dropSnd3 $ spanLen (=='*') ds]

instance Ord TypeKind where
    compare x = compare (sizeof x) . sizeof

instance NFData TypeKind

{-# INLINE fundamental #-}
fundamental :: [TypeKind]
fundamental = [CTChar, CTShort, CTInt, CTLong]

-- | `lookupMember` search the specified member by its name from `CTStruct`.
lookupMember :: T.Text -> TypeKind -> Maybe StructMember
lookupMember t (CTStruct m) = M.lookup t m
lookupMember _ _ = Nothing

-- | `isCTArray` returns `True` when the given argument is `CTArray`. Otherwise, returns `False`.
{-# INLINE isCTArray #-}
isCTArray :: TypeKind -> Bool
isCTArray (CTArray _ _) = True
isCTArray _ = False

-- | `isCTStruct` returns `True` when the given argument is `CTStruct`. Otherwise, returns `False`.
{-# INLINE isCTStruct #-}
isCTStruct :: TypeKind -> Bool
isCTStruct (CTStruct _) = True
isCTStruct _ = False

{-# INLINE isCTUndef #-}
-- | `isCTUndef` returns `True` when the given argument is `CTUndef`. Otherwise, returns `False`.
isCTUndef :: TypeKind -> Bool
isCTUndef CTUndef = True
isCTUndef _ = False

-- | `sizeof` returns the byte size of the type defined by C language.
sizeof :: TypeKind -> Natural
sizeof CTInt = 4 
sizeof CTChar = 1
sizeof CTShort = 2
sizeof CTLong = 8
sizeof (CTPtr _) = 8
sizeof (CTArray v t) = v * sizeof t
sizeof t@(CTStruct m) 
    | M.null m = 1
    | otherwise = let sn = maximumBy (flip (.) smOffset . compare . smOffset) $ M.elems m in
        toNatural $ alignas (toInteger $ smOffset sn + sizeof (smType sn)) (toInteger $ alignof t)
sizeof CTUndef = 0

-- | `alignof` returns the alignment of the type defined by C language.
alignof :: TypeKind -> Natural
alignof CTInt = 4
alignof CTChar = 1
alignof CTShort = 2
alignof CTLong = 8
alignof (CTPtr _) = 8
alignof (CTArray _ t) = alignof $ removeAllExtents t
alignof (CTStruct m)
    | M.null m = 1
    | otherwise = maximum $ map (alignof . smType) $ M.elems m
alignof CTUndef = 0

{-# INLINE alignas #-}
-- | `alignas` align to @n@.
alignas :: (Bits a, Num a, Enum a) => a -> a -> a
alignas !n !aval = pred (n + aval) .&. complement (pred aval)

-- | `derefMaybe` returns @Just x@ for the underlying type @x@ only if `TypeKind` is `CTPtr` or `CTArray`. Otherwise returns `Nothing`. 
derefMaybe :: TypeKind -> Maybe TypeKind
derefMaybe (CTPtr x) = Just x
derefMaybe ct@(CTArray _ _) = Just $ f ct
    where
        f (CTArray n c@(CTArray _ _)) = CTArray n (f c)
        f (CTArray _ t) = t
        f t = t
derefMaybe _ = Nothing

-- | `ctorPtr` returns a convolution function with \(n\) specified pointers nested
ctorPtr :: Natural -> (TypeKind -> TypeKind)
ctorPtr n = foldr (.) id $ replicate (fromIntegral n) CTPtr

-- | `dctorPtr` deconstructs the nested structure of `CTPtr` and
-- returns the convolution function of the original type and `CTPtr`.
dctorPtr :: TypeKind -> (TypeKind, TypeKind -> TypeKind)
dctorPtr (CTPtr x) = second (CTPtr .) $ dctorPtr x
dctorPtr x = (x, id)

-- | `makeArray` returns a multidimensional arary based on the arguments (list of each dimension). e.g:
--
-- >>> makeArray [1,2] CTInt
-- int[1][2]
-- >>> makeArray [1,2] (CTArray 2 CTInt)
-- int[2][1][2]
makeArray :: [Natural] -> TypeKind -> TypeKind
makeArray ns t = foldl' (flip CTArray) t ns

-- | Only if both arguments is CTArray, `concatCTArray` returns a new multidimensional arary by combining the types of multidimensional arrays as follows.
-- Otherwise, `Nothing` is returned:
--
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

-- | `removeAllExtents` is the same as @std::remove_all_extents@ defined in C++11 @\<type_traits\>@ 
-- (See also: <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3337.pdf N3337>/§ 20.9.7.4) header.
-- If type @T@ is a multidimensional array of type @X@, type @X@ is returned.
-- Otherwise, it returns type @T@.
removeAllExtents :: TypeKind -> TypeKind
removeAllExtents (CTArray _ t) = removeAllExtents t
removeAllExtents x = x

-- | `isPtr` returns `True` only if the type is `CTPtr`, otherwise returns `False`.
{-# INLINE isPtr #-}
isPtr :: TypeKind -> Bool
isPtr (CTPtr _) = True
isPtr _ = False

-- | `isArray` returns `True only if the type is `CTArray`, otherwise returns `False`.
{-# INLINE isArray #-}
isArray :: TypeKind -> Bool
isArray (CTArray _ _) = True
isArray _ = False

-- | `isFundamental` returns `True` only if the type is fundamental type (See also: § 3.9.1), otherwise returns `False`.
{-# INLINE isFundamental #-}
isFundamental :: TypeKind -> Bool
isFundamental = flip elem fundamental

-- | `isQualifier` return `True` only if the type can be qualifier, otherwise returns `False`
{-# INLINE isQualifier #-}
isQualifier :: TypeKind -> Bool
isQualifier CTShort = True
isQualifier CTLong = True
isQualifier _ = False

-- | `isQualifier` return `True` only if the type can be qualified, otherwise returns `False`
{-# INLINE isQualifiable #-}
isQualifiable :: TypeKind -> Bool
isQualifiable CTInt = True
isQualifiable _ = False

-- | If the first argument is a type qualifier, 
-- `qualify` returns a type that qualifies the type of the second argument with that qualifier. 
-- Otherwise `Nothing` is returned.
qualify :: TypeKind -> TypeKind -> Maybe TypeKind
qualify ty1 ty2 
    | isQualifier ty1 && isQualifiable ty2 = Just ty1
    | isQualifier ty2 && isQualifiable ty1 = Just ty2
    | otherwise = Nothing
