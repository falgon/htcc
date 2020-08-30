{-|
Module      : Htcc.CRules.Types.StorageClass
Description : The storage-class of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The storage-class of C language
-}
{-# LANGUAGE DeriveGeneric #-}
module Htcc.CRules.Types.StorageClass (
    -- * StorageClass data type and class
    StorageClass (..),
    StorageClassBase (..)
) where

import           Control.DeepSeq            (NFData (..))
import           Data.Tuple.Extra           (first, second)
import           GHC.Generics               (Generic)

import           Htcc.CRules.Types.CType
import           Htcc.CRules.Types.TypeKind

-- | The data type representing `StorageClass`
data StorageClass i = SCAuto (TypeKind i) -- ^ The @auto@ keyword
    | SCStatic (TypeKind i) -- ^ The @static@ keyword
    | SCRegister (TypeKind i) -- ^ The @register@ keyword
    | SCUndef (TypeKind i) -- ^ `SCUndef` is used when storage-class specifier is not defined
    deriving (Eq, Generic)

-- | Class to a type based on `StorageClass`.
class StorageClassBase a where
    -- | When the given argument is `SCStatic`, `isSCStatic` returns `True`, otherwise `False`.
    isSCStatic :: a i -> Bool

{-# INLINE fromsc #-}
-- | Take type from `StorageClass`
fromsc :: StorageClass i -> TypeKind i
fromsc (SCAuto t)     = t
fromsc (SCStatic t)   = t
fromsc (SCRegister t) = t
fromsc (SCUndef t)    = t

{-# INLINE picksc #-}
-- | Take storage-class from `StorageClass`
picksc :: StorageClass i -> TypeKind j -> StorageClass j
picksc (SCAuto _)     = SCAuto
picksc (SCStatic _)   = SCStatic
picksc (SCRegister _) = SCRegister
picksc (SCUndef _)    = SCUndef

{-# INLINE isSameSC #-}
isSameSC :: StorageClass i -> StorageClass i -> Bool
isSameSC (SCAuto _) (SCAuto _)         = True
isSameSC (SCStatic _) (SCStatic _)     = True
isSameSC (SCRegister _) (SCRegister _) = True
isSameSC (SCUndef _) (SCUndef _)       = True
isSameSC _ _                           = False

instance Ord i => Ord (StorageClass i) where
    compare x y = compare (toTypeKind x) (toTypeKind y)

instance Show i => Show (StorageClass i) where
    show (SCAuto CTUndef)     = "auto"
    show (SCAuto t)           = "auto " ++ show t
    show (SCStatic CTUndef)   = "static"
    show (SCStatic t)         = "static " ++ show t
    show (SCRegister CTUndef) = "register"
    show (SCRegister t)       = "register " ++ show t
    show (SCUndef CTUndef)    = "undefined"
    show (SCUndef t)          = show t

instance Ord i => CType (StorageClass i) where
    isFundamental = isFundamental . toTypeKind
    qualify x y
        | isSameSC x y = picksc x <$> qualify (toTypeKind x) (toTypeKind y)
        | otherwise = Nothing
    sizeof = sizeof . toTypeKind
    alignof = alignof . toTypeKind
    deref x = picksc x <$> deref (toTypeKind x)
    ctorPtr n = mapTypeKind (ctorPtr n)
    dctorPtr x = first (picksc x) $ second (\f y -> picksc y $ f $ toTypeKind y) $ dctorPtr $ toTypeKind x
    dctorArray x = first (picksc x) $ second (\f y -> picksc y $ f $ toTypeKind y) $ dctorArray $ toTypeKind x
    removeAllExtents = mapTypeKind removeAllExtents
    conversion x y = SCAuto $ conversion (toTypeKind x) (toTypeKind y)
    implicitInt = mapTypeKind implicitInt

instance TypeKindBase StorageClass where
    {-# INLINE isCTArray #-}
    isCTArray = isCTArray . toTypeKind

    {-# INLINE isArray #-}
    isArray = isArray . toTypeKind

    {-# INLINE isCTStruct #-}
    isCTStruct = isCTStruct . toTypeKind

    {-# INLINE isCTUndef #-}
    isCTUndef = isCTUndef . toTypeKind

    {-# INLINE isCTIncomplete #-}
    isCTIncomplete = isCTIncomplete . toTypeKind

    {-# INLINE makeCTArray #-}
    makeCTArray ns = mapTypeKind (makeCTArray ns)

    concatCTArray x y
        | isSameSC x y = picksc x <$> concatCTArray (toTypeKind x) (toTypeKind y)
        | otherwise = Nothing

    {-# INLINE toTypeKind #-}
    toTypeKind = fromsc

    {-# INLINE mapTypeKind #-}
    mapTypeKind f sc = picksc sc $ f $ toTypeKind sc

instance IncompleteBase StorageClass where
    {-# INLINE isIncompleteArray #-}
    isIncompleteArray = isIncompleteArray . toTypeKind
    {-# INLINE isIncompleteStruct #-}
    isIncompleteStruct = isIncompleteStruct . toTypeKind
    {-# INLINE fromIncompleteStruct #-}
    fromIncompleteStruct = fromIncompleteStruct . toTypeKind
    {-# INLINE fromIncompleteArray #-}
    fromIncompleteArray = fromIncompleteArray . toTypeKind
    {-# INLINE isValidIncomplete #-}
    isValidIncomplete = isValidIncomplete . toTypeKind

instance StorageClassBase StorageClass where
    {-# INLINE isSCStatic #-}
    isSCStatic (SCStatic _) = True
    isSCStatic _            = False

instance NFData i => NFData (StorageClass i)
