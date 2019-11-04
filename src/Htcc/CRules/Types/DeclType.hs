{-|
Module      : Htcc.CRules.Types.DeclType
Description : The rules of types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The rules of storage-class of C language
-}
module Htcc.CRules.Types.DeclType (
    DeclType (..)
) where

import Data.Tuple.Extra (first, second)

import Htcc.CRules.Types.CType
import Htcc.CRules.Types.TypeKind (TypeKind (..))
import Htcc.CRules.Types.StorageClass

-- | The data type representing declaration-specifiers.
data DeclType i = DeclType -- ^ The constructor of `DeclType`
    { 
        csclass :: StorageClass,
        ctype :: TypeKind i
    }

instance Show i => Show (DeclType i) where
    show (DeclType sc t) = show sc ++ " " ++ show t

instance Ord i => CType (DeclType i) where
    isCTArray = isCTArray . ctype
    isCTStruct = isCTStruct . ctype
    isCTUndef = isCTUndef . ctype
    isFundamental = isFundamental . ctype
    qualify l r 
        | csclass l /= csclass r = Nothing
        | otherwise = DeclType (csclass l) <$> qualify (ctype l) (ctype r)
    sizeof = sizeof . ctype
    alignof = alignof . ctype
    deref x = DeclType (csclass x) <$> deref (ctype x)
    ctorPtr n x = DeclType (csclass x) $ ctorPtr n (ctype x)
    dctorPtr x = first (DeclType $ csclass x) $ second (\f -> (\y -> DeclType (csclass y) $ f $ ctype y)) $ dctorPtr $ ctype x
    makeCTArray xs x = DeclType (csclass x) $ makeCTArray xs (ctype x)
    concatCTArray l r
        | csclass l /= csclass r = Nothing
        | otherwise = DeclType (csclass l) <$> concatCTArray (ctype l) (ctype r)
    removeAllExtents x = DeclType (csclass x) $ removeAllExtents $ ctype x
    conversion l r = DeclType SCAuto $ conversion (ctype l) (ctype r)
    implicitInt x = DeclType (csclass x) $ implicitInt $ ctype x
