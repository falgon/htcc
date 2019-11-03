{-|
Module      : Htcc.CRules.Types.Core
Description : The types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The types of C language
-}
{-# LANGUAGE DeriveGeneric, BangPatterns #-}
module Htcc.CRules.Types.Core (
    -- * TypeKind data type
    StructMember (..),
    TypeKind (..),
    -- * Lookup functions
    lookupMember,
    -- * Utilities of C type
    alignas
) where

import Prelude hiding (toInteger)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Numeric.Natural
import Data.Tuple.Extra (first, second)
import Data.List (foldl', maximumBy, find, intercalate)
import Data.Bits ((.&.), complement, Bits (..))
import qualified Data.Map as M
import qualified Data.Text as T

import Htcc.CRules.Char
import Htcc.CRules.Types.CType
import Htcc.Utils (toNatural, toInteger, dropFst3, spanLen, maybe', dropSnd3)

-- | The type and offset value of a data member.
data StructMember i = StructMember -- ^ `StructMember` constructor
    {
        smType :: TypeKind i, -- ^ The type of a data member
        smOffset :: Natural -- ^ The offset of a data member
    } deriving (Eq, Show, Read, Generic)

instance NFData i => NFData (StructMember i)

-- | The kinds of types in C language.
data TypeKind i = CTInt -- ^ The type @int@ as C language
    | CTChar -- ^ The type @char@ as C language
    | CTSigned (TypeKind i) -- ^ The type @signed@ as C language
    | CTShort (TypeKind i) -- ^ The type @short@ as C language
    | CTLong (TypeKind i) -- ^ The type @long@ as C language
    | CTBool -- ^ The type @_Bool@ as C language
    | CTVoid -- ^ The type @void@ as C language
    | CTPtr (TypeKind i) -- ^ The pointer type of `TypeKind`
    | CTArray Natural (TypeKind i) -- ^ The array type
    | CTEnum (TypeKind i) (M.Map T.Text i) -- ^ The enum, has its underlying type and a map
    | CTStruct (M.Map T.Text (StructMember i)) -- ^ The struct, has its members and their names.
    | CTUndef -- ^ Undefined type
    deriving (Eq, Generic)

{-# INLINE fundamental #-}
fundamental :: [TypeKind i]
fundamental = [CTChar, CTInt, CTShort CTUndef, CTLong CTUndef, CTSigned CTUndef]
    
{-# INLINE isLongShortable #-}
isLongShortable :: TypeKind i -> Bool
isLongShortable CTInt = True
isLongShortable CTUndef = True
isLongShortable (CTLong x) = isLongShortable x
isLongShortable (CTShort x) = isLongShortable x
isLongShortable (CTSigned x) = isLongShortable x
isLongShortable _ = False
            
{-# INLINE isShort #-}
isShort :: TypeKind i -> Bool
isShort (CTShort _) = True
isShort (CTSigned t) = isShort t
isShort _ = False

{-# INLINE isLong #-}
isLong :: TypeKind i -> Bool
isLong (CTLong _) = True
isLong (CTSigned t) = isLong t
isLong _ = False
    
{-# INLINE isQualifier #-}
isQualifier :: TypeKind i -> Bool
isQualifier (CTShort _) = True
isQualifier (CTLong _) = True
isQualifier (CTSigned _) = True
isQualifier _ = False

{-# INLINE qual #-}
qual :: TypeKind i -> TypeKind i -> TypeKind i
qual (CTLong x) t = CTLong $ qual x t
qual (CTShort x) t = CTShort $ qual x t
qual (CTSigned x) t = CTSigned $ qual x t
qual CTUndef t = t
qual _ _ = error "qual: should not reach here"

{-# INLINE removeAllQualified #-}
removeAllQualified :: TypeKind i -> TypeKind i
removeAllQualified (CTLong x) = removeAllQualified x
removeAllQualified (CTShort x) = removeAllQualified x
removeAllQualified (CTSigned x) = removeAllQualified x
removeAllQualified x = x

instance Show i => Show (TypeKind i) where
    show CTInt = "int"
    show CTChar = "char"
    show (CTSigned _) = "signed"
    show (CTShort _) = "short"
    show (CTLong _) = "long"
    show CTBool = "_Bool"
    show CTVoid = "void"
    show (CTPtr x) = show x ++ "*"
    show (CTArray v t) = show t ++ "[" ++ show v ++ "]"
    show (CTEnum _ m) = "enum { " ++ intercalate ", " (map T.unpack $ M.keys m) ++ " }" 
    show (CTStruct m) = "struct { " ++ concatMap (\(v, inf) -> show (smType inf) ++ " " ++ T.unpack v ++ "; ") (M.toList m) ++ "}"
    show CTUndef = "undefined"

instance (Show i, Read i, Ord i) => Read (TypeKind i) where
    readsPrec _ xs = let (ys, ds) = dropFst3 $ spanLen isValidChar xs in
        maybe' (error "no parse pattern by TypeKind") (find ((ys==) . show) fundamental) $ \x ->
            [first (flip id x . ctorPtr . toNatural) $ dropSnd3 $ spanLen (=='*') ds]

instance Ord i => Ord (TypeKind i) where
    compare x = compare (sizeof x) . sizeof

instance NFData i => NFData (TypeKind i)

instance Ord i => CType (TypeKind i) where
    qualify = (.) (fmap (uncurry qual)) . maxQual
        where
            maxQual CTChar (CTSigned CTUndef) = Just (CTSigned CTUndef, CTChar)
            maxQual (CTSigned CTUndef) CTChar = Just (CTSigned CTUndef, CTChar)
            maxQual ty1 ty2
                | isShort ty1 && isLong ty2 || isShort ty2 && isLong ty1 = Nothing
                | isQualifier ty1 && isQualifier ty2 && isLongShortable (max ty1 ty2) = Just (min ty1 ty2, max ty1 ty2)
                | otherwise = f ty1 ty2 <|> f ty2 ty1
                where
                    f t u 
                        | isQualifier t && isLongShortable u = Just (t, u) 
                        | otherwise = Nothing
    
    {-# INLINE isCTArray #-}
    isCTArray (CTArray _ _) = True
    isCTArray _ = False
    
    {-# INLINE isCTStruct #-}
    isCTStruct (CTStruct _) = True
    isCTStruct _ = False
    
    {-# INLINE isCTUndef #-}
    isCTUndef CTUndef = True
    isCTUndef _ = False

    {-# INLINE isFundamental #-}
    isFundamental = flip elem [CTChar, CTInt] . removeAllQualified

    sizeof CTInt = 4 
    sizeof CTChar = 1
    sizeof (CTSigned x) = sizeof x
    sizeof (CTShort CTInt) = 2
    sizeof (CTShort (CTSigned x)) = sizeof $ CTShort x
    sizeof (CTLong CTInt) = 8
    sizeof (CTLong (CTSigned x)) = sizeof $ CTLong x
    sizeof (CTLong x) = sizeof x
    sizeof CTBool = 1
    sizeof CTVoid = 1 -- Non standard
    sizeof (CTPtr _) = 8
    sizeof (CTArray v t) = v * sizeof t
    sizeof (CTEnum t _) = sizeof t
    sizeof t@(CTStruct m) 
        | M.null m = 1
        | otherwise = let sn = maximumBy (flip (.) smOffset . compare . smOffset) $ M.elems m in
            toNatural $ alignas (toInteger $ smOffset sn + sizeof (smType sn)) (toInteger $ alignof t)
    sizeof CTUndef = 0
    sizeof _ = error "sizeof: sould not reach here"
    
    alignof CTInt = 4
    alignof CTChar = 1
    alignof (CTSigned x) = sizeof x
    alignof (CTShort CTInt) = 2
    alignof (CTShort (CTSigned x)) = sizeof $ CTShort x
    alignof (CTLong CTInt) = 8
    alignof (CTLong (CTSigned x)) = sizeof $ CTLong x
    alignof (CTLong x) = alignof x
    alignof CTBool = 1
    alignof CTVoid = 1 -- Non standard
    alignof (CTPtr _) = 8
    alignof (CTArray _ t) = alignof $ removeAllExtents t
    alignof (CTEnum t _) = alignof t
    alignof (CTStruct m)
        | M.null m = 1
        | otherwise = maximum $ map (alignof . smType) $ M.elems m
    alignof CTUndef = 0
    alignof _ = error "alignof: sould not reach here"

    deref (CTPtr x) = Just x
    deref ct@(CTArray _ _) = Just $ f ct
        where
            f (CTArray n c@(CTArray _ _)) = CTArray n (f c)
            f (CTArray _ t) = t
            f t = t
    deref _ = Nothing

    ctorPtr n = foldr (.) id $ replicate (fromIntegral n) CTPtr

    dctorPtr (CTPtr x) = second (CTPtr .) $ dctorPtr x
    dctorPtr x = (x, id)

    makeCTArray ns t = foldl' (flip CTArray) t ns

    concatCTArray l@(CTArray _ _) r@(CTArray n r')
        | removeAllExtents l == removeAllExtents r = Just $ CTArray n $ f l r'
        | otherwise = Nothing
        where
            f l'@(CTArray _ _) (CTArray n'' r'') = CTArray n'' $ f l' r''
            f l' _ = l'
    concatCTArray _ _ = Nothing

    removeAllExtents (CTArray _ t) = removeAllExtents t
    removeAllExtents x = x

    conversion l r
        | l == r = l
        | otherwise = max l r
    
    {-# INLINE implicitInt #-}
    implicitInt (CTLong x) = CTLong $ implicitInt x
    implicitInt (CTShort x) = CTShort $ implicitInt x
    implicitInt (CTSigned x) = CTSigned $ implicitInt x
    implicitInt CTUndef = CTInt
    implicitInt x = x

{-# INLINE alignas #-}
-- | `alignas` align to @n@.
alignas :: (Bits a, Num a, Enum a) => a -> a -> a
alignas !n !aval = pred (n + aval) .&. complement (pred aval)

-- | `lookupMember` search the specified member by its name from `CTStruct`.
lookupMember :: T.Text -> TypeKind i -> Maybe (StructMember i)
lookupMember t (CTStruct m) = M.lookup t m
lookupMember _ _ = Nothing
