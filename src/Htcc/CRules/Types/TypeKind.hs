{-|
Module      : Htcc.CRules.Types.TypeKind
Description : The types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The types of C language
-}
{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module Htcc.CRules.Types.TypeKind (
    -- * TypeKind data type
    StructMember (..),
    TypeKind (..),
    Incomplete (..),
    -- * Type classes that can be converted to `TypeKind`
    TypeKindBase (..),
    IncompleteBase (..),
    -- * Lookup functions
    lookupMember,
    -- * Utilities of C type
    alignas,
    Desg (..),
    accessibleIndices,
) where

import           Control.Applicative     ((<|>))
import           Control.DeepSeq         (NFData (..))
import           Data.Bits               (Bits (..), complement, (.&.))
import           Data.Foldable           (Foldable (..))
import           Data.List               (find, intercalate, maximumBy)
import           Data.List.Split         (chunksOf)
import qualified Data.Map                as M
import qualified Data.Text               as T
import           Data.Tree               (Tree (..))
import           Data.Tuple.Extra        (first, second)
import           GHC.Generics            (Generic)
import           Numeric.Natural
import           Prelude                 hiding (toInteger)

import           Htcc.CRules.Char
import           Htcc.CRules.Types.CType
import           Htcc.Utils              (dropFst3, dropSnd3, lor, maybe',
                                          spanLen, toInteger, toNatural)

-- | Class to a type based on `TypeKind`.
class TypeKindBase a where
    -- | `isCTArray` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTArray`.
    -- Otherwise, returns `False`
    isCTArray :: a i -> Bool
    -- | `isArray` return `True` when the given argument is `Htcc.CRules.Types.Core.CTArray` or `IncompleteArray`
    -- Otherwise, returns `False`
    isArray :: a i -> Bool
    -- | `isCTStruct` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTStruct`.
    -- Otherwise, returns `False`
    isCTStruct :: a i -> Bool
    -- | `isCTUndef` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTUndef`.
    -- Otherwise, returns `False`
    isCTUndef :: a i -> Bool
    -- | `isCTIncomplete` returns `True` when the given argument is `Htcc.CRules.Types.CType.CTIncomplete`.
    isCTIncomplete :: a i -> Bool
    -- | `makeCTArray` retunrs a multidimensional array based on the arguments (list of each dimension).
    -- e.g.:
    --
    -- >>> makeCTArray [1, 2] CTInt
    -- int[1][2]
    -- >>> makeCTArray [1, 2] (CTArray 2 CTInt)
    -- int[2][1][2]
    makeCTArray :: [Natural] -> a i -> a i
    -- | Only if both arguments is `Htcc.CRules.Types.Core.CTArray`,
    -- `concatCTArray` returns a new multidimensional array by conbining the types of
    -- multidimensional arrays as follows.
    --
    -- >>> makeCTArray [1, 2] CTInt `concatCTArray` makeCTArray [3, 4] CTInt
    -- Just int[1][2][3][4]
    -- >>> CTInt `concatCTArray` CTArray 2 CTInt
    -- Nothing
    concatCTArray :: Ord i => a i -> a i -> Maybe (a i)
    -- | Convert to `TypeKind`.
    toTypeKind :: a i -> TypeKind i
    -- | Application to `TypeKind`.
    mapTypeKind :: (TypeKind i -> TypeKind j) -> a i -> a j

-- | The type and offset value of a data member.
data StructMember i = StructMember -- ^ `StructMember` constructor
    {
        smType   :: TypeKind i, -- ^ The type of a data member
        smOffset :: Natural -- ^ The offset of a data member
    } deriving (Eq, Show, Read, Generic)

instance NFData i => NFData (StructMember i)

-- | A class requesting a type that represents an incomplete type.
class IncompleteBase a where
    -- | When the given argument is incomplete array, `isIncompleteArray` returns `True`, otherwise `False`.
    isIncompleteArray :: a i -> Bool
    -- | When the given argument is incmoplete struct, `isIncompleteStruct` returns `True`, otherwise `False`.
    isIncompleteStruct :: a i -> Bool
    -- | Extract the tag name from `IncompleteStruct`. If not `IncompleteStruct`, `Nothing` is retunred.
    fromIncompleteStruct :: a i -> Maybe T.Text
    -- | Extract the type of array from `IncompleteArray`. If not `IncompleteArray`, `Nothing` is retunred.
    fromIncompleteArray :: a i -> Maybe (TypeKind i)
    -- | Returns True if the incomplete type is temporarily valid at the time of declaration. Otherwise returns `False`.
    isValidIncomplete :: Ord i => a i -> Bool

-- | The type representing an incomplete type
data Incomplete i = IncompleteArray (TypeKind i) -- ^ incomplete array, it has a base type.
    | IncompleteStruct T.Text -- ^ incomplete struct, it has a tag name.
    deriving (Eq, Generic)

instance IncompleteBase Incomplete where
    isIncompleteArray (IncompleteArray _) = True
    isIncompleteArray _                   = False
    isIncompleteStruct (IncompleteStruct _) = True
    isIncompleteStruct _                    = False
    fromIncompleteStruct (IncompleteStruct t) = Just t
    fromIncompleteStruct _                    = Nothing
    fromIncompleteArray (IncompleteArray t) = Just t
    fromIncompleteArray _                   = Nothing
    isValidIncomplete (IncompleteArray t) = isFundamental t
    isValidIncomplete _                   = True

instance Show i => Show (Incomplete i) where
    show (IncompleteArray t)  = show t ++ "[]"
    show (IncompleteStruct t) = T.unpack t

instance NFData i => NFData (Incomplete i)

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
    | CTIncomplete (Incomplete i) -- ^ The incomplete type.
    | CTUndef -- ^ Undefined type
    deriving Generic

{-# INLINE fundamental #-}
fundamental :: [TypeKind i]
fundamental = [CTChar, CTInt, CTShort CTUndef, CTLong CTUndef, CTSigned CTUndef]

{-# INLINE isLongShortable #-}
isLongShortable :: TypeKind i -> Bool
isLongShortable CTInt        = True
isLongShortable CTUndef      = True
isLongShortable (CTLong x)   = isLongShortable x
isLongShortable (CTShort x)  = isLongShortable x
isLongShortable (CTSigned x) = isLongShortable x
isLongShortable _            = False

{-# INLINE isShort #-}
isShort :: TypeKind i -> Bool
isShort (CTShort _)  = True
isShort (CTSigned t) = isShort t
isShort _            = False

{-# INLINE isLong #-}
isLong :: TypeKind i -> Bool
isLong (CTLong _)   = True
isLong (CTSigned t) = isLong t
isLong _            = False

{-# INLINE isQualifier #-}
isQualifier :: TypeKind i -> Bool
isQualifier (CTShort _)  = True
isQualifier (CTLong _)   = True
isQualifier (CTSigned _) = True
isQualifier _            = False

{-# INLINE qual #-}
qual :: TypeKind i -> TypeKind i -> TypeKind i
qual (CTLong x) t   = CTLong $ qual x t
qual (CTShort x) t  = CTShort $ qual x t
qual (CTSigned x) t = CTSigned $ qual x t
qual CTUndef t      = t
qual _ _            = error "qual: should not reach here"

{-# INLINE removeAllQualified #-}
removeAllQualified :: TypeKind i -> TypeKind i
removeAllQualified (CTLong x)   = removeAllQualified x
removeAllQualified (CTShort x)  = removeAllQualified x
removeAllQualified (CTSigned x) = removeAllQualified x
removeAllQualified x            = x

{-# INLINE combTable #-}
combTable :: TypeKind i -> Maybe Int
combTable CTChar       = Just 1
combTable CTInt        = Just $ shiftL 1 1
combTable CTBool       = Just $ shiftL 1 2
combTable CTVoid       = Just $ shiftL 1 3
combTable CTUndef      = Just $ shiftL 1 4
combTable (CTPtr x)    = (shiftL 1 5 .|.) <$> combTable x
combTable (CTSigned x) = (shiftL 1 6 .|.) <$> combTable x
combTable (CTLong x)   = (shiftL 1 7 .|.) <$> combTable x
combTable (CTShort x)  = (shiftL 1 8 .|.) <$> combTable x
combTable _            = Nothing

{-# INLINE arSizes #-}
arSizes :: (Num i, Enum i) => TypeKind i -> (i, [[i]])
arSizes = arSizes' 0
    where
        arSizes' !dp (CTArray v t) = second ([0..pred $ fromIntegral v]:) $ arSizes' (succ dp) t
        arSizes' !dp _ = (dp, [])

-- | The type of designator
data Desg i = DesgIdx i -- ^ index type
    | DesgMem (StructMember i) -- ^ struct member type
    deriving Eq

instance (Eq i, Ord i, Integral i) => Ord (Desg i) where
    compare (DesgIdx x) (DesgIdx y) = compare x y
    compare (DesgMem mem) y = compare (DesgIdx $ fromIntegral $ smOffset mem) y
    compare x (DesgMem mem) = compare x (DesgIdx $ fromIntegral $ smOffset mem)

instance (Enum i, Integral i) => Enum (Desg i) where
    toEnum = DesgIdx . fromIntegral
    fromEnum (DesgIdx x)   = fromIntegral x
    fromEnum (DesgMem mem) = fromIntegral $ smOffset mem

-- | If the given argument is `CTArray`, it returns a list of accessible indexes of the array.
-- Othrewise returns empty list.
accessibleIndices :: Integral i => TypeKind i -> [[Desg i]]
accessibleIndices = uncurry (concatMap . chunksOf) . first fromIntegral . second (concatMap (map (iNode' id) . iNode id) . arIndices') . arSizes
    where
        arIndices' []     = []
        arIndices' (x:xs) = map (flip ($) (arIndices' xs) . Node) x

        iNode f x@(Node _ []) = [f x]
        iNode f (Node v xs@(Node _ []:_)) = [Node v $ map f xs]
        iNode f (Node v (x:xs)) = iNode (Node v . (:[]) . f) x ++ concatMap (iNode (Node v . (:[]) . f)) xs

        iNode' f (Node v []) = f [DesgIdx v]
        iNode' f (Node v t)  = concatMap (iNode' ((DesgIdx v:) . f)) t

instance Eq i => Eq (TypeKind i) where
    (==) CTInt CTInt = True
    (==) CTChar CTChar = True
    (==) CTBool CTBool = True
    (==) CTVoid CTVoid = True
    (==) (CTEnum ut1 m1) (CTEnum ut2 m2) = ut1 == ut2 && m1 == m2
    (==) (CTArray v1 t1) (CTArray v2 t2) = v1 == v2 && t1 == t2
    (==) (CTStruct m1) (CTStruct m2) = m1 == m2
    (==) CTUndef CTUndef = True
    (==) (CTPtr t1) (CTPtr t2) = t1 == t2
    (==) (CTIncomplete t1) (CTIncomplete t2) = t1 == t2
    (==) l r
        | isQualifier l || isQualifier r = maybe' False (combTable l) $ \lh ->
            maybe' False (combTable r) $ \rh -> lh == rh
        | otherwise = False

instance Show i => Show (TypeKind i) where
    show CTInt = "int"
    show CTChar = "char"
    show (CTSigned CTUndef) = "signed"
    show (CTSigned t) = "signed " ++ show t
    show (CTShort CTUndef) = "short"
    show (CTShort t) = "short " ++ show t
    show (CTLong CTUndef) = "long"
    show (CTLong t) = "long " ++ show t
    show CTBool = "_Bool"
    show CTVoid = "void"
    show (CTPtr x) = show x ++ "*"
    show (CTArray v t) = show t ++ "[" ++ show v ++ "]"
    show (CTEnum _ m) = "enum { " ++ intercalate ", " (map T.unpack $ M.keys m) ++ " }"
    show (CTStruct m) = "struct { " ++ concatMap (\(v, inf) -> show (smType inf) ++ " " ++ T.unpack v ++ "; ") (M.toList m) ++ "}"
    show (CTIncomplete t) = show t
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

    {-# INLINE isFundamental #-}
    isFundamental = flip elem [CTChar, CTInt, CTBool] . removeAllQualified

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
    sizeof (CTIncomplete _) = 0
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
    alignof (CTIncomplete (IncompleteArray t))
        | isFundamental t = alignof t
        | otherwise = 0
    alignof (CTIncomplete _) = 1
    alignof (CTStruct m)
        | M.null m = 1
        | otherwise = maximum $ map (alignof . smType) $ M.elems m
    alignof CTUndef = 0
    alignof _ = error "alignof: sould not reach here"

    deref (CTPtr x) = Just x
    deref ct@(CTArray _ _) = Just $ f ct
        where
            f (CTArray n c@(CTArray _ _)) = CTArray n (f c)
            f (CTArray _ t)               = t
            f t                           = t
    deref (CTIncomplete (IncompleteArray (CTArray _ _))) = Nothing
    deref (CTIncomplete (IncompleteArray t)) = Just t
    deref _ = Nothing

    ctorPtr n = foldr (.) id $ replicate (fromIntegral n) CTPtr

    dctorPtr (CTPtr x) = second (CTPtr .) $ dctorPtr x
    dctorPtr x         = (x, id)

    dctorArray (CTArray n x) = second (CTArray n .) $ dctorArray x
    dctorArray x             = (x, id)

    removeAllExtents (CTArray _ t)                      = removeAllExtents t
    removeAllExtents (CTIncomplete (IncompleteArray t)) = removeAllExtents t
    removeAllExtents x                                  = x

    conversion l r
        | l == r = l
        | otherwise = max l r

    {-# INLINE implicitInt #-}
    implicitInt (CTLong x)   = CTLong $ implicitInt x
    implicitInt (CTShort x)  = CTShort $ implicitInt x
    implicitInt (CTSigned x) = CTSigned $ implicitInt x
    implicitInt CTUndef      = CTInt
    implicitInt x            = x


instance TypeKindBase TypeKind where
    {-# INLINE isCTArray #-}
    isCTArray (CTArray _ _) = True
    isCTArray _             = False

    {-# INLINE isArray #-}
    isArray = lor [isCTArray, isIncompleteArray]

    {-# INLINE isCTStruct #-}
    isCTStruct (CTStruct _) = True
    isCTStruct _            = False

    {-# INLINE isCTUndef #-}
    isCTUndef CTUndef = True
    isCTUndef _       = False

    {-# INLINE isCTIncomplete #-}
    isCTIncomplete (CTIncomplete _) = True
    isCTIncomplete _                = False

    {-# INLINE makeCTArray #-}
    makeCTArray ns t = foldl' (flip CTArray) t ns

    concatCTArray l@(CTArray _ _) r@(CTArray n r')
        | removeAllExtents l == removeAllExtents r = Just $ CTArray n $ f l r'
        | otherwise = Nothing
        where
            f l'@(CTArray _ _) (CTArray n'' r'') = CTArray n'' $ f l' r''
            f l' _                               = l'
    concatCTArray l@(CTIncomplete (IncompleteArray _)) r@(CTArray n r')
        | removeAllExtents l == removeAllExtents r = Just $ CTArray n $ f l r'
        | otherwise = Nothing
        where
            f l'@(CTIncomplete (IncompleteArray _)) (CTArray n' r'') = CTArray n' $ f l' r''
            f l' _ = l'
    concatCTArray _ _ = Nothing

    {-# INLINE toTypeKind #-}
    toTypeKind = id

    {-# INLINE mapTypeKind #-}
    mapTypeKind = id

instance IncompleteBase TypeKind where
    {-# INLINE isIncompleteArray #-}
    isIncompleteArray (CTIncomplete x) = isIncompleteArray x
    isIncompleteArray (CTArray _ x)    = isIncompleteArray x
    isIncompleteArray _                = False
    {-# INLINE isIncompleteStruct #-}
    isIncompleteStruct (CTIncomplete x) = isIncompleteStruct x
    isIncompleteStruct _                = False
    {-# INLINE fromIncompleteStruct #-}
    fromIncompleteStruct (CTIncomplete x) = fromIncompleteStruct x
    fromIncompleteStruct _                = Nothing
    {-# INLINE fromIncompleteArray #-}
    fromIncompleteArray (CTIncomplete x) = fromIncompleteArray x
    fromIncompleteArray (CTArray _ x)    = fromIncompleteArray x
    fromIncompleteArray _                = Nothing
    {-# INLINE isValidIncomplete #-}
    isValidIncomplete (CTIncomplete x) = isValidIncomplete x
    isValidIncomplete (CTArray _ x)    = isValidIncomplete x
    isValidIncomplete _                = True

{-# INLINE alignas #-}
-- | `alignas` align to @n@.
alignas :: (Bits a, Num a, Enum a) => a -> a -> a
alignas !n !aval = pred (n + aval) .&. complement (pred aval)

-- | `lookupMember` search the specified member by its name from `CTStruct`.
lookupMember :: T.Text -> TypeKind i -> Maybe (StructMember i)
lookupMember t (CTStruct m) = M.lookup t m
lookupMember _ _            = Nothing
