{-|
Module      : Htcc.CRules.Types.CType
Description : The rules of types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The rules of types of C language
-}
module Htcc.CRules.Types.CType (
    CType (..)
) where

import Numeric.Natural

-- | A data type representing the type of C language
class CType a where
    -- | `isCTArray` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTArray`.
    -- Otherwise, returns `False`
    isCTArray :: a -> Bool
    -- | `isCTArray` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTStruct`.
    -- Otherwise, returns `False`
    isCTStruct :: a -> Bool
    -- | `isCTArray` returns `True` when the given argument is `Htcc.CRules.Types.Core.CTUndef`.
    -- Otherwise, returns `False`
    isCTUndef :: a -> Bool
    -- | `isFundamental` returns `True` only if the type is fundamental type (See also: § 3.9.1), otherwise retunrs `False`.
    isFundamental :: a -> Bool
    -- | If the first argument is a type qualifier,
    -- `qualify` returns a type that qualifies the type of the second argument with that qualifier.
    -- Otherwise `Nothing` is returned.
    qualify :: a -> a -> Maybe a
    -- | `sizeof` returns the byte size of the type defined by C language.
    sizeof :: a -> Natural
    -- | `alignof` returns the alignment of the type defiend by C language.
    alignof :: a -> Natural
    -- | `deref` returns @Just x@ for the underlying type @x@ only if @a@ is `Htcc.CRules.Types.Core.CTPtr` or `Htcc.CRules.Types.Core.CTArray`.
    -- Otherwise returns `Nothing`.
    deref :: a -> Maybe a
    -- | `ctorPtr` returns a convolution function with \(n\) specified pointers nested
    ctorPtr :: Natural -> a -> a
    -- | `dctorPtr` deconstructs the nested structure of `Htcc.CRules.Types.Core.CTPtr` and returns the convolution function
    -- of the original type and `Htcc.CRules.Types.Core.CTPtr`
    dctorPtr :: a -> (a, a -> a)
    -- | `makeCTArray` retunrs a multidimensional array based on the arguments (list of each dimension).
    -- e.g.:
    --
    -- >>> makeCTArray [1, 2] CTInt
    -- int[1][2]
    -- >>> makeCTArray [1, 2] (CTArray 2 CTInt)
    -- int[2][1][2]
    makeCTArray :: [Natural] -> a -> a
    -- | Only if both arguments is `Htcc.CRules.Types.Core.CTArray`, 
    -- `concatCTArray` returns a new multidimensional array by conbining the types of
    -- multidimensional arrays as follows.
    -- 
    -- >>> makeCTArray [1, 2] CTInt `concatCTArray` makeCTArray [3, 4] CTInt
    -- Just int[1][2][3][4]
    -- >>> CTInt `concatCTArray` CTArray 2 CTInt
    -- Nothing
    concatCTArray :: a -> a -> Maybe a
    -- | `removeAllExtents` is the same as @std::remove_all_extents@ defined in C++11 @\<type_traits\>@ 
    -- (See also: <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3337.pdf N3337>/§ 20.9.7.4) header.
    -- If type @T@ is a multidimensional array of type @X@, type @X@ is returned.
    -- Otherwise, it returns type @T@.
    removeAllExtents :: a -> a
    -- | `conversion` defines one type from two types according to the implicit conversion defined in §6.3.1.8
    conversion :: a -> a -> a
    -- | `implicitInt` sets long or short type declarations for type declarations with only modifiers such as long and short.
    -- Otherwise, nothing to do.
    implicitInt :: a -> a
