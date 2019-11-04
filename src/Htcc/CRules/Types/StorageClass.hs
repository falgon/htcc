{-|
Module      : Htcc.CRules.Types.StorageClass
Description : The rules of types of C language
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The rules of storage-class of C language
-}
module Htcc.CRules.Types.StorageClass (
    StorageClass (..)
) where

-- | The data type representing `StorageClass`
data StorageClass = SCAuto -- ^ The @auto@ keyword
    | SCStatic -- ^ The @static@ keyword
    | SCRegister -- ^ The @register@ keyword
    deriving (Eq, Enum)

instance Show StorageClass where
    show SCAuto = "auto"
    show SCStatic = "static"
    show SCRegister = "register"
