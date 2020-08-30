{-|
Module      : Htcc.CRules.Preprocessor.Punctuators
Description : The puncuators of preprocessor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The puncuators of preprocessor
-}
{-# LANGUAGE DeriveGeneric #-}
module Htcc.CRules.Preprocessor.Punctuators (
    -- * The define of C preprocessor
    bgMacro,
    Macros (..),
    -- * Utilities
    macros,
    length
) where

import           Control.DeepSeq (NFData (..))
import           GHC.Generics    (Generic)
import           Prelude         hiding (length)
import qualified Prelude         as P (length)

{-# INLINE bgMacro #-}
-- | `bgMacro` is the character that starts the macro, so it is @#@
bgMacro :: Char
bgMacro = '#'

-- | `Macros` is a macro token defined in C.
data Macros = MacInclude -- ^ the @include@
    deriving (Eq, Enum, Generic)

instance NFData Macros

instance Show Macros where
    show MacInclude = "include"

{-# INLINE macros #-}
-- | all macros
macros :: [Macros]
macros = enumFrom $ toEnum 0

{-# INLINE length #-}
-- | the length of the macro
length :: Macros -> Int
length = P.length . show
