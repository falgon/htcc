{-|
Module      : Htcc.Parser.Combinators.Type.Utils
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE TupleSections #-}
module Htcc.Parser.Combinators.Type.Utils (
    starsToPtrCtor
  , starsToPtr
) where

import qualified Htcc.CRules.Types            as CT
import           Htcc.Parser.Combinators.Core (Parser, star)
import           Htcc.Utils                   (toNatural)
import qualified Text.Megaparsec              as M

starsToPtrCtor :: Ord i => Parser i (CT.StorageClass i -> CT.StorageClass i)
starsToPtrCtor = CT.ctorPtr . toNatural . length <$> M.many star

starsToPtr :: Ord i => CT.StorageClass i -> Parser i (CT.StorageClass i)
starsToPtr ty = starsToPtrCtor <*> pure ty
