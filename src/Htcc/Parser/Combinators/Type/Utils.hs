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
    takeCtorPtr
) where

import qualified Htcc.CRules.Types            as CT
import           Htcc.Parser.Combinators.Core (Parser, star)
import           Htcc.Utils                   (toNatural)
import qualified Text.Megaparsec              as M

takeCtorPtr :: Ord i => Parser i (CT.StorageClass i -> CT.StorageClass i)
takeCtorPtr = CT.ctorPtr . toNatural . length <$> M.many star
