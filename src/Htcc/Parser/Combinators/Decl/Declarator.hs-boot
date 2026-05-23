{-|
Module      : Htcc.Parser.Combinators.Decl.Declarator
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
module Htcc.Parser.Combinators.Decl.Declarator where

import           Data.Bits                    (Bits (..))
import qualified Data.Text                    as T
import qualified Htcc.CRules.Types            as CT
import           Htcc.Parser.Combinators.Core (Parser)

declarator :: (Integral i, Show i, Read i, Bits i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i, Maybe T.Text)
