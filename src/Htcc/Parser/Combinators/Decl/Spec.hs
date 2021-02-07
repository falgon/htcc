{-|
Module      : Htcc.Parser.Combinators.Decl.Spec
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
module Htcc.Parser.Combinators.Decl.Spec (
    declspec
) where

import           Data.Functor                       ((<&>))
import qualified Data.Text                          as T
import qualified Htcc.CRules.Types                  as CT
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Parser.Combinators.Type.Utils
import qualified Text.Megaparsec                    as M

declspec',
    declspec :: (Show i, Read i, Integral i) => Parser i (CT.StorageClass i)

declspec' = M.choice
    [ kStatic   *> (CT.SCStatic . CT.toTypeKind <$> declspec')
    , kRegister *> (CT.SCRegister . CT.toTypeKind <$> declspec')
    , kAuto     *> declspec'
    -- , struct
    , M.choice kBasicTypes <&> CT.SCAuto . CT.toTypeKind . CT.implicitInt . read' . T.unpack
    ]
    where
        read' :: (Show i, Read i, Integral i)
            => String
            -> CT.TypeKind i
        read' = read

declspec = declspec' >>= starsToPtr
