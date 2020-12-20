{-|
Module      : Htcc.Parser.Combinators.Type
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.Type (
    cType
) where
import           Control.Monad.Combinators        (choice)
import qualified Data.Text                        as T
import qualified Htcc.CRules.Types                as CT
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import           Htcc.Utils                       (toNatural)
import qualified Text.Megaparsec                  as M


preType, cType :: (Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
preType = choice
    [ kStatic   >> (CT.SCStatic . CT.toTypeKind <$> preType)
    , kRegister >> (CT.SCRegister . CT.toTypeKind <$> preType)
    , kAuto     >> preType
    , CT.SCAuto . CT.toTypeKind . CT.implicitInt . read' . T.unpack
        <$> choice kBasicTypes
    ]
    where
        read' :: (Show i, Read i, Integral i)
            => String
            -> CT.TypeKind i
        read' = read

cType = flip id 
    <$> preType 
    <*> (CT.ctorPtr . toNatural . length <$> M.many (symbol "*"))
