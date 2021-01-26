{-|
Module      : Htcc.Parser.Combinators.Type.AbsDecl
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE TupleSections #-}
module Htcc.Parser.Combinators.Type.AbsDecl (
    absDeclType
) where

import           Data.Bits                         (Bits (..))
import qualified Htcc.CRules.Types                 as CT
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Type.Core (arraySuffix, declspec)
import Htcc.Parser.Combinators.Type.Utils (takeCtorPtr)
import qualified Text.Megaparsec                   as M

-- TODO: Allow the function pointer type
absDeclType :: (Integral i, Show i, Read i, Bits i) => Parser i (CT.StorageClass i)
absDeclType = do
    ty <- declspec
    if CT.isSCStatic ty {- TODO: or register -} then fail "storage-class specifier is not allowed" else do
        ty' <- flip id ty <$> takeCtorPtr
        M.choice
            [ arraySuffix ty'
            , snd <$> absDeclType' id ty'
            ]
    where
        absDeclType' fn ty = do
            cpfn <- takeCtorPtr
            M.option (cpfn, ty) $ do
                (cpfn', ty') <- parens $ absDeclType' (fn . cpfn) ty
                M.option (id, cpfn' ty') ((id,) . cpfn' <$> arraySuffix ty')
