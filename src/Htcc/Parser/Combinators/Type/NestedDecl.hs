{-|
Module      : Htcc.Parser.Combinators.Type.NestedDecl
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE TupleSections #-}
module Htcc.Parser.Combinators.Type.NestedDecl (
    nestedDeclType
) where

import           Data.Bits                          (Bits (..))
import qualified Data.Text                          as T
import qualified Htcc.CRules.Types                  as CT
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Type.Core  (arraySuffix)
import           Htcc.Parser.Combinators.Type.Utils
import           Htcc.Utils                         (dropFst3, swap)
import qualified Text.Megaparsec                    as M

-- TODO: Allow the function pointer type
nestedDeclType :: (Integral i, Show i, Read i, Bits i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i, Maybe T.Text)
nestedDeclType ty = do
    ty' <- flip id ty <$> takeCtorPtr
    M.option (ty', Nothing) $ M.choice
        [ M.lookAhead lparen *> (swap . dropFst3 <$> nestedDeclType' id ty')
        , swap <$> ((,) <$> (Just <$> identifier) <*> M.option ty' (arraySuffix ty'))
        ]
    where
        nestedDeclType' fn ty' = do
            ptrf <- takeCtorPtr
            M.choice
                [ nested ptrf
                , nonNested ptrf
                ]
            where
                nested ptrf = do
                    (ptrf', ident, ty'') <- parens $ nestedDeclType' (fn . ptrf) ty'
                    M.option (id, ident, ptrf' ty'') ((id, ident,) . ptrf' <$> arraySuffix ty'')

                nonNested ptrf = (ptrf,,)
                    <$> (Just <$> identifier)
                    <*> M.option ty' (arraySuffix ty')
