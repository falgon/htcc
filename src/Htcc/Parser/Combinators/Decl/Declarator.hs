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
{-# LANGUAGE TupleSections #-}
module Htcc.Parser.Combinators.Decl.Declarator (
    declarator
) where

import           Control.Monad.Fix                  (fix)
import           Data.Bits                          (Bits (..))
import qualified Data.Text                          as T
import           Data.Tuple.Extra                   (uncurry3)
import qualified Htcc.CRules.Types                  as CT
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Type.Core  (typeSuffix)
import           Htcc.Parser.Combinators.Type.Utils
import           Htcc.Utils                         (dropFst3, swap)
import qualified Text.Megaparsec                    as M

declarator :: (Integral i, Show i, Read i, Bits i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i, Maybe T.Text)
declarator ty = do
    ty' <- starsToPtr ty
    M.option (ty', Nothing) $ M.choice
        [ nested ty'
        , swap <$> ((,) <$> M.option Nothing (Just <$> identifier) <*> typeSuffix ty')
        ]
    where
        nested ty' = fmap (swap . dropFst3)
            $ ($ (id, ty'))
            $ fix $ \f (fn, ty'') -> do
                ptrf <- (fn .) <$> starsToPtrCtor
                M.choice
                    [ parens (f (ptrf, ty'')) >>= uncurry3 nested'
                    , (ptrf,,) <$> (Just <$> identifier) <*> M.option ty'' (typeSuffix ty'')
                    ]
            where
                nested' ptrf ident t =
                    M.option (id, ident, ptrf t) ((id, ident,) . ptrf <$> typeSuffix t)
