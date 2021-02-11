{-|
Module      : Htcc.Parser.Combinators.Type.Core
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module Htcc.Parser.Combinators.Type.Core (
    typeSuffix
  -- * Helper functions
  , toNamedParams
) where
import           Control.Monad                           (mfilter)
import           Control.Monad.Combinators               (choice)
import           Control.Monad.Trans                     (MonadTrans (..))
import           Control.Monad.Trans.Maybe               (MaybeT (..),
                                                          runMaybeT)
import           Control.Monad.Trans.State               (gets)
import           Data.Bits                               (Bits (..))
import           Data.Functor                            ((<&>))
import           Data.Maybe                              (fromJust, isJust)
import qualified Data.Text                               as T
import           Data.Tuple.Extra                        (dupe, first, second)
import qualified Htcc.CRules.Types                       as CT
import           Htcc.Parser.Combinators.ConstExpr       (evalConstexpr)
import           Htcc.Parser.Combinators.Core
import {-# SOURCE #-} Htcc.Parser.Combinators.Decl.Declarator
import           Htcc.Parser.Combinators.Decl.Spec       (declspec)
import           Htcc.Parser.ConstructionData.Core       (incomplete)
import           Htcc.Utils                              (toNatural)
import qualified Text.Megaparsec                         as M

arraySuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
arraySuffix ty = choice
    [ withConstantExp
    , nonConstantExp
    ]
    where
        failWithTypeMaybe ty' = maybe (fail $ show ty') pure

        withConstantExp = do
            arty <- flip id ty . CT.mapTypeKind . CT.CTArray . toNatural <$> M.try (brackets evalConstexpr)
            M.option Nothing (Just <$> arraySuffix ty)
                >>= \case
                    Nothing -> pure arty
                    Just ty' ->
                        runMaybeT (mfilter CT.isValidIncomplete $ MaybeT $ pure $ CT.concatCTArray arty ty')
                            >>= failWithTypeMaybe ty'

        nonConstantExp = let mtIncomplete ty' = MaybeT $ lift $ gets $ incomplete ty' in
            symbol "["
                *> symbol "]"
                *> M.option Nothing (Just <$> arraySuffix ty)
                >>= \case
                    Nothing ->
                        runMaybeT (CT.mapTypeKind (CT.CTIncomplete . CT.IncompleteArray) <$> mtIncomplete ty)
                            >>= failWithTypeMaybe ty
                    Just ty' ->
                        runMaybeT (multiple <$> mtIncomplete ty')
                            >>= failWithTypeMaybe ty'
            where
                multiple = CT.mapTypeKind $
                    uncurry ((.) fromJust . CT.concatCTArray)
                        . first (CT.CTIncomplete . CT.IncompleteArray . CT.removeAllExtents)
                        . dupe

funcParams :: (Show i, Read i, Integral i, Bits i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
funcParams ty = lparen
    *> choice
        [ [] <$ (symbol "void" *> rparen)
        , withParams
        ]
    <&> CT.wrapCTFunc ty
    where
        withParams = M.manyTill
            (M.try (declIdentFuncParam comma) M.<|> declIdentFuncParam (M.lookAhead rparen))
            rparen

        declIdentFuncParam sep = do
            ty' <- M.try declspec
            M.choice
                [ (ty', Nothing) <$ sep
                , declarator ty' >>= \case
                    (t, Nothing) -> (t, Nothing) <$ sep
                    (t, Just ident) -> (narrowPtr t, Just ident) <$ sep
                ]
            where
                narrowPtr ty'
                    | CT.isCTArray ty' = maybe ty' (CT.mapTypeKind CT.CTPtr) $ CT.deref ty'
                    | CT.isIncompleteArray ty' = flip CT.mapTypeKind ty' $
                        \(CT.CTIncomplete (CT.IncompleteArray ty'')) -> CT.CTPtr ty''
                    | otherwise = ty'

toNamedParams :: (Show i, Read i, Integral i, Bits i)
    => CT.StorageClass i
    -> Parser i [(CT.StorageClass i, T.Text)]
toNamedParams ty = case CT.toTypeKind ty of
    (CT.CTFunc _ params) -> pure
        [ first CT.SCAuto $ second fromJust p
        | p <- params
        , isJust $ snd p
        ]
    _ -> fail "expected function parameters"

typeSuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
typeSuffix ty = M.option ty $ choice
    [ arraySuffix ty
    , funcParams ty
    ]
