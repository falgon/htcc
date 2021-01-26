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
    arraySuffix
  , funcParams
  , declspec
  , declIdent
) where
import           Control.Monad                           (mfilter)
import           Control.Monad.Combinators               (choice)
import           Control.Monad.Trans                     (MonadTrans (..))
import           Control.Monad.Trans.Maybe               (MaybeT (..),
                                                          runMaybeT)
import           Control.Monad.Trans.State               (gets)
import           Data.Bits                               (Bits (..))
import           Data.Either                             (rights)
import           Data.Functor                            ((<&>))
import           Data.Maybe                              (fromJust)
import qualified Data.Text                               as T
import           Data.Tuple.Extra                        (dupe, first)
import qualified Htcc.CRules.Types                       as CT
import           Htcc.Parser.Combinators.ConstExpr       (evalConstexpr)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import {-# SOURCE #-} Htcc.Parser.Combinators.Type.NestedDecl
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
    -> Parser i (CT.StorageClass i, [(CT.StorageClass i, T.Text)])
funcParams ty = choice
    [ (CT.wrapCTFunc ty [], []) <$ (symbol "void" *> rparen)
    , withParams <&> \p -> (CT.wrapCTFunc ty $ map (either id fst) p, rights p)
    ]
    where
        withParams = M.manyTill
            (M.try (declIdentFuncParam comma) M.<|> declIdentFuncParam (M.lookAhead rparen))
            rparen

        declIdentFuncParam sep = declIdent >>= \case
            (ty', Nothing) -> Left ty' <$ sep
            (ty', Just ident) -> Right (narrowPtr ty', ident) <$ sep
            where
                narrowPtr ty'
                    | CT.isCTArray ty' = maybe ty' (CT.mapTypeKind CT.CTPtr) $ CT.deref ty'
                    | CT.isIncompleteArray ty' = flip CT.mapTypeKind ty' $
                        \(CT.CTIncomplete (CT.IncompleteArray ty'')) -> CT.CTPtr ty''
                    | otherwise = ty'
{-
typeSuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
typeSuffix ty = M.option ty $ choice
    [ arraySuffix ty
    , lparen *> funcParams ty <&> fst
    ]
-}

declspec',
    declspec :: (Show i, Read i, Integral i) => Parser i (CT.StorageClass i)

declspec' = choice
    [ kStatic   *> (CT.SCStatic . CT.toTypeKind <$> declspec')
    , kRegister *> (CT.SCRegister . CT.toTypeKind <$> declspec')
    , kAuto     *> declspec'
    -- , struct
    , choice kBasicTypes <&> CT.SCAuto . CT.toTypeKind . CT.implicitInt . read' . T.unpack
    ]
    where
        read' :: (Show i, Read i, Integral i)
            => String
            -> CT.TypeKind i
        read' = read

declspec = do
    pt <- declspec'
    fn <- CT.ctorPtr . toNatural . length <$> M.many star
    pure $ fn pt

declIdent :: (Show i, Read i, Bits i, Integral i) => Parser i (CT.StorageClass i, Maybe T.Text)
declIdent = M.try declspec >>= nestedDeclType
