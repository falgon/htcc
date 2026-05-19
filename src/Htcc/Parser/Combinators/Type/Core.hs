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
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}
module Htcc.Parser.Combinators.Type.Core (
    typeSuffix
  -- * Helper functions
  , toNamedParams
) where
import                          Control.Applicative                     ((<|>))
import                          Control.Monad                           (mfilter,
                                                                         void,
                                                                         when)
import                          Control.Monad.Combinators               (choice)
import                          Control.Monad.State                     (get,
                                                                         gets,
                                                                         modify,
                                                                         put)
import                          Control.Monad.Trans                     (MonadTrans (..))
import                          Control.Monad.Trans.Maybe               (MaybeT (..),
                                                                         runMaybeT)
import                          Data.Bifunctor                          (bimap,
                                                                         first)
import                          Data.Bits                               (Bits (..))
import                          Data.Functor                            ((<&>))
import                          Data.Maybe                              (fromJust,
                                                                         isJust)
import                qualified Data.Text                               as T
import                          Data.Tuple.Extra                        (dupe)
import                qualified Htcc.CRules.Types                       as CT
import                          Htcc.Parser.Combinators.ConstExpr       (evalConstexpr)
import                          Htcc.Parser.Combinators.Core
import {-# SOURCE #-}           Htcc.Parser.Combinators.Decl.Declarator
import                          Htcc.Parser.Combinators.Decl.Spec       (declspec)
import                          Htcc.Parser.Combinators.Keywords        (kVoid)
import                          Htcc.Parser.Combinators.Utils           (registerLVar)
import                          Htcc.Parser.ConstructionData.Core       (ConstructionData (functionParamScopes, scope, tagHistory),
                                                                         FunctionParamScope (..),
                                                                         incomplete,
                                                                         succNest)
import                          Htcc.Parser.ConstructionData.Scope      (Scoped (curScopeId, enumerators, nextScopeId, structs))
import                          Htcc.Utils                              (toNatural)
import                qualified Text.Megaparsec                         as M

arraySuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
arraySuffix ty = lbracket *> choice
    [ nonConstantExp
    , withConstantExp
    ]
    where
        failWithTypeMaybe ty' = maybe (fail $ show ty') pure

        withConstantExp = do
            len <- evalConstexpr
            when (len < 0) $
                fail "array bound is negative"
            void rbracket
            let arty = flip id ty . CT.mapTypeKind . CT.CTArray $ toNatural len
            M.option Nothing (Just <$> arraySuffix ty)
                >>= \case
                    Nothing -> pure arty
                    Just ty'
                        | CT.isIncompleteArray ty' ->
                            failWithTypeMaybe ty' Nothing
                    Just ty' ->
                        runMaybeT (mfilter CT.isValidIncomplete $ MaybeT $ pure $ CT.concatCTArray arty ty')
                            >>= failWithTypeMaybe ty'

        nonConstantExp = let mtIncomplete ty' = MaybeT $ lift $ gets $ incomplete ty' in
            rbracket
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
funcParams ty = lparen *> do
    pre <- get
    modify succNest
    params <- scopedParams
    post <- get
    let paramScope = scope post
        carry =
            FunctionParamScope
                { fpsScopeId = curScopeId paramScope
                , fpsTags = structs paramScope
                , fpsEnumerators = enumerators paramScope
                }
        restoredScope = (scope pre) { nextScopeId = nextScopeId paramScope }
    put $
        pre
            { scope = restoredScope
            , tagHistory = tagHistory post
            , functionParamScopes = carry : functionParamScopes post
            }
    pure $ CT.wrapCTFunc ty params
    where
        scopedParams =
            choice
                [ M.try $ [(CT.SCAuto CT.CTVoid, Nothing)] <$ (kVoid *> rparen)
                , [] <$ rparen
                , withParams
                ]

        withParams = do
            firstParam <- declIdentFuncParam
            restParams <- M.many (comma *> declIdentFuncParam)
            void rparen
            validateVoidParams $ firstParam : restParams

        declIdentFuncParam = do
            ty' <- M.try declspec
            M.choice
                [ M.try $ (ty', Nothing) <$ M.lookAhead (comma <|> rparen)
                , declarator ty' >>= \(t, mIdent) -> do
                    rejectVoidArrayParam t
                    let paramTy = narrowPtr t
                    case mIdent of
                        Nothing ->
                            pure ()
                        Just ident ->
                            void $ registerLVar paramTy ident
                    pure (paramTy, mIdent)
                ]
            where
                rejectVoidArrayParam paramTy
                    | containsVoidArrayType $ CT.toTypeKind paramTy =
                        fail "parameter declared as array of void"
                    | otherwise = pure ()

                containsVoidArrayType = \case
                    CT.CTPtr ty'' ->
                        containsVoidArrayType ty''
                    CT.CTArray _ ty'' ->
                        hasVoidArrayElement ty''
                    CT.CTIncomplete (CT.IncompleteArray ty'') ->
                        hasVoidArrayElement ty''
                    _ ->
                        False

                hasVoidArrayElement = \case
                    CT.CTArray _ ty'' ->
                        hasVoidArrayElement ty''
                    CT.CTIncomplete (CT.IncompleteArray ty'') ->
                        hasVoidArrayElement ty''
                    CT.CTVoid ->
                        True
                    _ ->
                        False

                narrowPtr ty'
                    | CT.isCTArray ty' = maybe ty' (CT.mapTypeKind CT.CTPtr) $ CT.deref ty'
                    | CT.isIncompleteArray ty' = flip CT.mapTypeKind ty' $
                        \(CT.CTIncomplete (CT.IncompleteArray ty'')) -> CT.CTPtr ty''
                    | otherwise = ty'

        validateVoidParams params
            | isSingleUnnamedVoid params = pure params
            | any (isVoidParam . fst) params = fail "parameter declared void"
            | otherwise = pure params

        isSingleUnnamedVoid [(paramTy, Nothing)] = isVoidParam paramTy
        isSingleUnnamedVoid _                    = False

        isVoidParam = (CT.CTVoid ==) . CT.toTypeKind

toNamedParams :: (Show i, Read i, Integral i, Bits i)
    => CT.StorageClass i
    -> Parser i [(CT.StorageClass i, T.Text)]
toNamedParams ty = case CT.toTypeKind ty of
    (CT.CTFunc _ params) -> pure
        [ bimap CT.SCAuto fromJust p
        | p <- params
        , fst p /= CT.CTVoid
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
