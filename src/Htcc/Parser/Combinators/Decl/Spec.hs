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
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Htcc.Parser.Combinators.Decl.Spec (
    DeclStorage (..)
  , declarationSpec
  , declspec
) where

import                          Control.Applicative                           (some,
                                                                               (<|>))
import                          Control.Monad                                 (void,
                                                                               when)
import                          Control.Monad.State                           (get,
                                                                               gets,
                                                                               put)
import                          Data.Bits                                     (Bits)
import                          Data.Functor                                  (($>),
                                                                               (<&>))
import                qualified Data.Map.Strict                               as MP
import                          Data.Maybe                                    (catMaybes)
import                qualified Data.Text                                     as T
import                qualified Htcc.CRules.Types                             as CT
import                          Htcc.Parser.Combinators.ConstExpr             (evalConstexpr)
import                          Htcc.Parser.Combinators.Core
import {-# SOURCE #-}           Htcc.Parser.Combinators.Decl.Declarator       (declarator)
import                          Htcc.Parser.Combinators.Keywords
import                          Htcc.Parser.Combinators.Type.Utils
import                          Htcc.Parser.Combinators.Utils                 (captureFunctionParamScopes)
import                          Htcc.Parser.ConstructionData.Core             (ConstructionData (scope),
                                                                               addEnumerator,
                                                                               addTag,
                                                                               hasIncompleteObjectType,
                                                                               lookupEnumerator,
                                                                               lookupFunction,
                                                                               lookupGVar,
                                                                               lookupLVar,
                                                                               lookupTag,
                                                                               lookupTypedef,
                                                                               normalizeCompletedStorageClass)
import                          Htcc.Parser.ConstructionData.Scope            (Scoped (curNestDepth, curScopeId))
import                qualified Htcc.Parser.ConstructionData.Scope.Enumerator as PSE
import                qualified Htcc.Parser.ConstructionData.Scope.Function   as PSF
import                qualified Htcc.Parser.ConstructionData.Scope.Tag        as PST
import                qualified Htcc.Parser.ConstructionData.Scope.Typedef    as PT
import                qualified Htcc.Parser.ConstructionData.Scope.Var        as PSV
import                qualified Htcc.Tokenizer.Token                          as HT
import                qualified Htcc.Utils                                    as U
import                          Numeric.Natural                               (Natural)
import                qualified Text.Megaparsec                               as M

data DeclStorage
    = OrdinaryDecl
    | TypedefDecl
    | ExternDecl
    | AutoDecl
    deriving (Eq, Show)

declspec',
    declspec :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (CT.StorageClass i)

declarationSpec :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (DeclStorage, CT.StorageClass i)
declarationSpec =
    leadingExternOrTypedef
        <|> ordinaryWithLeadingStorage
        <|> ordinaryOrTrailingExternOrTypedef
    where
        leadingExternOrTypedef = do
            storage <- externOrTypedefStorage
            ty <- declspecNoStorage
            pure (storage, ty)

        ordinaryWithLeadingStorage =
            explicitAuto <|> explicitStaticOrRegister
            where
                explicitAuto = do
                    void kAuto
                    ty <- declspecNoStorage
                    pure (AutoDecl, ty)
                explicitStaticOrRegister = do
                    void $ M.lookAhead $ M.choice [kStatic, kRegister]
                    ty <- declspec
                    pure (OrdinaryDecl, ty)

        ordinaryOrTrailingExternOrTypedef = do
            ty <- declspecNoStorage
            storage <- M.option OrdinaryDecl externOrTypedefStorage
            pure (storage, ty)

        externOrTypedefStorage =
            M.choice
                [ TypedefDecl <$ M.try kTypedef
                , ExternDecl <$ M.try kExtern
                ]

declspec' = M.choice
    [ kStatic   *> (CT.SCStatic . CT.toTypeKind <$> declspecNoStorage)
    , kRegister *> (CT.SCRegister . CT.toTypeKind <$> declspecNoStorage)
    , kAuto     *> declspecNoStorage
    , declspecNoStorage
    ]

declspecNoStorage :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
declspecNoStorage =
    M.choice
        [ M.try structSpecifier
        , M.try enumSpecifier
        , M.try typedefSpecifier
        , basicTypeSpecifier
        ]

basicTypeSpecifier :: (Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
basicTypeSpecifier = do
    specifiers <- some $ M.choice $ map M.try kBasicTypes
    validateBasicTypeSpecifiers specifiers
    pure $ CT.SCAuto $ toBasicTypeKind specifiers
    where
        toBasicTypeKind specifiers
            | has "void" specifiers = CT.CTVoid
            | has "_Bool" specifiers = CT.CTBool
            | has "char" specifiers && has "signed" specifiers = CT.CTSigned CT.CTChar
            | has "char" specifiers = CT.CTChar
            | count "long" specifiers == 2 = CT.CTLong $ CT.CTLong CT.CTInt
            | count "long" specifiers == 1 = CT.CTLong CT.CTInt
            | has "short" specifiers = CT.CTShort CT.CTInt
            | otherwise = CT.CTInt

        validateBasicTypeSpecifiers specifiers
            | any has ["double", "float", "unsigned", "_Complex", "_Imaginary"] = invalidCombination specifiers
            | count "long" > 2 = invalidCombination specifiers
            | count "signed" > 1 = invalidCombination specifiers
            | count "short" > 1 = invalidCombination specifiers
            | count "short" > 0 && count "long" > 0 = invalidCombination specifiers
            | any ((> 1) . count) ["char", "int", "_Bool", "void"] = invalidCombination specifiers
            | has "void" && length specifiers > 1 = invalidCombination specifiers
            | has "_Bool" && length specifiers > 1 = invalidCombination specifiers
            | has "char" && any has ["int", "long", "short"] = invalidCombination specifiers
            | baseSpecifierCount > 1 = invalidCombination specifiers
            | otherwise = pure ()
            where
                count keyword =
                    length $ filter (== T.pack keyword) specifiers
                has keyword =
                    count keyword > 0
                baseSpecifierCount =
                    length $ filter (`elem` map T.pack ["char", "int", "_Bool", "void"]) specifiers

        count keyword =
            length . filter (== T.pack keyword)

        has keyword =
            (> 0) . count keyword

        invalidCombination specifiers =
            fail $
                "invalid type specifier combination '"
                    <> T.unpack (T.unwords specifiers)
                    <> "'"

structSpecifier :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
structSpecifier = do
    void kStruct
    mtag <- M.option Nothing $ Just <$> M.try identifier
    ifMtaggedStruct mtag
    where
        ifMtaggedStruct mtag =
            (M.lookAhead lbrace *> defineStruct mtag)
                <|> resolveStructTag mtag

        defineStruct Nothing = do
            anonymousTag <- mkAnonymousStructTag
            members <- braces structMembers
            scopeId <- gets $ curScopeId . scope
            pure $ CT.SCAuto $ CT.CTNamedStruct anonymousTag scopeId members
        defineStruct (Just tag) = do
            scopeId <- gets $ curScopeId . scope
            registerTagType PST.StructTag (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteStruct tag scopeId) tag
            members <- braces structMembers
            let ty = CT.SCAuto $ CT.CTNamedStruct tag scopeId members
            registerTagType PST.StructTag ty tag
            pure ty

        resolveStructTag Nothing =
            fail "expected identifier or '{' after 'struct'"
        resolveStructTag (Just tag) =
            do
                standaloneForwardDecl <- M.option False $ True <$ M.lookAhead semi
                scp <- get
                let depth = curNestDepth $ scope scp
                    scopeId = curScopeId $ scope scp
                    incompleteStructTy =
                        CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteStruct tag scopeId
                maybe
                    (registerIncompleteStructTag tag $> incompleteStructTy)
                    (ensureStructTag depth scopeId standaloneForwardDecl)
                    (lookupTag tag scp)
            where
                ensureStructTag depth scopeId standaloneForwardDecl tagInfo
                    | standaloneForwardDecl && PST.stNestDepth tagInfo < depth =
                        registerIncompleteStructTag tag
                            $> CT.SCAuto (CT.CTIncomplete $ CT.IncompleteStruct tag scopeId)
                    | PST.stKind tagInfo == PST.StructTag = pure $ PST.sttype tagInfo
                    | otherwise = fail $ "use of 'struct " <> T.unpack tag <> "' with wrong tag type"

        structMembers = go (0 :: Natural) MP.empty
            where
                go offset acc =
                    (M.lookAhead rbrace $> acc)
                        <|> do
                            (nextOffset, name, mem) <- structMember offset
                            when (MP.member name acc) $
                                fail $ "duplicate member '" <> T.unpack name <> "'"
                            go nextOffset $ MP.insert name mem acc

                structMember offset = do
                    rejectMemberStorageClass
                    memberBaseTy <- declspecNoStorage
                    ((memberTy, mident), _) <- captureFunctionParamScopes $ declarator memberBaseTy
                    ident <- maybe
                        (fail "expected member name or ';' after declaration specifiers")
                        pure
                        mident
                    resolvedTy <- gets (`normalizeCompletedStorageClass` memberTy)
                    when (hasIncompleteObjectType resolvedTy) $
                        fail "declaration with incomplete type"
                    when (isVoidObjectType resolvedTy) $
                        fail $ "variable or field '" <> T.unpack ident <> "' declarated void"
                    when (isFunctionMemberType resolvedTy) $
                        fail $ "field '" <> T.unpack ident <> "' declared as a function"
                    void semi
                    let memberOffset =
                            U.toNatural $
                                CT.alignas
                                    (U.toInteger offset)
                                    (fromIntegral $ CT.alignof resolvedTy)
                        memberInfo =
                            CT.StructMember
                                (CT.toTypeKind resolvedTy)
                                memberOffset
                    pure
                        ( memberOffset + fromIntegral (CT.sizeof resolvedTy)
                        , ident
                        , memberInfo
                        )

                rejectMemberStorageClass =
                    M.lookAhead $
                        M.choice
                            [ kAuto *> fail "invalid storage-class specifier"
                            , kStatic *> fail "invalid storage-class specifier"
                            , kRegister *> fail "invalid storage-class specifier"
                            , kExtern *> fail "invalid storage-class specifier"
                            , kTypedef *> fail "invalid storage-class specifier"
                            , pure ()
                            ]

        mkAnonymousStructTag = do
            pos <- M.getSourcePos
            pure $ T.pack ".anonymous.struct." <> U.tshow pos

enumSpecifier :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
enumSpecifier = do
    void kEnum
    mtag <- M.option Nothing $ Just <$> M.try identifier
    (M.lookAhead lbrace *> defineEnum mtag)
        <|> resolveEnumTag mtag
    where
        defineEnum Nothing = CT.SCAuto . CT.CTEnum CT.CTInt <$> braces enumMembers
        defineEnum (Just tag) = do
            members <- braces enumMembers
            let ty = CT.SCAuto $ CT.CTEnum CT.CTInt members
            registerTagType PST.EnumTag ty tag
            pure ty

        resolveEnumTag Nothing =
            fail "expected identifier or '{' after 'enum'"
        resolveEnumTag (Just tag) =
            gets (lookupTag tag)
                >>= maybe
                    (fail $ "storage size of '" <> T.unpack tag <> "' isn't known")
                    ensureEnumTag
            where
                ensureEnumTag tagInfo
                    | PST.stKind tagInfo == PST.EnumTag = pure $ PST.sttype tagInfo
                    | otherwise = fail $ "use of 'enum " <> T.unpack tag <> "' with wrong tag type"

        enumMembers = go 0 MP.empty
            where
                enumTy = CT.SCAuto CT.CTInt

                go nextVal acc =
                    (M.lookAhead rbrace *>
                     if MP.null acc
                         then fail "use of empty enum"
                         else pure acc)
                        <|> do
                            ident <- identifier
                            val <- M.option nextVal $ equal *> evalConstexpr
                            registerEnumeratorValue enumTy ident val
                            hasComma <- M.option False (True <$ comma)
                            let acc' = MP.insert ident val acc
                                nextVal' = succ val
                            if hasComma then
                                (M.lookAhead rbrace $> acc') <|> go nextVal' acc'
                            else
                                pure acc'

typedefSpecifier :: (Ord i, Bits i, Show i, Read i, Integral i) => Parser i (CT.StorageClass i)
typedefSpecifier =
    M.try $
        identifier >>= \ident -> do
            mTypedef <- gets $ lookupTypedef ident
            ordinaryDepth <- visibleOrdinaryIdentifierDepth ident
            case mTypedef of
                Just td
                    | maybe False (>= PT.tdNestDepth td) ordinaryDepth ->
                        fail $ "'" <> T.unpack ident <> "' is not a type or also a typedef identifier"
                    | otherwise ->
                        pure $ PT.tdtype td
                Nothing ->
                    fail $ "'" <> T.unpack ident <> "' is not a type or also a typedef identifier"

declspec = declspec' >>= starsToPtr

isVoidObjectType :: CT.StorageClass i -> Bool
isVoidObjectType = go . CT.toTypeKind
    where
        go = \case
            CT.CTLong innerTy                       -> go innerTy
            CT.CTShort innerTy                      -> go innerTy
            CT.CTSigned innerTy                     -> go innerTy
            CT.CTArray _ innerTy                    -> go innerTy
            CT.CTIncomplete (CT.IncompleteArray innerTy) -> go innerTy
            CT.CTVoid                               -> True
            _                                       -> False

isFunctionMemberType :: CT.StorageClass i -> Bool
isFunctionMemberType = go . CT.toTypeKind
    where
        go = \case
            CT.CTLong innerTy   -> go innerTy
            CT.CTShort innerTy  -> go innerTy
            CT.CTSigned innerTy -> go innerTy
            CT.CTFunc _ _       -> True
            _                   -> False

visibleOrdinaryIdentifierDepth :: T.Text -> Parser i (Maybe Natural)
visibleOrdinaryIdentifierDepth ident =
    gets $ \scp ->
        let depths =
                catMaybes
                    [ PSV.nestDepth <$> lookupLVar ident scp
                    , PSE.enNestDepth <$> lookupEnumerator ident scp
                    , PSV.gvNestDepth <$> lookupGVar ident scp
                    , PSF.fnNestDepth <$> lookupFunction ident scp
                    ]
        in
            if null depths then Nothing else Just $ maximum depths

tmpTKIdent :: Num i => T.Text -> HT.TokenLC i
tmpTKIdent ident = (HT.TokenLCNums 1 1, HT.TKIdent ident)

registerTagType :: Num i => PST.TagKind -> CT.StorageClass i -> T.Text -> Parser i ()
registerTagType kind ty ident = do
    scp <- get
    case addTag kind ty (tmpTKIdent ident) scp of
        Left err   -> fail $ T.unpack $ fst err
        Right scp' -> put scp'

registerIncompleteStructTag :: Num i => T.Text -> Parser i ()
registerIncompleteStructTag ident = do
    scopeId <- gets $ curScopeId . scope
    registerTagType PST.StructTag (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteStruct ident scopeId) ident

registerEnumeratorValue :: Num i => CT.StorageClass i -> T.Text -> i -> Parser i ()
registerEnumeratorValue ty ident val = do
    scp <- get
    case addEnumerator ty (tmpTKIdent ident) val scp of
        Left err   -> fail $ T.unpack $ fst err
        Right scp' -> put scp'
