{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, ScopedTypeVariables,
             TupleSections #-}
{-|
Module      : Htcc.Parser.Parsing.Type
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The module of the Type parsing
-}
module Htcc.Parser.Parsing.Type (
    -- * Constant
    ConstantResult,
    constantExp,
    -- * Utilities
    isTypeName,
    -- * Structure and Enum
    takeStructFields,
    takeEnumFiels,
    -- * Declarations
    arrayDeclSuffix,
    absDeclaration,
    declaration,
    -- * Type
    takePreType,
    takeType,
    takeTypeName
) where

import           Data.Bits                                       hiding (shift)
import           Data.Bool                                       (bool)
import qualified Data.Map.Strict                                 as M
import           Data.Maybe                                      (fromJust,
                                                                  fromMaybe,
                                                                  isJust)
import qualified Data.Text                                       as T
import           Data.Tuple.Extra                                (dupe, first,
                                                                  uncurry3)
import           Prelude                                         hiding
                                                                  (toInteger)

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST
import           Htcc.Parser.ConstructionData
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import qualified Htcc.Parser.ConstructionData.Scope.Tag          as PST
import qualified Htcc.Parser.ConstructionData.Scope.Typedef      as PSD
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import {-# SOURCE #-} Htcc.Parser.Parsing.Core                        (conditional)
import           Htcc.Parser.Utils
import qualified Htcc.Tokenizer                                  as HT
import           Htcc.Utils                                      (dropFst3,
                                                                  dropFst4,
                                                                  dropSnd3,
                                                                  first3,
                                                                  maybe',
                                                                  maybeToRight,
                                                                  spanLen,
                                                                  toInteger,
                                                                  toNatural,
                                                                  tshow)

-- | \[
-- \begin{array}{ccc}
-- \text{struct-decl}&=&\text{"struct"}\ \text{ident?}\ \left(\text{"\{"}\ \text{struct-member}\ \text{"\}"}\right)\text{?}\\
-- \text{struct-member}&=&\text{pre-type}\ \text{declaration}\ \text{array-decl-suffix}\ \text{";"}
-- \end{array}
-- \]
takeStructFields :: (Integral i, Show i, Read i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (M.Map T.Text (CT.StructMember i), ConstructionData i)
takeStructFields tk sc = takeStructFields' tk sc 0
    where
        takeStructFields' [] scp' _ = Right (M.empty, scp')
        takeStructFields' fs scp' !n = (>>=) (takeType fs scp' >>= validDecl (HT.altEmptyToken tk)) $ \case
            (ty@(CT.SCAuto _), Just (_, HT.TKIdent ident), (_, HT.TKReserved ";"):ds, scp'') -> let ofs = toNatural $ CT.alignas (toInteger n) $ toInteger $ CT.alignof ty in
                first (M.insert ident (CT.StructMember (CT.toTypeKind ty) ofs)) <$> takeStructFields' ds scp'' (ofs + fromIntegral (CT.sizeof ty))
            (_, Just _, _, _) -> Left ("invalid storage-class specifier", head fs)
            _ -> Left ("expected member name or ';' after declaration specifiers", HT.altEmptyToken fs)
        validDecl _ (t, Just ident, tks, scp) = maybeToRight ("declaration with incomplete type", ident) (incomplete t scp) >>= \t' -> if CT.toTypeKind t == CT.CTVoid then
            Left ("variable or field '" <> tshow (snd ident) <> "' declarated void", ident) else Right (t', Just ident, tks, scp)
        validDecl errPlaceholder (t, noth, tks, scp) = maybeToRight ("declaration with incomplete type", errPlaceholder) (incomplete t scp) >>= \t' -> if CT.toTypeKind t == CT.CTVoid then
            Left ("declarations of type void is invalid in this context", errPlaceholder) else Right (t', noth, tks, scp)

-- | \[
-- \begin{array}{ccc}
-- \text{enum-specifier}&=&\text{"enum"}\ \text{ident}\ \mid\ \text{"enum"}\ \text{ident?}\ \text{"\{"}\ \text{enum-list?}\ \text{"\}"}\\
-- \text{enum-list}&=&\text{enum-elem}\ \left(\text{","}\ \text{enum-elem}\right)\ast\ \text{","?}\\
-- \text{enum-elem}&=&\text{ident}\ \left(\text{"="}\ \text{const-expr}\right)\text{?}
-- \end{array}
-- \]
takeEnumFiels :: (Integral i, Show i, Read i, Bits i) => CT.StorageClass i -> [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (M.Map T.Text i, ConstructionData i)
takeEnumFiels = takeEnumFiels' 0
    where
        takeEnumFiels' !n ty [cur@(_, HT.TKIdent ident)] scp = (M.singleton ident n,) <$> addEnumerator ty cur n scp
        takeEnumFiels' !n ty (cur@(_, HT.TKIdent ident):(_, HT.TKReserved ","):xs) scp = (>>=) (takeEnumFiels' (succ n) ty xs scp) $ \(m, scp') ->
            (M.insert ident n m,) <$> addEnumerator ty cur n scp'
        takeEnumFiels' _ ty (cur@(_, HT.TKIdent ident):(_, HT.TKReserved "="):xs) scp = case constantExp xs scp of
            Left (Just err) -> Left err
            Left Nothing -> Left ("The enumerator value for '" <> tshow (snd cur) <> "' is not an integer constant", cur)
            Right ((_, HT.TKReserved ","):ds, val) -> (>>=) (takeEnumFiels' (succ val) ty ds scp) $ \(m, scp') ->
                (M.insert ident val m,) <$> addEnumerator ty cur val scp'
            Right (ds, val) -> (>>=) (takeEnumFiels' (succ val) ty ds scp) $ \(m, scp') ->
                (M.insert ident val m,) <$> addEnumerator ty cur val scp'
        takeEnumFiels' _ _ ds _ = let lst = if null ds then HT.emptyToken else last ds in
            Left ("expected enum identifier_opt { enumerator-list } or enum identifier_opt { enumerator-list , }", lst)

{-# INLINE takeCtorPtr #-}
takeCtorPtr :: Integral i => [HT.TokenLC i] -> (CT.StorageClass i -> CT.StorageClass i, [HT.TokenLC i])
takeCtorPtr = first (CT.ctorPtr . toNatural) . dropSnd3 . spanLen ((==HT.TKReserved "*") . snd)

-- | It is obtained by parsing the front part of the type from the token string.
-- e.g. @int (*)[4]@ applied to this function yields @int@.
--
-- \[\begin{array}{ccc}
-- \text{pre-type}&=&\text{builtin-type}\ \mid\ \text{struct-decl}\ \mid\ \text{typedef-name}\ \mid\ \text{enum-specifier}\\
-- \text{builtin-type}&=&\text{"void"}\ \mid\ \text{"_Bool"}\ \mid\ \text{"char"}\ \mid\ \text{"short"}\ \mid\ \text{"int"}\ \mid\ \text{"long"}\ \mid\ \text{"long "long"}
-- \end{array}
-- \]
takePreType :: (Integral i, Show i, Read i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (CT.StorageClass i, [HT.TokenLC i], ConstructionData i)
takePreType ((_, HT.TKType ty1):y@(iy, HT.TKType ty2):xs) scp = maybeToRight (T.singleton '\'' <> tshow ty1 <> " " <> tshow ty2 <> "' is invalid.", y) (CT.qualify ty1 ty2) >>= \ty -> -- for a complex type
    takePreType ((iy, HT.TKType ty):xs) scp
takePreType ((_, HT.TKType ty):xs) scp = Right (CT.SCAuto $ CT.toTypeKind $ CT.implicitInt ty, xs, scp) -- for fundamental type
takePreType ((_, HT.TKStruct):cur@(_, HT.TKReserved "{"):xs) scp = maybeToRight (internalCE, cur) (takeBrace "{" "}" (cur:xs)) >>= -- for @struct@ definition
    either (Left . ("expected '}' token to match this '{'",)) (\(field, ds) -> uncurry (,ds,) . first (CT.SCAuto . CT.CTStruct) <$> takeStructFields (tail $ init field) scp)
takePreType ((_, HT.TKStruct):cur1@(_, HT.TKIdent ident):cur2@(_, HT.TKReserved "{"):xs) scp = (>>=) (maybeToRight (internalCE, cur1) (takeBrace "{" "}" (cur2:xs))) $ -- for @struct@ definition with tag
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (addTag (CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteStruct ident) cur1 scp) $ \scp' ->
        (>>=) (takeStructFields (tail $ init field) scp') $ \(mem, scp'') -> let ty = CT.SCAuto $ CT.CTStruct mem in addTag ty cur1 scp'' >>= Right . (ty, ds,)
takePreType ((_, HT.TKStruct):cur1@(_, HT.TKIdent ident):xs) scp = case lookupTag ident scp of -- for variable declaration with @struct@ tag
    Nothing -> let ty = CT.SCAuto $ CT.CTIncomplete $ CT.IncompleteStruct ident in (>>=) (addTag ty cur1 scp) $ \scp' -> Right (ty, xs, scp')
    Just ty -> Right (PST.sttype ty, xs, scp)
takePreType (cur@(_, HT.TKIdent ident):xs) scp = (, xs, scp) . PSD.tdtype <$> maybeToRight (T.singleton '\'' <> tshow (snd cur) <> "' is not a type or also a typedef identifier", cur) (lookupTypedef ident scp) -- for declaration variable with @typedef@
takePreType ((_, HT.TKEnum):cur@(_, HT.TKReserved "{"):xs) scp = (>>=) (maybeToRight (internalCE, cur) (takeBrace "{" "}" (cur:xs))) $ -- for @enum@
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> uncurry (,ds,) . first (CT.SCAuto . CT.CTEnum CT.CTInt) <$> takeEnumFiels (CT.SCAuto CT.CTInt) (tail $ init field) scp
takePreType ((_, HT.TKEnum):cur1@(_, HT.TKIdent _):cur2@(_, HT.TKReserved "{"):xs) scp = (>>=) (maybeToRight (internalCE, cur1) (takeBrace "{" "}" (cur2:xs))) $ -- for @enum@ with tag
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeEnumFiels (CT.SCAuto CT.CTInt) (tail $ init field) scp) $ \(mem, scp') -> let ty = CT.SCAuto $ CT.CTEnum CT.CTInt mem in
        addTag ty cur1 scp' >>= Right . (ty, ds,)
takePreType ((_, HT.TKEnum):cur1@(_, HT.TKIdent ident):xs) scp = (, xs, scp) . PST.sttype <$> maybeToRight ("storage size of '" <> ident <> "' isn't known", cur1) (lookupTag ident scp) -- declaration for @enum@
takePreType ((_, HT.TKReserved _):cur@(_, HT.TKReserved _):_) _ = Left ("cannot combine with previous '" <> tshow (snd cur) <> "' declaration specifier", cur)
takePreType ((_, HT.TKReserved "static"):xs) scp = first3 (CT.SCStatic . CT.toTypeKind) <$> takePreType xs scp
takePreType ((_, HT.TKReserved "register"):xs) scp = first3 (CT.SCRegister . CT.toTypeKind) <$> takePreType xs scp
takePreType ((_, HT.TKReserved "auto"):xs) scp = takePreType xs scp
takePreType (x:_) _ = Left ("ISO C forbids declaration with no type", x)
takePreType _ _ = Left ("ISO C forbids declaration with no type", HT.emptyToken)

{-# INLINE declaration #-}
-- | \[
-- \text{declaration} = \text{"*"*}\ \left(\text{"("}\ \text{declaration}\ \text{")"}\ \mid\ \text{ident}\right)\ \text{array-decl-suffix}
-- \]
declaration :: (Integral i, Bits i, Show i, Read i) => CT.StorageClass i -> [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (CT.StorageClass i, Maybe (HT.TokenLC i), [HT.TokenLC i])
declaration ty xs scp = case takeCtorPtr xs of
    (fn, xs'@((_, HT.TKReserved "("):_)) -> dropFst4 <$> declaration' id (fn ty) xs' scp
    (fn, ident@(_, HT.TKIdent _):ds') -> case arrayDeclSuffix (fn ty) ds' scp of
        Nothing -> Right (fn ty, Just ident, ds')
        Just rs -> uncurry (,Just ident,) <$> rs
    (fn, es) -> Right (fn ty, Nothing, es)
    where
        declaration' fn ty' xs' scp' = case takeCtorPtr xs' of
            (ptrf, cur@(_, HT.TKReserved "("):ds') -> (>>=) (declaration' (fn . ptrf) ty' ds' scp') $ \case
                (ptrf', ty'', ident, (_, HT.TKReserved ")"):ds'') -> case arrayDeclSuffix ty'' ds'' scp' of
                    Nothing -> Right (id, ptrf' ty', ident, ds'')
                    Just rs -> uncurry (id,,ident,) . first ptrf' <$> rs
                _ -> Left ("expected ')' token for this '('", cur)
            (ptrf, ident@(_, HT.TKIdent _):ds') -> case arrayDeclSuffix ty' ds' scp' of
                Nothing -> Right (ptrf, ty', Just ident, ds')
                Just rs -> uncurry (ptrf,,Just ident,) <$> rs
            _ -> Left ("expected some identifier", HT.emptyToken)

-- | `takeType` returns a pair of type (including pointer and array type) and the remaining tokens wrapped in
-- `Just` only if the token starts with `HT.TKType`, `HT.TKStruct` or identifier that is declarated by @typedef@.
-- Otherwise `Nothing` is returned.
--
-- \[
-- \text{type}=\text{pre-type}\ \text{declaration}
-- \]
takeType :: (Integral i, Show i, Read i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (CT.StorageClass i, Maybe (HT.TokenLC i), [HT.TokenLC i], ConstructionData i)
takeType tk scp = takePreType tk scp >>= (\(x, y, z) -> uncurry3 (,,, z) <$> declaration x y z)


-- | `absDeclaration` parses abstract type declarations:
--
-- \[
-- \text{abs-declaration} = \text{"*"*}\ \left(\text{"("}\ \text{abs-declaration}\ \text{")"}\right)\text{?}\ \text{array-decl-suffix}
-- \]
absDeclaration :: (Integral i, Bits i, Show i, Read i) => CT.StorageClass i -> [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (CT.StorageClass i, [HT.TokenLC i])
absDeclaration ty xs scp = case takeCtorPtr xs of
    (fn, xs'@((_, HT.TKReserved "("):_)) -> dropFst3 <$> absDeclarator' id (fn ty) xs' scp
    (fn, ds) -> fromMaybe (Right (fn ty, ds)) $ arrayDeclSuffix (fn ty) ds scp
    where
        absDeclarator' fn ty' xs' scp' = case takeCtorPtr xs' of
            (ptrf, cur@(_, HT.TKReserved "("):ds') -> (>>=) (absDeclarator' (fn . ptrf) ty' ds' scp') $ \case
                (ptrf', ty'', (_, HT.TKReserved ")"):ds'') -> maybe (Right (id, ptrf' ty'', ds'')) (fmap (uncurry (id,,) . first ptrf')) $ arrayDeclSuffix ty'' ds'' scp'
                _ -> Left ("expected ')' token for this '('", cur)
            (p, ds) -> Right (p, ty', ds)

-- | `takeTypeName` is used to parse type names used for sizeof etc. Version without `takeType`s identifier.
takeTypeName :: (Integral i, Show i, Read i, Bits i) => [HT.TokenLC i] -> ConstructionData i -> Either (ASTError i) (CT.StorageClass i, [HT.TokenLC i])
takeTypeName tk scp = (>>=) (takePreType tk scp) $ \(x, y, z) -> if CT.isSCStatic x then Left ("storage-class specifier is not allowed", head tk) else absDeclaration x y z -- !

-- | @HT.TKReserved "[", n, HT.TKReserved "]"@ from the beginning of the token sequence.
-- `arrayDeclSuffix` constructs an array type of the given type @t@ based on
-- the token sequence if \(k\leq 1\), wraps it in `Right` and `Just` and returns it with the rest of the token sequence.
-- If the token @HT.TKReserved "["@ exists at the beginning of the token sequence,
-- but the subsequent token sequence is invalid as an array declaration in C programming language,
-- an error mesage and the token at the error location are returned wrapped in
-- `Left` and `Just`. When \(k=0\), `Nothing` is returned.
--
-- \[
-- \text{array-decl-suffix}=\left(\text{"["}\ \text{const-expr?}\ \text{"]"}\ \text{array-decl-suffix}\right)\text{?}
-- \]
arrayDeclSuffix :: forall i. (Integral i, Bits i, Show i, Read i) => CT.StorageClass i -> [HT.TokenLC i] -> ConstructionData i -> Maybe (Either (ASTError i) (CT.StorageClass i, [HT.TokenLC i]))
arrayDeclSuffix t (cur@(_, HT.TKReserved "["):(_, HT.TKReserved "]"):xs) scp = case arrayDeclSuffix t xs scp of
    Nothing -> Just ((,xs) . CT.mapTypeKind (CT.CTIncomplete . CT.IncompleteArray) <$> maybeToRight (errSt t) (incomplete t scp))
    Just rs -> Just . (>>=) rs $ \(t', ds) -> (,ds) . CT.mapTypeKind (uncurry ((.) fromJust . CT.concatCTArray) . first (CT.CTIncomplete . CT.IncompleteArray . CT.removeAllExtents) . dupe) <$>
        maybeToRight (errSt t') (incomplete t' scp)
    where
        errSt t' = ("array type has incomplete element type '" <> tshow t' <> "'", cur)
arrayDeclSuffix t (cur@(_, HT.TKReserved "["):xs) scp = case constantExp xs scp of
    Left (Just err) -> Just $ Left err
    Left Nothing -> Just $ Left $ if null xs then ("The expression is not constant-expression", cur) else ("The expression '" <> tshow (snd $ head xs) <> "' is not constant-expression", head xs)
    Right ((_, HT.TKReserved "]"):ds, val) -> Just $ maybe' (Right (CT.mapTypeKind (CT.CTArray (toNatural val)) t, ds)) (arrayDeclSuffix t ds scp) $
        either Left $ \(t', ds') -> maybe' (errSt t') (CT.concatCTArray (CT.mapTypeKind (CT.CTArray (toNatural val)) t) t') $ \ty -> if CT.isValidIncomplete ty then Right (ty, ds') else errSt t'
    _ -> Just $ Left ("expected storage size after '[' token", cur)
    where
        errSt t' = Left ("array type has incomplete element type '" <> tshow t' <> "'", cur)
arrayDeclSuffix _ _ _ = Nothing

{-# INLINE isTypeName #-}
-- | `isTypeName` returns @True@ if the token is a type name, @False@ otherwise.
isTypeName :: HT.TokenLC i -> ConstructionData i -> Bool
isTypeName (_, HT.TKType _) _              = True
isTypeName (_, HT.TKStruct) _              = True
isTypeName (_, HT.TKEnum) _                = True
isTypeName (_, HT.TKReserved "static") _   = True
isTypeName (_, HT.TKReserved "auto") _     = True
isTypeName (_, HT.TKReserved "register") _ = True
isTypeName (_, HT.TKIdent ident) scp       = isJust $ lookupTypedef ident scp
isTypeName _ _                             = False

-- | The `Just` represents an error during construction of the syntax tree, and the `Nothing` represents no valid constant expression.
type ConstantResult i = Maybe (ASTError i)

-- | `constantExp` evaluates to a constant expression from token list.
constantExp :: forall i. (Bits i, Integral i, Show i, Read i) => [HT.TokenLC i] -> ConstructionData i -> Either (ConstantResult i) ([HT.TokenLC i], i)
constantExp tk scp = flip (either (Left . Just)) (conditional tk ATEmpty scp) $ \(ds, at, _) ->
    maybe (Left Nothing) (Right . (ds, )) $ evalConstantExp at
    where
        evalConstantExp :: ATree i -> Maybe i
        evalConstantExp (ATNode k _ lhs rhs) = let fromBool = fromIntegral . fromEnum :: Bool -> i in case k of
            ATAdd -> binop (+)
            ATSub -> binop (-)
            ATMul -> binop (*)
            ATDiv -> binop div
            ATAnd -> binop (.&.)
            ATXor -> binop xor
            ATOr -> binop (.|.)
            ATShl -> binop (flip (.) fromIntegral . shiftL)
            ATShr -> binop (flip (.) fromIntegral . shiftR)
            ATEQ -> binop ((.) fromBool . (==))
            ATNEQ -> binop ((.) fromBool . (/=))
            ATLT -> binop ((.) fromBool . (<))
            ATGT -> binop ((.) fromBool . (>))
            ATLEQ -> binop ((.) fromBool . (<=))
            ATGEQ -> binop ((.) fromBool . (>=))
            ATConditional cn th el -> evalConstantExp cn >>= bool (evalConstantExp el) (evalConstantExp th) . castBool
            ATComma -> evalConstantExp rhs
            ATNot ->  fromIntegral . fromEnum . not . castBool <$> evalConstantExp lhs
            ATBitNot -> complement <$> evalConstantExp lhs
            ATLAnd -> binop ((.) fromBool . flip (.) castBool . (&&) . castBool)
            ATLOr -> binop ((.) fromBool . flip (.) castBool . (||) . castBool)
            ATNum v -> Just v
            _ -> Nothing
            where
                binop f = (>>=) (evalConstantExp lhs) $ \lhs' -> fromIntegral . f lhs' <$> evalConstantExp rhs
                castBool x | x == 0 = False | otherwise = True
        evalConstantExp ATEmpty = Nothing

