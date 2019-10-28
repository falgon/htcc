{-|
Module      : Htcc.Tokenizer.Utils
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections, BangPatterns, LambdaCase #-}
module Htcc.Tokenizer.Utils (
    -- * Utilities of the token
    takeBrace,
    takeExps,
    takeType,
    takeTypeName,
    emptyToken
) where

import Prelude hiding (toInteger)
import Data.Bits (Bits (..))
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Tuple.Extra (first, uncurry3)
import Data.Maybe (fromJust, fromMaybe)

import qualified Htcc.CRules as CR
import Htcc.Utils (dropFst4, dropFst3, dropThd3, lastInit, spanLen, dropSnd3, tshow, toNatural, toInteger, maybe')
import qualified Htcc.Parser.AST.Scope.Struct as PST
import qualified Htcc.Parser.AST.Scope.Typedef as PSD
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)
import Htcc.Parser.ConstructionData
import Htcc.Parser.Utils (internalCE)
import Htcc.Tokenizer.Token

-- | Extract the partial token enclosed in parentheses from the token sequence. If it is invalid, `takeBrace` returns @(i, Text)@ indicating the error location.
-- Otherwise, `takeBrace` returns a partial token enclosed in parentheses and subsequent tokens.
takeBrace :: forall i. (Integral i, Read i, Show i) => T.Text -> T.Text -> [TokenLC i] -> Maybe (Either (TokenLC i) ([TokenLC i], [TokenLC i]))
takeBrace leftb rightb xxs@((_, TKReserved y):_) 
    | y == leftb = Just $ f 0 0 xxs
    | otherwise = Nothing
    where
        f :: Int -> Int -> [TokenLC i] -> Either (TokenLC i) ([TokenLC i], [TokenLC i])
        f !l !r []
            | l /= r = Left $ head xxs 
            | otherwise = Right ([], [])
        f !l !r (c@(p, TKReserved x):xs') 
            | x == rightb = if l == succ r then Right ([c], xs') else g l (succ r) xs'
            | x == leftb = if succ l == r then Right ([c], xs') else g (succ l) r xs'
            | otherwise = g l r xs'
            where
                g = (.) (fmap (first ((p, TKReserved x):)) .) . f
        f !l !r ((p, x):xs') = first ((:) (p, x)) <$> f l r xs'
takeBrace _ _ _ = Nothing

-- | Get an argument from list of `Token` (e.g: Given the token of @f(g(a, b)), 42@, return the token of @f(g(a, b))@).
readFn :: Eq i => [TokenLC i] -> Maybe ([TokenLC i], [TokenLC i])
readFn = readFn' 0 (0 :: Int)
    where
        readFn' !li !ri (cur@(_, TKReserved ","):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li ri xs
        readFn' !li !ri (cur@(_, TKReserved ")"):xs)
            | li == ri = Just ([], xs)
            | otherwise = first (cur:) <$> readFn' li (succ ri) xs
        readFn' !li !ri (cur@(_, TKReserved "("):xs) = first (cur:) <$> readFn' (succ li) ri xs
        readFn' !li !ri []
            | li == ri = Just ([], [])
            | otherwise = Nothing
        readFn' !li !ri (x:xs) = first (x:) <$> readFn' li ri xs

-- | Get arguments from list of `Token` (e.g: Given the token of @f(f(g(a, b)), 42);@, 
-- return expressions that are the token of "f(g(a, b))" and the token of "42".
takeExps :: Eq i => [TokenLC i] -> Maybe [[TokenLC i]]
takeExps ((_, TKIdent _):(_, TKReserved "("):xs) = maybe' Nothing (lastInit ((==TKReserved ")") . snd) xs) $ fmap (filter (not . null)) . f
    where
        f [] = Just []
        f args = maybe Nothing (\(ex, ds) -> (ex:) <$> f ds) $ readFn args
takeExps _ = Nothing

takeStructFields :: (Integral i, Show i, Read i, Bits i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (M.Map T.Text (CR.StructMember i), ConstructionData i)
takeStructFields tk sc = takeStructFields' tk sc 0
    where
        takeStructFields' [] scp' _ = Right (M.empty, scp')
        takeStructFields' fs scp' !n = (>>=) (takeType fs scp') $ \case
            (ty, Just (_, TKIdent ident), (_, TKReserved ";"):ds, scp'') -> let ofs = toNatural $ CR.alignas (toInteger n) $ toInteger $ CR.alignof ty in
                first (M.insert ident (CR.StructMember ty ofs)) <$> takeStructFields' ds scp'' (ofs + fromIntegral (CR.sizeof ty))
            _ -> Left ("expected member name or ';' after declaration specifiers", if null fs then emptyToken else head fs)

{-
enum-specifier:
    enum identifieropt { enumerator-list }
    enum identifieropt { enumerator-list , }
    enum identifier
enumerator-list:
    enumerator
    enumerator-list , enumerator
enumerator:
    enumeration-constant
    enumeration-constant = constant-expression
 -}
takeEnumFiels :: (Integral i, Show i, Read i, Bits i) => CR.TypeKind i -> [TokenLC i] -> ConstructionData i -> Either (ASTError i) (M.Map T.Text i, ConstructionData i)
takeEnumFiels = takeEnumFiels' 0
    where
        takeEnumFiels' !n ty [cur@(_, TKIdent ident)] scp = (M.singleton ident n,) <$> addEnumerator ty cur n scp
        takeEnumFiels' !n ty (cur@(_, TKIdent ident):(_, TKReserved ","):xs) scp = (>>=) (takeEnumFiels' (succ n) ty xs scp) $ \(m, scp') -> 
            (M.insert ident n m,) <$> addEnumerator ty cur n scp'
        takeEnumFiels' _ ty (cur@(_, TKIdent ident):(_, TKReserved "="):(_, TKNum val):(_, TKReserved ","):xs) scp = (>>=) (takeEnumFiels' (succ val) ty xs scp) $ \(m, scp') ->
            (M.insert ident val m,) <$> addEnumerator ty cur val scp'
        takeEnumFiels' _ ty (cur@(_, TKIdent ident):(_, TKReserved "="):(_, TKNum val):xs) scp = (>>=) (takeEnumFiels' (succ val) ty xs scp) $ \(m, scp') ->
            (M.insert ident val m,) <$> addEnumerator ty cur val scp'
        takeEnumFiels' _ _ ((_, TKIdent _):cur@(_, TKReserved "="):_) _ = Left ("expected number after '" <> tshow (snd cur) <> "' token", cur) -- TODO: enumeration-constant = constant-expression
        takeEnumFiels' _ _ ds _ = let lst = if null ds then emptyToken else last ds in
            Left ("expected enum identifier_opt { enumerator-list } or enum identifier_opt { enumerator-list , }", lst)

{-# INLINE takeCtorPtr #-}
takeCtorPtr :: Integral i => [TokenLC i] -> (CR.TypeKind i -> CR.TypeKind i, [TokenLC i])
takeCtorPtr = first (CR.ctorPtr . toNatural) . dropSnd3 . spanLen ((==TKReserved "*") . snd)

-- | It is obtained by parsing the front part of the type from the token string. 
-- e.g. @int (*)[4]@ applied to this function yields @int@
takePreType :: (Integral i, Show i, Read i, Bits i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (CR.TypeKind i, [TokenLC i], ConstructionData i)
takePreType (x@(_, TKType ty1):y@(_, TKType ty2):xs) scp -- for complex type
    | CR.isQualifier ty1 && CR.isQualifiable ty2 = takePreType (x:xs) scp
    | CR.isQualifier ty2 && CR.isQualifiable ty1 = takePreType (y:xs) scp
    | ty1 == CR.CTLong && ty2 == CR.CTLong = takePreType (x:xs) scp
takePreType ((_, TKType ty):xs) scp = Right (ty, xs, scp) -- for fundamental type
takePreType ((_, TKStruct):cur@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur)) (takeBrace "{" "}" (cur:xs)) $ -- for @struct@
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> uncurry (,ds,) . first CR.CTStruct <$> takeStructFields (tail $ init field) scp
takePreType ((_, TKStruct):cur1@(_, TKIdent _):cur2@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur1)) (takeBrace "{" "}" (cur2:xs)) $ -- for @struct@ with tag
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeStructFields (tail $ init field) scp) $ \(mem, scp') -> let ty = CR.CTStruct mem in
        addTag ty cur1 scp' >>= Right . (ty, ds,) 
takePreType ((_, TKStruct):cur1@(_, TKIdent ident):xs) scp = maybe' (Left ("storage size of '" <> ident <> "' isn't known", cur1)) (lookupTag ident scp) $ Right . (, xs, scp) . PST.sttype -- declaration for @struct@
takePreType (cur@(_, TKIdent ident):xs) scp = maybe' (Left (tshow (snd cur) <> " is not a type or also a typedef identifier", cur)) (lookupTypedef ident scp) $ Right . (, xs, scp) . PSD.tdtype -- declaration for @typedef@
takePreType ((_, TKEnum):cur@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur)) (takeBrace "{" "}" (cur:xs)) $ -- for @enum@
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> uncurry (,ds,) . first (CR.CTEnum CR.CTInt) <$> takeEnumFiels CR.CTInt (tail $ init field) scp
takePreType ((_, TKEnum):cur1@(_, TKIdent _):cur2@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur1)) (takeBrace "{" "}" (cur2:xs)) $ -- for @enum@ with tag
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeEnumFiels CR.CTInt (tail $ init field) scp) $ \(mem, scp') -> let ty = CR.CTEnum CR.CTInt mem in
        addTag ty cur1 scp' >>= Right . (ty, ds,)
takePreType ((_, TKEnum):cur1@(_, TKIdent ident):xs) scp = maybe' (Left ("storage size of '" <> ident <> "' isn't known", cur1)) (lookupTag ident scp) $ Right . (, xs, scp) . PST.sttype -- declaration for @enum@
takePreType (x:_) _ = Left ("ISO C forbids declaration with no type", x)
takePreType _ _ = Left ("ISO C forbids declaration with no type", emptyToken)

{-# INLINE declaration #-}
declaration :: (Integral i, Show i) => CR.TypeKind i -> [TokenLC i] -> Either (ASTError i) (CR.TypeKind i, Maybe (TokenLC i), [TokenLC i])
declaration ty xs = case takeCtorPtr xs of 
    (fn, xs'@((_, TKReserved "("):_)) -> declaration' id (fn ty) xs' >>= uncurry3 (validDecl emptyToken) . dropFst4
    (fn, ident@(_, TKIdent _):ds') -> case arrayDeclSuffix (fn ty) ds' of
        Nothing -> validDecl ident (fn ty) (Just ident) ds'
        Just rs -> rs >>= uncurry (flip (validDecl ident) (Just ident))
    (fn, es) -> validDecl emptyToken (fn ty) Nothing es
    where
        validDecl errtk t ident ds
            | t == CR.CTVoid = Left ("variable or field '" <> tshow (snd errtk) <> "' declared void", errtk) 
            | otherwise = Right (t, ident, ds)
        declaration' fn ty' xs' = case takeCtorPtr xs' of
            (ptrf, cur@(_, TKReserved "("):ds') -> (>>=) (declaration' (fn . ptrf) ty' ds') $ \case
                (ptrf', ty'', ident, (_, TKReserved ")"):ds'') -> case arrayDeclSuffix ty'' ds'' of
                    Nothing -> Right (id, ptrf' ty', ident, ds'')
                    Just rs -> uncurry (id,,ident,) . first ptrf' <$> rs
                _ -> Left ("expected ')' token for this '('", cur)
            (ptrf, ident@(_, TKIdent _):ds') -> case arrayDeclSuffix ty' ds' of
                Nothing -> Right (ptrf, ty', Just ident, ds')
                Just rs -> uncurry (ptrf,,Just ident,) <$> rs 
            _ -> Left ("expected some identifier", emptyToken)


-- | `takeType` returns a pair of type (including pointer and array type) and the remaining tokens wrapped in 
-- `Just` only if the token starts with `TKType`, `TKStruct` or identifier that is declarated by @typedef@.
-- Otherwise `Nothing` is returned.
takeType :: (Integral i, Show i, Read i, Bits i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (CR.TypeKind i, Maybe (TokenLC i), [TokenLC i], ConstructionData i)
takeType tk scp = takePreType tk scp >>= (\(x, y, z) -> uncurry3 (,,, z) <$> declaration x y)

-- `absDeclaration` parses abstract type declarations
absDeclaration :: (Integral i, Show i) => CR.TypeKind i -> [TokenLC i] -> Either (ASTError i) (CR.TypeKind i, [TokenLC i])
absDeclaration ty xs = case takeCtorPtr xs of
    (fn, xs'@((_, TKReserved "("):_)) -> dropFst3 <$> absDeclarator' id (fn ty) xs'
    (fn, ds) -> fromMaybe (Right (fn ty, ds)) $ arrayDeclSuffix (fn ty) ds
    where
        absDeclarator' fn ty' xs' = case takeCtorPtr xs' of
            (ptrf, cur@(_, TKReserved "("):ds') -> (>>=) (absDeclarator' (fn . ptrf) ty' ds') $ \case
                (ptrf', ty'', (_, TKReserved ")"):ds'') -> maybe (Right (id, ptrf' ty'', ds'')) (fmap (uncurry (id,,) . first ptrf')) $ arrayDeclSuffix ty'' ds''
                _ -> Left ("expected ')' token for this '('", cur)
            (p, ds) -> Right (p, ty', ds)

-- | `takeTypeName` is used to parse type names used for sizeof etc. Version without `takeType`s identifier.
takeTypeName :: (Integral i, Show i, Read i, Bits i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (CR.TypeKind i, [TokenLC i])
takeTypeName tk scp = takePreType tk scp >>= (uncurry absDeclaration . dropThd3)

-- For a number \(n\in\mathbb{R}\), let \(k\) be the number of consecutive occurrences of
-- @TKReserved "[", n, TKReserved "]"@ from the beginning of the token sequence.
-- `arrayDeclSuffix` constructs an array type of the given type @t@ based on 
-- the token sequence if \(k\leq 1\), wraps it in `Right` and `Just` and returns it with the rest of the token sequence.
-- If the token @TKReserved "["@ exists at the beginning of the token sequence, 
-- but the subsequent token sequence is invalid as an array declaration in C programming language,
-- an error mesage and the token at the error location are returned wrapped in
-- `Left` and `Just`. When \(k=0\), `Nothing` is returned.
arrayDeclSuffix :: forall i. (Integral i, Show i) => CR.TypeKind i -> [TokenLC i] -> Maybe (Either (ASTError i) (CR.TypeKind i, [TokenLC i]))
arrayDeclSuffix t ((_, TKReserved "["):(_, TKNum n):(_, TKReserved "]"):xs) = maybe' (Just $ Right (CR.CTArray (toNatural n) t, xs)) (arrayDeclSuffix t xs) $
    Just . fmap (first $ fromJust . CR.concatCTArray (CR.CTArray (toNatural n) t))
arrayDeclSuffix _ ((_, TKReserved "["):cur@(_, TKNum n):xs) = let mes = "expected ']' " in 
    Just $ Left $ if null xs then (mes <> "after '" <> tshow n <> "' token", cur) else (mes <> "before '" <> tshow (head xs) <> "' token", head xs)
arrayDeclSuffix _ (cur@(_, TKReserved "["):_) = Just $ Left ("expected storage size after '[' token", cur)
arrayDeclSuffix _ _ = Nothing

{-# INLINE emptyToken #-}
-- | An empty token that is used when there are no remaining tokens when an error occurs,
-- or when it cannot be referenced
emptyToken :: Num i => TokenLC i
emptyToken = (TokenLCNums 0 0, TKEmpty)
