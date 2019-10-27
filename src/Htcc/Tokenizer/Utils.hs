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
    emptyToken
) where

import Prelude hiding (toInteger)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Tuple.Extra (first, uncurry3)
import Data.Maybe (fromJust)

import qualified Htcc.CRules as CR
import Htcc.Utils (dropFst4, lastInit, spanLen, dropSnd3, tshow, toNatural, toInteger, maybe')
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

takeFields :: (Integral i, Show i, Read i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (M.Map T.Text CR.StructMember, ConstructionData i)
takeFields tk sc = takeFields' tk sc 0
    where
        takeFields' [] scp' _ = Right (M.empty, scp')
        takeFields' fs scp' !n = (>>=) (takeType fs scp') $ \case
            (ty, Just (_, TKIdent ident), (_, TKReserved ";"):ds, scp'') -> let ofs = toNatural $ CR.alignas (toInteger n) $ toInteger $ CR.alignof ty in
                first (M.insert ident (CR.StructMember ty ofs)) <$> takeFields' ds scp'' (ofs + fromIntegral (CR.sizeof ty))
            _ -> Left ("expected member name or ';' after declaration specifiers", if null fs then emptyToken else head fs)

{-# INLINE takeCtorPtr #-}
takeCtorPtr :: Integral i => [TokenLC i] -> (CR.TypeKind -> CR.TypeKind, [TokenLC i])
takeCtorPtr = first (CR.ctorPtr . toNatural) . dropSnd3 . spanLen ((==TKReserved "*") . snd)

{-# INLINE declaration #-}
declaration :: (Integral i, Show i) => CR.TypeKind -> [TokenLC i] -> Either (ASTError i) (CR.TypeKind, Maybe (TokenLC i), [TokenLC i])
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
                _ -> Left ("expected ')' token", cur)
            (ptrf, ident@(_, TKIdent _):ds') -> case arrayDeclSuffix ty' ds' of
                Nothing -> Right (ptrf, ty', Just ident, ds')
                Just rs -> uncurry (ptrf,,Just ident,) <$> rs 
            _ -> Left ("expected some identifier", emptyToken)


-- | `takeType` returns a pair of type (including pointer and array type) and the remaining tokens wrapped in 
-- `Just` only if the token starts with `TKType`, `TKStruct` or identifier that is declarated by @typedef@.
-- Otherwise `Nothing` is returned.
takeType :: (Integral i, Show i, Read i) => [TokenLC i] -> ConstructionData i -> Either (ASTError i) (CR.TypeKind, Maybe (TokenLC i), [TokenLC i], ConstructionData i)
takeType (x@(_, TKType ty1):y@(_, TKType ty2):xs) scp
    | CR.isQualifier ty1 && CR.isQualifiable ty2 = takeType (x:xs) scp
    | CR.isQualifier ty2 && CR.isQualifiable ty1 = takeType (y:xs) scp
    | ty1 == CR.CTLong && ty2 == CR.CTLong = takeType (x:xs) scp
takeType ((_, TKType ty):xs) scp = uncurry3 (,,,scp) <$> declaration ty xs
takeType ((_, TKStruct):cur@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur)) (takeBrace "{" "}" (cur:xs)) $
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeFields (tail $ init field) scp) $ \(mem, scp') -> uncurry3 (,,,scp') <$> declaration (CR.CTStruct mem) ds
takeType ((_, TKStruct):cur1@(_, TKIdent _):cur2@(_, TKReserved "{"):xs) scp = maybe' (Left (internalCE, cur1)) (takeBrace "{" "}" (cur2:xs)) $
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeFields (tail $ init field) scp) $ \(mem, scp') -> let ty = CR.CTStruct mem in
        (>>=) (addStructTag ty cur1 scp') $ \scp'' -> uncurry3 (,,,scp'') <$> declaration ty ds
takeType ((_, TKStruct):cur1@(_, TKIdent ident):xs) scp = maybe' (Left ("storage size of '" <> ident <> "' isn't known", cur1)) (lookupStructTag ident scp) $ \stg ->
    uncurry3 (,,,scp) <$> declaration (PST.sttype stg) xs
takeType (cur@(_, TKIdent ident):xs) scp = maybe' (Left (tshow (snd cur) <> "is not a type or also a typedef identifier", cur)) (lookupTypedef ident scp) $ \stg ->
    uncurry3 (,,,scp) <$> declaration (PSD.tdtype stg) xs
takeType (x:_) _ = Left ("ISO C forbids declaration with no type", x)
takeType _ _ = Left ("ISO C forbids declaration with no type", emptyToken)


-- For a number \(n\in\mathbb{R}\), let \(k\) be the number of consecutive occurrences of
-- @TKReserved "[", n, TKReserved "]"@ from the beginning of the token sequence.
-- `arrayDeclSuffix` constructs an array type of the given type @t@ based on 
-- the token sequence if \(k\leq 1\), wraps it in `Right` and `Just` and returns it with the rest of the token sequence.
-- If the token @TKReserved "["@ exists at the beginning of the token sequence, 
-- but the subsequent token sequence is invalid as an array declaration in C programming language,
-- an error mesage and the token at the error location are returned wrapped in
-- `Left` and `Just`. When \(k=0\), `Nothing` is returned.
arrayDeclSuffix :: forall i. (Integral i, Show i) => CR.TypeKind -> [TokenLC i] -> Maybe (Either (ASTError i) (CR.TypeKind, [TokenLC i]))
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
