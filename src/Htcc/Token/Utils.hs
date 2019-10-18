{-|
Module      : Htcc.Token.Utils
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections, BangPatterns #-}
module Htcc.Token.Utils (
    -- * Utilities of the token
    takeBrace,
    takeExps,
    makeTypes,
    arrayDeclSuffix
) where

import Prelude hiding (toInteger)
import Numeric.Natural
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Tuple.Extra (first, second, dupe)
import Data.Maybe (fromJust)

import qualified Htcc.CRules as CR
import Htcc.Utils (lastInit, spanLen, dropSnd, tshow, toNatural, toInteger)
import Htcc.Parse.Utils (internalCE)
import qualified Htcc.Parse.Scope as PS
import qualified Htcc.Parse.Struct as PST
import qualified Htcc.Parse.Typedef as PSD
import Htcc.Token.Core

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
takeExps ((_, TKIdent _):(_, TKReserved "("):xs) = flip (maybe Nothing) (lastInit ((==TKReserved ")") . snd) xs) $ fmap (filter (not . null)) . f
    where
        f [] = Just []
        f args = maybe Nothing (\(ex, ds) -> (ex:) <$> f ds) $ readFn args
takeExps _ = Nothing

takeFields :: (Integral i, Show i, Read i) => [TokenLC i] -> PS.Scoped i -> Natural -> Either (T.Text, TokenLC i) (M.Map T.Text CR.StructMember, PS.Scoped i)
takeFields [] scp' _ = Right (M.empty, scp')
takeFields fs scp' !n = (>>=) (makeTypes fs scp') $ \(ty, ds', scp'') -> if null ds' then 
    Left ("expected member name or ';' after declaration specifiers", if null fs then (TokenLCNums 0 0, TKEmpty) else head fs) else case head ds' of
        (_, TKIdent ident) -> case arrayDeclSuffix ty (tail ds') of
            Nothing 
                | not (null $ tail ds') && snd (ds' !! 1) == TKReserved ";" -> let ofs = toNatural $ CR.alignas (toInteger n) $ toInteger $ CR.alignof ty in
                    first (M.insert ident (CR.StructMember ty ofs)) <$> takeFields (drop 2 ds') scp'' (ofs + fromIntegral (CR.sizeof ty))
                | otherwise -> Left ("expected ';' at member variable declaration", head ds')
            Just (Left er) -> Left er
            Just (Right (ty', ds''))
                | snd (head ds'') == TKReserved ";" -> let ofs = toNatural $ CR.alignas (toInteger n) $ toInteger $ CR.alignof ty' in
                    first (M.insert ident (CR.StructMember ty' ofs)) <$> takeFields (tail ds'') scp'' (ofs + fromIntegral (CR.sizeof ty'))
                | otherwise -> Left ("expected ';' at member variable declaration", head ds'')
        _ -> Left ("expected member name or ';' after declaration specifiers", head ds')


{-# INLINE takePtr #-}
takePtr :: Eq i => CR.TypeKind -> [TokenLC i] -> (CR.TypeKind, [TokenLC i])
takePtr ty xs = first (flip CR.makePtr ty . toNatural) $ dropSnd $ spanLen ((==TKReserved "*") . snd) xs

-- | `makeTypes` returns a pair of type (including pointer type) and the remaining tokens wrapped in 
-- `Just` only if the token starts with `TKType` or `TKStruct`.
-- Otherwise `Nothing` is returned.
makeTypes :: (Integral i, Show i, Read i) => [TokenLC i] -> PS.Scoped i -> Either (T.Text, TokenLC i) (CR.TypeKind, [TokenLC i], PS.Scoped i)
makeTypes ((_, TKType tktype):xs) scp = Right $ uncurry (,,scp) $ takePtr tktype xs -- for a variable or a function definition
makeTypes ((_, TKStruct):cur@(_, TKReserved "{"):xs) scp = flip (maybe (Left (internalCE, cur))) (takeBrace "{" "}" (cur:xs)) $ -- for a struct definition
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> uncurry id . second (flip takePtr ds . CR.CTStruct . fst) . first (uncurry . (\x -> (,,snd x))) . dupe <$> takeFields (tail $ init field) scp 0
makeTypes ((_, TKStruct):cur1@(_, TKIdent _):cur2@(_, TKReserved "{"):xs) scp = flip (maybe (Left (internalCE, cur1))) (takeBrace "{" "}" (cur2:xs)) $ -- for a struct definition with its tag
    either (Left . ("expected '}' token to match this '{'",)) $ \(field, ds) -> (>>=) (takeFields (tail $ init field) scp 0) $ \(mem, scp') -> let ty = CR.CTStruct mem in {- (ty, ds,)-} uncurry (,,) (takePtr ty ds) <$> PS.addStructTag ty cur1 scp'
makeTypes ((_, TKStruct):cur1@(_, TKIdent ident):xs) scp = flip (maybe (Left ("storage size of '" <> ident <> "' isn't known", cur1))) (PS.lookupStructTag ident scp) $ \stg -> Right $ uncurry (,,scp) $ takePtr (PST.sttype stg) xs  -- for a variable or a function definition with tag of the struct
makeTypes (cur@(_, TKIdent ident):xs) scp = flip (maybe (Left (tshow (snd cur) <> "is not a type or also a typedef identifier", cur))) (PS.lookupTypedef ident scp) $ \ty -> Right $ uncurry (,,scp) $ takePtr (PSD.tdtype ty) xs -- for a variable or a function definition with type defined by typedef
makeTypes (x:_) _ = Left ("ISO C forbids declaration with no type", x)
makeTypes _ _ = Left ("ISO C forbids declaration with no type", (TokenLCNums 0 0, TKEmpty))

-- | For a number \(n\in\mathbb{R}\), let \(k\) be the number of consecutive occurrences of
-- @TKReserved "[", n, TKReserved "]"@ from the beginning of the token sequence.
-- `arrayDeclSuffix` constructs an array type of the given type @t@ based on 
-- the token sequence if \(k\leq 1\), wraps it in `Right` and `Just` and returns it with the rest of the token sequence.
-- If the token @TKReserved "["@ exists at the beginning of the token sequence, 
-- but the subsequent token sequence is invalid as an array declaration in C programming language,
-- an error mesage and the token at the error location are returned wrapped in
-- `Left` and `Just`. When \(k=0\), `Nothing` is returned.
arrayDeclSuffix :: forall i. (Integral i, Show i) => CR.TypeKind -> [TokenLC i] -> Maybe (Either (T.Text, TokenLC i) (CR.TypeKind, [TokenLC i]))
arrayDeclSuffix t ((_, TKReserved "["):(_, TKNum n):(_, TKReserved "]"):xs) = flip (maybe (Just $ Right (CR.CTArray (toNatural n) t, xs))) (arrayDeclSuffix t xs) $
    Just . fmap (first $ fromJust . CR.concatCTArray (CR.CTArray (toNatural n) t))
arrayDeclSuffix _ ((_, TKReserved "["):cur@(_, TKNum n):xs) = let mes = "expected ']' " in 
    Just $ Left $ if null xs then (mes <> "after '" <> tshow n <> "' token", cur) else (mes <> "before '" <> tshow (head xs) <> "' token", head xs)
arrayDeclSuffix _ (cur@(_, TKReserved "["):_) = Just $ Left ("expected storage size after '[' token", cur)
arrayDeclSuffix _ _ = Nothing
