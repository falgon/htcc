{-|
Module      : Htcc.Tokenizer.Core
Description : Tokenizer
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The tokenizer
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase, TupleSections, MultiWayIf #-}
module Htcc.Tokenizer.Core (
    -- * Tokenizer
    tokenize'
) where

import Control.Applicative (Alternative (..))
import Control.Conditional (ifM)
import Control.Monad.Extra (firstJustM)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Trans (lift)
import Control.Monad.State 

import Data.Bool (bool)
import Data.Char (isDigit, digitToInt, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Extra (first, second)
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromJust)

import Numeric (showHex, readHex)

import qualified Htcc.CRules as CR
import qualified Htcc.CRules.Preprocessor.Punctuators as CP
import Htcc.Tokenizer.Token
import Htcc.Utils (spanLenT, subTextIndex, isStrictSpace, lor, maybe', tshow)
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

{-# INLINE isNewLine #-}
isNewLine :: Char -> Bool
isNewLine = lor [(=='\n'), (=='\r')]

{-# INLINE headToken #-}
headToken :: (Char -> Token i) ->  T.Text -> Token i
headToken f txt
    | T.null txt = TKEmpty
    | otherwise = f $ T.head txt

type Tokenizer i a = StateT (TokenLCNums i, T.Text) (Either (ASTError i)) a 

{-# INLINE itemP #-}
itemP :: Tokenizer i (Maybe Char)
itemP = do
    x <- gets snd
    return $ if T.null x then Nothing else Just $ T.head x

{-# INLINE itemC #-}
itemC :: (Enum i, Num i) => Tokenizer i (Maybe Char)
itemC = do
    res <- itemP
    maybe' (return Nothing) res $ \res' ->
        res <$ (get >>= put . first (\lc -> if isNewLine res' then lc { tkCn = 1, tkLn = succ $ tkLn lc } else lc { tkCn = succ $ tkCn lc }) . second T.tail)

{-# INLINE takeP #-}
takeP :: Int -> Tokenizer i T.Text
takeP n = gets (T.take n . snd) 

{-# INLINE itemsP #-}
itemsP :: Int -> Tokenizer i (Maybe T.Text)
itemsP n = do
    tp <- takeP n
    return $ if T.length tp == n then Just tp else Nothing

{-# INLINE curLC #-}
curLC :: Tokenizer i (TokenLCNums i)
curLC = gets fst 

char :: (Enum i, Num i) => (Char -> Bool) -> Tokenizer i (Maybe Char)
char f = do
    x <- itemP
    maybe' (return Nothing) x $ bool (return Nothing) (x <$ void itemC) . f

string :: (Enum i, Num i) => (Char -> Bool) -> Tokenizer i T.Text
string = fmap T.pack . unfoldM . char

isPrefixOf :: Enum i => T.Text -> Tokenizer i Bool
isPrefixOf txt = gets (T.isPrefixOf txt . snd) 

consumeSpace :: (Enum i, Num i) => Tokenizer i Bool
consumeSpace = not . T.null <$> string isStrictSpace

consumeNewLine :: (Enum i, Num i) => Tokenizer i Bool
consumeNewLine = not . T.null <$> string isNewLine

consumeComment :: (Enum i, Num i) => Tokenizer i Bool
consumeComment = ifM (isPrefixOf "//") (True <$ line) $ flip (ifM (isPrefixOf "/*")) (return False) $ do
    cur <- curLC
    replicateM_ 2 itemC
    ind <- gets (subTextIndex "*/" . snd)
    True <$ maybe' (lift $ Left ("*/", (cur, TKReserved "/"))) ind (flip replicateM_ itemC . (+2))

line :: (Enum i, Num i) => Tokenizer i T.Text
line = string (not . isNewLine)

macro :: (Enum i, Num i) => Tokenizer i (Maybe (Token i))
macro = ifM ((/= Just '#') <$> itemP) (return Nothing) f
    where
        f = (>>=) itemC $ maybe (return Nothing) $ \case
            '#' -> do
                cur <- curLC
                ln <- line
                if T.null ln then return Nothing else let (_, kmc, ds) = spanLenT CR.isValidChar ln in
                    maybe' (lift $ Left ("invalid macro in program", (cur, TKReserved ln))) (find ((==kmc) . tshow) CP.macros) $ \m -> return $ Just $ TKMacro m ds
            _ -> return Nothing

natLit :: (Enum i, Num i, Eq i, Read i) => Tokenizer i (Maybe (Token i))
natLit = do
    lc <- gets fst
    ifM (maybe True (not . isDigit) <$> itemP) (return Nothing) $ do
        txt <- gets snd
        maybe' (lift $ Left ("invalid number in program", (lc, headToken (TKNum . fromIntegral . digitToInt) txt))) (spanIntLit txt) $ \(n, tk, ds) -> 
            Just tk <$ put (lc { tkCn = tkCn lc + fromIntegral n }, ds)

strLit :: (Enum i, Num i) => Tokenizer i (Maybe (Token i))
strLit = do
    lc <- gets fst
    ifM (isNothing <$> char (=='\"')) (return Nothing) $ do
        txt <- gets snd
        maybe' (lift $ Left ("invalid string literal in program", (lc, TKReserved "\""))) (spanStrLiteral txt) $ \(lit, ds) -> 
            -- The meaning of adding 2 is to add the two "characters surrounding the string literal.
            Just (TKString (T.encodeUtf8 $ T.append lit "\0")) <$ put (lc { tkCn = 2 + tkCn lc + fromIntegral (T.length lit) }, ds) 

charLit :: (Enum i, Num i, Eq i) => Tokenizer i (Maybe (Token i))
charLit = do
    lc <- gets fst
    ifM (isNothing <$> char (=='\'')) (return Nothing) $ do
        txt <- gets snd
        maybe' (lift $ Left ("invalid char literal in program", (lc, TKReserved "\'"))) (spanCharLiteral txt) $ \(lit, ds) ->
            -- Adding 3 means to add a single character literal and two @"@
            if  | T.length lit == 1 -> Just (TKNum (fromIntegral $ ord $ T.head lit)) <$ put (lc { tkCn = 3 + tkCn lc }, ds) 
                -- For multi-character constants.
                -- The standard states that this is an implementation definition.
                -- Here it follows the implementation definitions of GCC and Clang.
                | otherwise -> Just (TKNum $ fst $ head $ readHex $ foldr (\x acc -> showHex (ord x) "" <> acc) [] $ T.unpack lit) <$
                    put (lc { tkCn = 2 + fromIntegral (T.length lit) + tkCn lc }, ds)

operators :: (Enum i, Num i) => Tokenizer i (Maybe (Token i))
operators = do
    s3 <- itemsP 3
    if isJust s3 && fromJust s3 `elem` CR.strOps3 then Just (TKReserved $ fromJust s3) <$ replicateM_ 3 itemC else do
        s2 <- itemsP 2
        if isJust s2 && fromJust s2 `elem` CR.strOps2 then Just (TKReserved $ fromJust s2) <$ replicateM_ 2 itemC else do
            s1 <- itemP
            if isJust s1 && fromJust s1 `elem` CR.charOps then Just (TKReserved $ T.singleton $ fromJust s1) <$ itemC else 
                return Nothing

keyWordOrIdent :: (Enum i, Num i, Show i) => Tokenizer i (Maybe (Token i))
keyWordOrIdent = do
    (lc, txt) <- get
    string CR.isValidChar >>= \s -> if T.null s then lift $ Left ("stray token in program", (lc, TKReserved $ T.takeWhile (not . CR.isValidChar) txt)) else
        return $ lookupKeyword s <|> Just (TKIdent s)

-- | The core function of `Htcc.Tokenizer.tokenize`
tokenize' :: forall i. (Enum i, Num i, Eq i, Read i, Show i) => T.Text -> Either (ASTError i) [TokenLC i]
tokenize' = evalStateT runTokenizer' . (TokenLCNums 1 1,)
    where
        next = get >>= lift . evalStateT runTokenizer'
        runTokenizer' = ifM consumeSpace next $ ifM consumeNewLine next $ ifM consumeComment next $ do
            cur <- curLC
            ifM (isNothing <$> itemP) (lift $ Right []) $ 
                firstJustM id [macro, natLit, strLit, charLit, operators, keyWordOrIdent] >>= maybe (lift $ Right []) (\tk -> ((cur, tk):) <$> next)
