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
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Tokenizer.Core (
    -- * Tokenizer
    tokenize
) where

import Data.Char (isDigit, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Tuple.Extra (first)

import qualified Htcc.CRules as CR
import Htcc.Tokenizer.Token
import Htcc.Utils (spanLenT, subTextIndex, first3, dropSnd3, isStrictSpace, lor, land, maybe')

-- | The core function of `tokenize`
tokenize' :: (Integral i, Read i, Show i) => TokenLCNums i -> T.Text -> Either (TokenLCNums i, T.Text) [TokenLC i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd3 $ spanLenT isStrictSpace xs
    where
        f n' (rssize, xxs) = case T.uncons xxs of
            Just (x, xs')
                | lor [(=='\n'), (=='\r')] x -> tokenize' (TokenLCNums (succ $ tkLn n') 1) xs' -- for new line
                | not (T.null xs') && x == '/' && T.head xs' == '/' -> tokenize' n' $ T.dropWhile (land [(/='\n'), (/='\r')]) (T.tail xs') -- for line comment
                | not (T.null xs') && x == '/' && T.head xs' == '*' -> let xs'' = T.tail xs'; cur = n' { tkCn = rssize + tkCn n' } in -- for block comment
                    maybe' (Left (cur, "*/")) (subTextIndex "*/" xs'') $ \ind -> 
                        let comment = T.take (ind + 3) xs''
                            next = TokenLCNums (tkLn cur + fromIntegral (T.length $ T.filter (lor [(=='\n'), (=='\r')]) comment)) 
                                (tkCn cur + fromIntegral (T.length $ T.filter (land [(/='\n'), (=='\r')]) comment) + 2) in tokenize' next $ T.drop (ind + 3) xs''
                | isDigit x -> let (n'', ts, ds) = first3 fromIntegral $ spanLenT isDigit xs'; cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = tkCn cur + n'' }; num = T.cons x ts in -- for numbers
                    flip (either (const $ Left (cur, T.singleton x))) (T.decimal num) $ \(nu, _) -> ((cur, TKNum nu):) <$> tokenize' next ds
                | x == '\"' -> let cur = n' { tkCn = rssize + tkCn n' } in maybe' (Left (cur, "\"")) (spanStrLiteral xs') $ \(lit, ds) -> -- for a string literal
                    let next = n' { tkCn = tkCn cur + 2 + fromIntegral (T.length lit) } in 
                            ((cur, TKString (T.encodeUtf8 $ T.append lit "\0")):) <$> tokenize' next ds
                | x == '\'' -> let cur = n' { tkCn = rssize + tkCn n' } in maybe' (Left (cur, "\'")) (spanCharLiteral xs') $ \(lit, ds) -> -- for a char literal
                    if T.length lit /= 1 then Left (cur, "\'") else ((cur, TKNum (fromIntegral $ ord $ T.head lit)):) <$> tokenize' (cur { tkCn = 3 + tkCn cur }) ds
                | T.length xxs > 2 && T.take 3 xxs `elem` CR.strOps3 -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = 3 + tkCn cur }; op = T.take 3 xxs in -- for operators (three characters)
                    ((cur, TKReserved op):) <$> tokenize' next (T.drop 2 xs')
                | not (T.null xs') && T.take 2 xxs `elem` CR.strOps2 -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = 2 + tkCn cur }; op = T.take 2 xxs in -- for operators (two characters)
                    ((cur, TKReserved op):) <$> tokenize' next (T.tail xs')
                | x `elem` CR.charOps -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = succ (tkCn cur) } in ((cur, TKReserved (T.singleton x)):) <$> tokenize' next xs' -- for operators (one character)
                | otherwise -> let (len, tk, ds) = spanLenT CR.isValidChar xxs; cur = n' { tkCn = tkCn n' + rssize } in
                    if len == 0 then Left (cur, T.takeWhile (not . CR.isValidChar) ds) else let next = n' { tkCn = tkCn cur + fromIntegral len } in
                        maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk
            _ -> Right []

-- | Tokenize the `T.Text`. If an invalid chraracter matches as C language, the part and the character are returned.
-- Otherwise, @[TokenIdx i]@ is returned.
tokenize :: (Integral i, Read i, Show i) => T.Text -> Either (TokenLCNums i, T.Text) [TokenLC i]
tokenize = tokenize' (TokenLCNums 1 1)
