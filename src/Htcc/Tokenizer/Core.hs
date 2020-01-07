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
    tokenize'
) where

import Data.Char (isDigit, digitToInt, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Extra (first)
import Data.List (find)

import qualified Htcc.CRules as CR
import qualified Htcc.CRules.Preprocessor.Punctuators as CP
import Htcc.Tokenizer.Token
import Htcc.Utils (spanLenT, subTextIndex, dropSnd3, isStrictSpace, lor, maybe', tshow)
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

{-# INLINE isNewLine #-}
isNewLine :: Char -> Bool
isNewLine = lor [(=='\n'), (=='\r')]

-- | The core function of `Htcc.Tokenizer.tokenize`
tokenize' :: (Integral i, Read i, Show i) => TokenLCNums i -> T.Text -> Either (ASTError i) [TokenLC i] --(TokenLCNums i, T.Text) [TokenLC i]
tokenize' n xs = f n $ first fromIntegral $ dropSnd3 $ spanLenT isStrictSpace xs
    where
        f n' (rssize, xxs) = case T.uncons xxs of
            Just (x, xs')
                -- for new line
                | isNewLine x -> tokenize' (TokenLCNums (succ $ tkLn n') 1) xs'
                -- for macro
                | x == CP.bgMacro -> let (_, mc, ds) = spanLenT (not . isNewLine) xs' in
                    if T.null mc then tokenize' n' ds else let (_, kmc, ds') = spanLenT CR.isValidChar mc; cur = n' { tkCn = rssize + tkCn n' } in 
                        maybe' (Left ("invalid macro in program", (cur, TKReserved mc))) (find ((==kmc) . tshow) CP.macros) $ \m -> ((cur, TKMacro m ds'):) <$> tokenize' cur ds
                -- for line comment
                | not (T.null xs') && x == '/' && T.head xs' == '/' -> tokenize' n' $ T.dropWhile (not . isNewLine) (T.tail xs') 
                -- for block comment
                | not (T.null xs') && x == '/' && T.head xs' == '*' -> let xs'' = T.tail xs'; cur = n' { tkCn = rssize + tkCn n' } in
                    maybe' (Left ("*/", (cur, TKReserved "/"))) (subTextIndex "*/" xs'') $ \ind -> 
                        let comment = T.take (ind + 3) xs''
                            next = TokenLCNums (tkLn cur + fromIntegral (T.length $ T.filter (lor [(=='\n'), (=='\r')]) comment)) 
                                (tkCn cur + fromIntegral (T.length $ T.filter (not . isNewLine) comment) + 2) in tokenize' next $ T.drop (ind + 3) xs''
                -- for numbers
                | isDigit x -> let cur = n' { tkCn = rssize + tkCn n' } in
                    maybe' (Left ("invalid number in program", (cur, TKNum $ fromIntegral $ digitToInt x))) (spanIntLit xxs) $ \(len, rn, ds) -> ((cur, rn):) <$> tokenize' (cur { tkCn = tkCn cur + fromIntegral len }) ds
                -- for a string literal
                | x == '\"' -> let cur = n' { tkCn = rssize + tkCn n' } in maybe' (Left ("invalid char literal in program", (cur, TKReserved "\""))) (spanStrLiteral xs') $ \(lit, ds) ->
                    let next = n' { tkCn = tkCn cur + 2 + fromIntegral (T.length lit) } in 
                            ((cur, TKString (T.encodeUtf8 $ T.append lit "\0")):) <$> tokenize' next ds
                -- for a @char@ literal
                | x == '\'' -> let cur = n' { tkCn = rssize + tkCn n' } in maybe' (Left ("invalid char literal in program", (cur, TKReserved "\'"))) (spanCharLiteral xs') $ \(lit, ds) -> 
                    if T.length lit /= 1 then Left ("invalid char literal in program", (cur, TKReserved "\'")) else ((cur, TKNum (fromIntegral $ ord $ T.head lit)):) <$> tokenize' (cur { tkCn = 3 + tkCn cur }) ds
                -- for operators (three characters)
                | T.length xxs > 2 && T.take 3 xxs `elem` CR.strOps3 -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = 3 + tkCn cur }; op = T.take 3 xxs in 
                    ((cur, TKReserved op):) <$> tokenize' next (T.drop 2 xs')
                -- for operators (two characters)
                | not (T.null xs') && T.take 2 xxs `elem` CR.strOps2 -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = 2 + tkCn cur }; op = T.take 2 xxs in 
                    ((cur, TKReserved op):) <$> tokenize' next (T.tail xs')
                -- for operators (one character)
                | x `elem` CR.charOps -> let cur = n' { tkCn = rssize + tkCn n' }; next = n' { tkCn = succ (tkCn cur) } in ((cur, TKReserved (T.singleton x)):) <$> tokenize' next xs' 
                -- other string...
                | otherwise -> let (len, tk, ds) = spanLenT CR.isValidChar xxs; cur = n' { tkCn = tkCn n' + rssize } in
                    if len == 0 then Left ("stray token in program", (cur, TKReserved $ T.takeWhile (not . CR.isValidChar) ds)) else let next = n' { tkCn = tkCn cur + fromIntegral len } in
                        maybe (((cur, TKIdent tk):) <$> tokenize' next ds) (\tkn -> ((cur, tkn):) <$> tokenize' next ds) $ lookupKeyword tk
            _ -> Right []
