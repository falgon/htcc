{-|
Module      : Htcc.Asm.Generate
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The executable module for compilation
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Generate (
    InputCCode,
    casm
) where

import Control.Monad (unless)
import Data.Foldable (toList)
import Data.Tuple.Extra (uncurry3)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as S
import System.Exit (exitFailure)

import Htcc.Parser (parse, ATree (..))
import Htcc.Parser.AST.Scope.Var (GVar (..), Literal (..))
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

import Htcc.Asm.Generate.Core
import Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text as IT    
import qualified Htcc.Asm.Intrinsic.Structure.Internal as SI

import qualified Htcc.Tokenizer as HT
import Htcc.Utils (fst4, dropFst4, putStrErr, putStrLnErr, tshow, toInts)

-- | input string, C source code
type InputCCode = T.Text

data MessageType = ErrorMessage | WarningMessage deriving (Eq, Ord, Enum, Bounded)

instance Show MessageType where
    show ErrorMessage = "error"
    show WarningMessage = "warning"

{-# INLINE repSpace #-}
repSpace :: Integral i => i -> IO ()
repSpace = flip (>>) (putStrErr (T.singleton '^')) . mapM_ (putStrErr . T.pack . flip replicate ' ' . pred) . toInts

{-# INLINE format #-}
format :: T.Text -> Int -> InputCCode -> IO ()
format errMesPre e xs = do
    putStrErr $ errMesPre <> " | "
    putStrLnErr (T.lines xs !! max 0 (fromIntegral e))
    putStrErr $ T.replicate (T.length errMesPre) " " <> " | "

parsedMessage :: (Integral i, Show i) => MessageType -> InputCCode -> ASTError i -> IO ()
parsedMessage mest xs (s, (i, etk)) = do
    putStrLnErr (tshow i <> ": " <> tshow mest <> ": " <> s)
    format (T.replicate 4 " " <> tshow (HT.tkLn i)) (pred $ fromIntegral $ HT.tkLn i) xs
    repSpace (HT.tkCn i) >> putStrLnErr (T.replicate (pred $ HT.length etk) "~")

parsedErrExit :: (Integral i, Show i) => InputCCode -> ASTError i -> IO ()
parsedErrExit = (.) (>> exitFailure) . parsedMessage ErrorMessage

parsedWarn :: (Integral i, Show i) => InputCCode -> S.Seq (ASTError i) -> IO ()
parsedWarn xs warns = mapM_ (parsedMessage WarningMessage xs) (toList warns)

casm' :: (Integral i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i, Show e) => [ATree i] -> M.Map T.Text (GVar i) -> [Literal i] -> SI.Asm SI.AsmCodeCtx e ()
casm' atl gvars lits = dataSection gvars lits >> textSection atl

-- | generate full assembly code from C language program
casm :: Bool -> InputCCode -> IO ()
casm supWarns ccode = flip (either (tokenizeErrExit ccode)) (f ccode) $ \x ->
    flip (either $ parsedErrExit ccode) (parse x) $ \res -> do
        unless supWarns (parsedWarn ccode $ fst4 res)
        SI.runAsm $ uncurry3 casm' $ dropFst4 res
    where
        f = HT.tokenize :: T.Text -> Either (HT.TokenLCNums Integer, T.Text) [HT.TokenLC Integer]
