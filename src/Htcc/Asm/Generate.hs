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
    -- * Generator
    casm',
    buildAST,
    execAST
) where

import           Control.Monad                                   (unless, (>=>))
import           Data.Bits                                       (Bits)
import           Data.Foldable                                   (toList)
import qualified Data.Sequence                                   as S
import qualified Data.Text                                       as T
import           System.Exit                                     (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen                    (Doc, blue,
                                                                  bold, char,
                                                                  empty,
                                                                  magenta, red,
                                                                  text, (<+>))

import           Htcc.Parser                                     (ASTResult,
                                                                  ASTs, parse)
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import           Htcc.Parser.ConstructionData.Scope.Var          (GlobalVars,
                                                                  Literals)
import qualified Htcc.Tokenizer                                  as HT

import           Htcc.Asm.Generate.Core
import           Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Structure                    as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text       as IT

import           Htcc.Utils                                      (dropFst4,
                                                                  putDocErr,
                                                                  putDocLnErr,
                                                                  putStrErr,
                                                                  putStrLnErr,
                                                                  toInts, tshow)

-- | input string, C source code
type InputCCode = T.Text

data MessageType = ErrorMessage | WarningMessage
    deriving (Eq, Ord, Enum, Bounded)

instance Show MessageType where
    show ErrorMessage   = "error"
    show WarningMessage = "warning"

{-# INLINE messageColor #-}
messageColor :: MessageType -> Doc -> Doc
messageColor ErrorMessage   = red
messageColor WarningMessage = magenta

{-# INLINE repSpace #-}
repSpace :: Integral i => i -> MessageType -> IO ()
repSpace i mest = do
    mapM_ (putStrErr . T.pack . flip replicate ' ' . pred) $ toInts i
    putDocErr $ messageColor mest $ char '^'

{-# INLINE format #-}
format :: T.Text -> Int -> InputCCode -> IO ()
format errMesPre e xs = do
    putDocErr $ blue (text $ T.unpack errMesPre) <+> blue (char '|') <+> empty
    putStrLnErr (T.lines xs !! max 0 (fromIntegral e))
    putStrErr $ T.replicate (T.length errMesPre) " "
    putDocErr $ empty <+> blue (char '|') <+> empty

parsedMessage :: (Integral i, Show i) => MessageType -> FilePath -> InputCCode -> ASTError i -> IO ()
parsedMessage mest fpath xs (s, (i, etk)) = do
    putDocLnErr $
        bold (text fpath) <> bold (char ':') <>
        bold (text (show i)) <> bold (char ':') <+>
        messageColor mest (text $ show mest) <> messageColor mest (char ':') <+>
        text (T.unpack s)
    format (T.replicate 4 " " <> tshow (HT.tkLn i)) (pred $ fromIntegral $ HT.tkLn i) xs
    repSpace (HT.tkCn i) mest
    putDocLnErr $ messageColor mest (text $ replicate (pred $ HT.length etk) '~')

-- | the function to output error message
parsedErrExit :: (Integral i, Show i) => FilePath -> InputCCode -> ASTError i -> IO ()
parsedErrExit fpath ccode err = parsedMessage ErrorMessage fpath ccode err >> exitFailure

-- | the function to output warning message
parsedWarn :: (Integral i, Show i) => FilePath -> InputCCode -> S.Seq (ASTError i) -> IO ()
parsedWarn fpath xs warns = mapM_ (parsedMessage WarningMessage fpath xs) (toList warns)

-- | Executor that receives information about the constructed AST,
-- global variables, and literals and composes assembly code
casm' :: (Integral e, Show e, Integral i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ASTs i -> GlobalVars i -> Literals i -> SI.Asm SI.AsmCodeCtx e ()
casm' atl gvars lits = dataSection gvars lits >> textSection atl

-- | Build AST from string of C source code
buildAST :: (Integral i, Read i, Show i, Bits i) => InputCCode -> ASTResult i
buildAST = HT.tokenize >=> parse

-- | Print warning or error message if building AST from string of C source code has some problems
execAST :: (Integral i, Read i, Show i, Bits i) => Bool -> FilePath -> InputCCode -> IO (Maybe (ASTs i, GlobalVars i, Literals i))
execAST supWarns fpath ccode = flip (either ((<$) Nothing . parsedErrExit fpath ccode)) (buildAST ccode) $ \xs@(warns, _, _, _) ->
    Just (dropFst4 xs) <$ unless supWarns (parsedWarn fpath ccode warns)
