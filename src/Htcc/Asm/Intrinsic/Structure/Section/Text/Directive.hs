{-|
Module      : Htcc.Asm.Intrinsic.Structure.Section.Text.Directive
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Htcc.Asm.Intrinsic.Structure.Section.Text.Directive (
    -- * Context type
    TextSectionCtx,
    TextLabelCtx,
    TargetLabelCtx,
    -- * Directives
    text,
    global,
    -- * Labels
    fn,
    label,
    begin,
    end,
    eLse,
    cAse,
    break,
    continue,
    gotoLabel,
    ref,
    refBegin,
    refEnd,
    refElse,
    refBreak,
    refHBreak,
    refContinue,
    refHContinue,
    refReturn,
    refGoto,
    -- * Generator
    makeCases
) where

import           Control.Monad                         (forM, unless)
import           Data.IORef                            (IORef, modifyIORef,
                                                        readIORef)
import           Data.Maybe                            (fromJust, isJust)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Prelude                               hiding (break)

import qualified Htcc.Asm.Intrinsic.Structure.Internal as C
import           Htcc.Parser.AST.Core                  (ATKind (..), ATree (..))
import           Htcc.Utils                            (err, tshow)

-- | the type representing the context inside the text section
data TextSectionCtx

-- | the type representing the context inside the label
data TextLabelCtx

-- | the type representing the context inside the instruction that needs to be specified,
-- such as a @jmp@ instruction.
data TargetLabelCtx

-- | @text@ section
text :: C.Asm TextSectionCtx e a -> C.Asm C.AsmCodeCtx e a
text = C.section "text"

-- | @global@ directive
global :: T.Text -> C.Asm TextSectionCtx e ()
global = C.putStrLnWithIndent . T.append ".global "

-- | the label as function definition in text section
fn :: T.Text -> C.Asm TextLabelCtx e a -> C.Asm TextSectionCtx e a
fn fname asm = C.writeCurFn (Just fname) *>
    C.putStrLnWithIndent (fname <> ":") *>
    C.unCtx (C.labeled asm)

-- | the label in text section
label :: (Show i, Show e) => T.Text -> i -> C.Asm TextLabelCtx e ()
label lbl n = C.Asm $ \x -> do
    cf <- readIORef $ C.curFn x
    unless (isJust cf) $ err "stray label"
    T.putStrLn $ ".L." <> lbl <> "." <> fromJust cf <> "." <> tshow n <> ":"

-- | goto label
gotoLabel :: T.Text -> C.Asm TextLabelCtx e ()
gotoLabel ident = C.Asm $ \x -> do
    cf <- readIORef $ C.curFn x
    unless (isJust cf) $ err "stray goto label"
    T.putStrLn $ ".L.label." <> fromJust cf <> "." <> ident <> ":"

-- | begin label
begin :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
begin = label "begin"

-- | end label
end :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
end = label "end"

-- | else label
eLse :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
eLse = label "else"

-- | case label
cAse :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
cAse n = C.Asm $ \x -> do
    cf <- readIORef $ C.curFn x
    unless (isJust cf) $ err "stray case"
    T.putStrLn $ ".L.case." <> fromJust cf <> "." <> tshow n <> ":"

-- | break label
break :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
break = label "break"

-- | continue label
continue :: (Show e, Show i) => i -> C.Asm TextLabelCtx e ()
continue = label "continue"

-- | reference for return label
refReturn :: Show e => C.Asm TargetLabelCtx e ()
refReturn = C.Asm $ \x -> do
    cf <- readIORef (C.curFn x)
    unless (isJust cf) $ err "stray label"
    T.putStrLn $ ".L.return." <> fromJust cf

refCnt :: Show e => (C.AsmInfo a -> IORef (Maybe e)) -> T.Text -> C.Asm ctx a ()
refCnt f mes = C.Asm $ \x -> do
    cf <- readIORef (C.curFn x)
    unless (isJust cf) $ err $ "stray " <> mes
    n <- readIORef (f x)
    unless (isJust n) $ err $ "stray " <> mes
    T.putStrLn $ ".L." <> mes <> "." <> fromJust cf <> "." <> tshow (fromJust n)

-- | reference for break label
refBreak :: (Show e, Show i) => i -> C.Asm TargetLabelCtx e ()
refBreak = ref "break"

-- | reference for break label (applying value by `Htcc.Asm.Intrinsic.Structure.Internal.brkCnt`)
refHBreak :: Show e => C.Asm TargetLabelCtx e ()
refHBreak = refCnt C.brkCnt "break"

-- | reference for continue label
refContinue :: (Show e, Show i) => i -> C.Asm TargetLabelCtx e ()
refContinue = ref "continue"

-- | reference for break label (applying value by `Htcc.Asm.Intrinsic.Structure.Internal.cntCnt`)
refHContinue :: Show e => C.Asm TargetLabelCtx e ()
refHContinue = refCnt C.cntCnt "continue"

-- | reference for goto label
refGoto :: T.Text -> C.Asm TargetLabelCtx e ()
refGoto ident = C.Asm $ \x -> do
    cf <- readIORef (C.curFn x)
    unless (isJust cf) $ err "stray label"
    T.putStrLn $ ".L.label." <> fromJust cf <> "." <> ident

-- | reference to begin label
refBegin :: (Show e, Show i) => i -> C.Asm TargetLabelCtx e ()
refBegin = ref "begin"

-- | reference to end label
refEnd :: (Show e, Show i) => i -> C.Asm TargetLabelCtx e ()
refEnd = ref "end"

-- | reference to else label
refElse :: (Show e, Show i) => i -> C.Asm TargetLabelCtx e ()
refElse = ref "else"

-- | reference to general label
ref :: (Show e, Show i) => T.Text -> i -> C.Asm TargetLabelCtx e ()
ref lbl n = C.Asm $ \x -> do
    cf <- readIORef (C.curFn x)
    unless (isJust cf) $ err "stray label"
    T.putStrLn $ ".L." <> lbl <> "." <> fromJust cf <> "." <> tshow n

-- | generate cases and return abstract tree
makeCases :: (Show e, Enum e, Integral e, Show i, Num i) => [ATree i] -> C.Asm TextLabelCtx e [ATree i]
makeCases cases = C.Asm $ \x -> do
    cf <- readIORef (C.curFn x)
    forM cases $ \case
        (ATNode (ATCase _ cn) t lhs rhs) -> do
            modifyIORef (C.lblCnt x) succ
            n' <- readIORef (C.lblCnt x)
            T.putStrLn $ "\tcmp rax, " <> tshow cn
            T.putStrLn $ "\tje .L.case." <> fromJust cf <> "." <> tshow n'
            return $ ATNode (ATCase (fromIntegral n') cn) t lhs rhs
        (ATNode (ATDefault _) t lhs rhs) -> do
            modifyIORef (C.lblCnt x) succ
            n' <- readIORef (C.lblCnt x)
            T.putStrLn $ "\tjmp .L.case." <> fromJust cf <> "." <> tshow n'
            return $ ATNode (ATDefault $ fromIntegral n') t lhs rhs
        at -> return at
