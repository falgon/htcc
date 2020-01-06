{-|
Module      : Htcc.Asm.Intrinsic.Structure.Internal
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Htcc.Asm.Intrinsic.Structure.Internal (
    Asm (..),
    AsmInfo (..),
    AsmCodeCtx,
    unCtx,
    runAsm,
    putStrWithIndent,
    putStrLnWithIndent,
    errCtx,
    writeCurFn,
    section,
    labeled
) where

import Data.IORef (IORef, newIORef, writeIORef)
import Data.Semigroup (Semigroup (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Finally (MonadFinally (..))

import Htcc.Utils (err)

-- | Counter and label information used when generating assembly code
data AsmInfo e = AsmInfo
    {
        inLabel :: Bool, -- ^ the flag that indicates whether it is inside the label. If True, indent by single tab,
        lblCnt :: IORef Int, -- ^ the label counter
        brkCnt :: IORef (Maybe Int), -- ^ the @break@ label counter
        cntCnt :: IORef (Maybe Int), -- ^ the @continue@ label counter
        curFn :: IORef (Maybe T.Text) -- ^ the function being processed
    }

-- | A monad that represents the context of the assembly code
newtype Asm ctx e a = Asm 
    { 
        unAsm :: AsmInfo e -> IO a -- ^ Function that determines the structure of assembly code
    }

instance Functor (Asm ctx e) where
    fmap f asm = Asm $ fmap f . unAsm asm

instance Applicative (Asm ctx e) where
    pure = Asm . const . return
    f <*> x = Asm (\ai -> unAsm f ai <*> unAsm x ai)

instance Monad (Asm ctx e) where
    return = pure
    x >>= f = Asm (\ai -> unAsm x ai >>= flip unAsm ai . f)

instance MonadFinally (Asm ctx e) where
    bracket' a r mc = do
        r' <- a
        a' <- mc r'
        (a',) <$> r r' (Just a')

instance Semigroup (Asm ctx e a) where
    (<>) = (*>)

instance Monoid a => Monoid (Asm ctx e a) where
    mempty = Asm $ const $ return mempty
    mappend = (<>)

-- | Type representing assembly code
data AsmCodeCtx

-- | the function to switch context
unCtx :: Asm ctx e a -> Asm ctx' e a
unCtx = Asm . unAsm

-- | the executor that outputs assembly code
runAsm :: (Num e, Enum e) => Asm AsmCodeCtx e a -> IO a
runAsm asm = do
    putStrLn ".intel_syntax noprefix"
    c <- newIORef 0
    brk <- newIORef Nothing
    cnt <- newIORef Nothing
    fn <- newIORef Nothing
    unAsm asm (AsmInfo False c brk cnt fn)

-- | print a string with indentation, output is broken on a new line
putStrLnWithIndent :: T.Text -> Asm ctx e ()
putStrLnWithIndent s = Asm $ \x -> T.putStrLn $ if inLabel x then '\t' `T.cons` s else s

-- | print a string with indentation
putStrWithIndent :: T.Text -> Asm ctx e ()
putStrWithIndent s = Asm $ \x -> T.putStr $ if inLabel x then '\t' `T.cons` s else s

-- | The error context.
-- when this is executed, 
-- it will exit the application immediately with `System.Exit.exitFailure` after printing the message.
errCtx :: T.Text -> Asm ctx e ()
errCtx = Asm . const . err

-- | rewriting functions during processing
writeCurFn :: Maybe T.Text -> Asm ctx e ()
writeCurFn fname = Asm $ \x -> writeIORef (curFn x) fname

-- | represents a section of assembly code
section :: T.Text -> Asm ctx e a -> Asm AsmCodeCtx e a
section sec asm = putStrLnWithIndent ('.' `T.cons` sec) *> unCtx asm

-- | switch to process in label
labeled :: Asm ctx e a -> Asm ctx e a
labeled asm = Asm $ \x -> unAsm asm $ x { inLabel = True }
