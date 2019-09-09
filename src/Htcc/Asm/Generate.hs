{-|
Module      : Htcc.Asm.Generate
Description : Assembly code generator
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Assembly code generator
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Generate (
    -- * Generator
    genStmt,
    casm
) where

-- Imports universal modules
import Control.Exception (finally)
import Control.Monad ((>=>), zipWithM_, forM_, unless)
import Control.Monad.Fix (fix)
import Data.Bits (complement)
import Data.List (find)
import Data.Either (either)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

-- Imports Tokenizer and parser
import Htcc.Utils (err, putStrLnErr, putStrErr, counter, tshow, toInts)
import Htcc.Token (TokenIdx, tokenize)
import Htcc.Parse (ATKind (..), ATree (..), fromATKindFor, isATForInit, isATForCond, isATForStmt, isATForIncr, parse, varNum)

-- Imports about assembly
import Htcc.Asm.Intrinsic.Register
import Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Instruction as I
import qualified Htcc.Asm.Intrinsic.Utils as I

{-# INLINE declMain #-}
declMain :: T.Text
declMain = I.declIS <> I.defGLbl "main"

{-# INLINE prologue #-}
prologue :: (Num i, Show i, I.BinaryInstruction i) => i -> T.Text
prologue = (T.append (I.push rbp <> I.mov rbp rsp) . I.sub rsp) . (*8)

{-# INLINE epilogue #-}
epilogue :: T.Text
epilogue = I.mov rsp rbp <> I.pop rbp <> I.ret

genLVal :: (Show i, I.BinaryInstruction i) => ATree i -> IO ()
genLVal (ATNode (ATLVar v) _ _) = T.putStr $ I.mov rax rbp <> I.sub rax v <> I.push rax
genLVal _ = err "lvalue required as left operand of assignment"

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
genStmt :: (Show i, Ord i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => IO Int -> ATree i -> IO ()
genStmt c lc@(ATNode (ATDefFunc x Nothing) _ st) = T.putStr (I.defGLbl x <> prologue (varNum lc)) >> genStmt c st >> T.putStr epilogue
genStmt c lc@(ATNode (ATDefFunc x (Just args)) _ st) = do
    T.putStr $ I.defGLbl x <> prologue (varNum lc)
    zipWithM_ (\(ATNode (ATLVar o) _ _) reg -> T.putStr $ I.mov (Ref $ rax `osub` o) reg) args [rdi, rsi, rdx, rcx, rn 8, rn 9]
    -- zipWithM_ (\(ATNode (ATLVar o) _ _) reg -> T.putStrLn $ "\tmov [rbp-" <> tshow o <> "], " <> tshow reg) args [rdi, rsi, rdx, rcx, rn 8, rn 9]
    genStmt c st
    T.putStr epilogue
genStmt _ (ATNode (ATCallFunc x Nothing) _ _) = T.putStr $ I.mov rbx rsp <> I.and rsp (complement 0x0f :: Int) <> I.call x <> I.mov rsp rbx <> I.push rax
genStmt c (ATNode (ATCallFunc x (Just args)) _ _) = let toReg = take 6 args; toStack = drop 6 args in do
    zipWithM_ (\t reg -> genStmt c t >> T.putStr (I.pop reg)) toReg [rdi, rsi, rdx, rcx, rn 8, rn 9]
    T.putStr (I.mov rbx rsp <> I.and rsp (complement 0x0f :: Int))
    unless (null toStack) (forM_ (reverse toStack) $ genStmt c) -- FIXME: RSP should be 16 multiple. 
    T.putStr (I.call x <> I.mov rsp rbx <> I.push rax)
genStmt c (ATNode (ATBlock stmts) _ _) = mapM_ (genStmt c >=> const (T.putStr $ I.pop rax)) stmts
genStmt c (ATNode (ATFor exps) _ _) = do
    n <- c
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForInit exps
    T.putStr $ I.defBegin n
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForCond exps
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForStmt exps
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForIncr exps
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n 
genStmt c (ATNode ATWhile lhs rhs) = do
    n <- c
    T.putStr $ I.defBegin n
    genStmt c lhs
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n
genStmt c (ATNode ATIf lhs rhs) = do
    genStmt c lhs
    n <- c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt c (ATNode ATElse (ATNode ATIf llhs rrhs) rhs) = do
    genStmt c llhs
    n <- c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl "else" n)
    genStmt c rrhs
    T.putStr $ I.jmp (I.refEnd n)
    T.putStr $ I.defLLbl "else" n
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt _ (ATNode ATElse _ _) = error "Asm code generator shold not reached here. Maybe abstract tree is broken it cause (bug)."
genStmt c (ATNode ATReturn lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.mov rsp rbp <> I.pop rbp <> I.ret)
genStmt c (ATNode ATNot lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.not rax <> I.push rax)
genStmt _ (ATNode (ATNum x) _ _) = T.putStr $ I.push x
genStmt _ n@(ATNode (ATLVar _) _ _) = genLVal n >> T.putStr (I.pop rax <> I.mov rax (Ref rax) <> I.push rax) 
genStmt c (ATNode ATAssign lhs rhs) = genLVal lhs >> genStmt c rhs >> T.putStr (I.pop rdi <> I.pop rax <> I.mov (Ref rax) rdi <> I.push rdi)
genStmt c (ATNode k lhs rhs) = flip finally (T.putStr $ I.push rax) $ genStmt c lhs *> genStmt c rhs *> T.putStr (I.pop rdi) *> T.putStr (I.pop rax) *> case k of
    ATAdd -> T.putStr $ I.add rax rdi 
    ATSub -> T.putStr $ I.sub rax rdi 
    ATMul -> T.putStr $ I.imul rax rdi 
    ATDiv -> T.putStr $ I.cqo <> I.idiv rdi
    ATMod -> T.putStr $ I.cqo <> I.idiv rdi <> I.mov rax rdx
    ATAnd -> T.putStr $ I.and rax rdi
    ATOr -> T.putStr $ I.or rax rdi 
    ATXor -> T.putStr $ I.xor rax rdi 
    ATShl -> T.putStr $ I.mov edx eax <> I.mov rax rdi <> I.mov ecx edx <> I.shl rax cl 
    ATShr -> T.putStr $ I.push rax <> I.mov rax rdi <> I.mov edx eax <> I.pop rax <> I.mov ecx edx <> I.sar rax cl  
    ATEQ -> T.putStr $ I.cmp rax rdi <> I.sete al <> I.movzb rax al
    ATNEQ -> T.putStr $ I.cmp rax rdi <> I.setne al <> I.movzb rax al
    ATLT -> T.putStr $ I.cmp rax rdi <> I.setl al <> I.movzb rax al
    ATLEQ -> T.putStr $ I.cmp rax rdi <> I.setle al <> I.movzb rax al
    ATGT -> T.putStr $ I.cmp rax rdi <> I.setg al <> I.movzb rax al
    ATGEQ -> T.putStr $ I.cmp rax rdi <> I.setge al <> I.movzb rax al
    _ -> err "Failed to assemble."
genStmt _ _ = return ()

repSpace :: Integral i => i -> IO ()
repSpace = flip (>>) (putStrLnErr (T.singleton '^')) . mapM_ (putStrErr . T.pack . flip replicate ' ' . pred) . toInts

tokenizeErrExit :: (Integral i, Show i) => T.Text -> (i, T.Text) -> IO ()
tokenizeErrExit xs e = do
    ($ e) . fix $ \f (i, s) -> unless (T.null s) $ do
        putStrLnErr (tshow i <> ": error: stray '" <> T.singleton (T.head s) <> "' in program")
        putStrLnErr xs
        repSpace i
        f (succ i, T.tail s)
    exitFailure

parseErrExit :: (Integral i, Show i) => T.Text -> (T.Text, TokenIdx i) -> IO ()
parseErrExit xs (s, (i, _)) = do
    putStrLnErr (tshow i <> ": error: " <> s)
    putStrLnErr xs
    repSpace i
    exitFailure

-- | Generate full assembly code from C language program
casm :: String -> IO ()
casm xs = let sline = T.pack xs in flip (either (tokenizeErrExit sline)) (f xs) $ \x -> 
    flip (either $ parseErrExit sline) (parse x) $ \ys -> do
        inc <- counter 0
        -- T.putStr I.declIS >> mapM_ (genStmt inc) ys
        T.putStr declMain >> T.putStr (prologue $ snd ys) >> mapM_ (genStmt inc) (fst ys) >> T.putStr (I.pop rax) >> T.putStr epilogue
        where
            f = tokenize :: String -> Either (Int, T.Text) [TokenIdx Int]
