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
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Asm.Generate (
    -- * Generator
    genStmt,
    casm
) where

-- Imports universal modules
import Control.Exception (finally)
import Control.Monad (zipWithM_, unless)
import qualified Data.ByteString as B
import Data.List (find)
import Data.Either (either)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

-- Imports Tokenizer and parser
import Htcc.Utils (err, putStrLnErr, putStrErr, counter, tshow, toInts, splitAtLen)
import qualified Htcc.Token as HT
import Htcc.Parse (ATKind (..), ATree (..), GVar (..), Literal (..), fromATKindFor, isATForInit, isATForCond, isATForStmt, isATForIncr, parse, stackSize)

-- Imports about assembly
import Htcc.Asm.Intrinsic.Register
import Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Instruction as I
import qualified Htcc.Asm.Intrinsic.Utils as I
import qualified Htcc.CRules.Types as CR

{-# INLINE prologue #-}
prologue :: forall i. Integral i => i -> T.Text
prologue = T.append (I.push rbp <> I.mov rbp rsp) . I.sub rsp . (fromIntegral :: i -> Integer)

{-# INLINE epilogue #-}
epilogue :: T.Text
epilogue = I.leave <> I.ret

{-# INLINE load #-}
load :: CR.TypeKind -> T.Text
load t 
    | CR.sizeof t == 1 = I.pop rax <> I.movsx rax (I.byte I.Ptr (Ref rax)) <> I.push rax
    | CR.sizeof t == 2 = I.pop rax <> I.movsx rax (I.word I.Ptr (Ref rax)) <> I.push rax
    | CR.sizeof t == 4 = I.pop rax <> I.movsxd rax (I.dword I.Ptr (Ref rax)) <> I.push rax
    | otherwise = I.pop rax <> I.mov rax (Ref rax) <> I.push rax

{-# INLINE store #-}
store :: CR.TypeKind -> T.Text
store t
    | CR.sizeof t == 1 = I.pop rdi <> I.pop rax <> I.mov (Ref rax) dil <> I.push rdi
    | CR.sizeof t == 2 = I.pop rdi <> I.pop rax <> I.mov (Ref rax) di <> I.push rdi
    | CR.sizeof t == 4 = I.pop rdi <> I.pop rax <> I.mov (Ref rax) edi <> I.push rdi
    | otherwise = I.pop rdi <> I.pop rax <> I.mov (Ref rax) rdi <> I.push rdi

genAddr :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => IO Int -> ATree i -> IO ()
genAddr _ (ATNode (ATLVar _ v) _ _ _) = T.putStr $ I.lea rax (Ref $ rbp `osub` v) <> I.push rax
genAddr _ (ATNode (ATGVar _ n) _ _ _) = T.putStr $ I.push (I.Offset n)
genAddr c (ATNode ATDeref _ lhs _) = genStmt c lhs
genAddr c (ATNode (ATMemberAcc m) _ lhs _) = genAddr c lhs >> T.putStr (I.pop rax <> I.add rax (CR.smOffset m) <> I.push rax)
genAddr _ _ = err "lvalue required as left operand of assignment"

genLval :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => IO Int -> ATree i -> IO ()
genLval c xs@(ATNode _ t _ _)
    | CR.isCTArray t = err "lvalue required as left operand of assignment"
    | otherwise = genAddr c xs
genLval _ _ = err "internal compiler error: genLval catch ATEmpty"

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
genStmt :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => IO Int -> ATree i -> IO ()
genStmt c lc@(ATNode (ATDefFunc x Nothing) _ st _) = T.putStr (I.defGLbl x <> prologue (stackSize lc)) >> genStmt c st
genStmt c lc@(ATNode (ATDefFunc x (Just args)) _ st _) = do -- TODO: supports more than 7 arguments
    T.putStr $ I.defGLbl x <> prologue (stackSize lc)
    flip (`zipWithM_` args) argRegs $ \(ATNode (ATLVar t o) _ _ _) reg -> 
        maybe (err "internal compiler error: there is no register that fits the specified size") (T.putStr . I.mov (Ref $ rbp `osub` o)) $
            find ((== CR.sizeof t) . byteWidth) reg
    genStmt c st
genStmt _ (ATNode (ATCallFunc x Nothing) _ _ _) = T.putStr $ I.call x <> I.push rax
genStmt c (ATNode (ATCallFunc x (Just args)) _ _ _) = let (n', toReg, _) = splitAtLen 6 args in do -- TODO: supports more than 7 arguments
    mapM_ (genStmt c) toReg
    mapM_ (T.putStr . I.pop) $ popRegs n'
    -- unless (null toStack) $ forM_ (reverse toStack) $ genStmt c
    n <- c
    T.putStr $ I.mov rax rsp <> I.and rax (0x0f :: Int)
    T.putStr $ I.jnz $ I.refLLbl ".call." n
    T.putStr $ I.mov rax (0 :: Int) <> I.call x
    T.putStr $ I.jmp $ I.refLLbl ".end." n
    T.putStr $ I.defLLbl ".call." n
    T.putStr $ I.sub rsp (8 :: Int) <> I.mov rax (0 :: Int) <> I.call x <> I.add rsp (8 :: Int)
    T.putStr $ I.defLLbl ".end." n
    T.putStr $ I.push rax
genStmt c (ATNode (ATBlock stmts) _ _ _) = mapM_ (genStmt c) stmts
genStmt c (ATNode (ATStmtExpr stmts) _ _ _) = mapM_ (genStmt c) stmts
genStmt c (ATNode (ATFor exps) _ _ _) = do
    n <- c
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForInit exps
    T.putStr $ I.defBegin n
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForCond exps
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForStmt exps
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForIncr exps
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n 
genStmt c (ATNode ATWhile _ lhs rhs) = do
    n <- c
    T.putStr $ I.defBegin n
    genStmt c lhs
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n
genStmt c (ATNode ATIf _ lhs rhs) = do
    genStmt c lhs
    n <- c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt c (ATNode ATElse _ (ATNode ATIf _ llhs rrhs) rhs) = do
    genStmt c llhs
    n <- c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".else." n)
    genStmt c rrhs
    T.putStr $ I.jmp (I.refEnd n)
    T.putStr $ I.defLLbl ".else." n
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt _ (ATNode ATElse _ _ _) = error "Asm code generator shold not reached here. Maybe abstract tree is broken it cause (bug)."
genStmt c (ATNode ATReturn _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> epilogue) 
genStmt c (ATNode ATExprStmt _ lhs _) = genStmt c lhs >> T.putStr (I.add rsp (8 :: Int))
genStmt c (ATNode ATBitNot _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.not rax <> I.push rax)
genStmt c (ATNode ATAddr _ lhs _) = genAddr c lhs
genStmt c (ATNode ATDeref t lhs _) = genStmt c lhs >> unless (CR.isCTArray t) (T.putStr $ load t) 
genStmt c (ATNode ATNot _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.sete al <> I.movzb rax al <> I.push rax)
genStmt _ (ATNode (ATNum x) _ _ _) = T.putStr $ I.push x
genStmt c n@(ATNode (ATLVar _ _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c n@(ATNode (ATGVar _ _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c n@(ATNode (ATMemberAcc _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c (ATNode ATAssign t lhs rhs) = genLval c lhs >> genStmt c rhs >> T.putStr (store t)
genStmt _ (ATNode (ATNull _) _ _ _) = return ()
genStmt c (ATNode k t lhs rhs) = flip finally (T.putStr $ I.push rax) $ genStmt c lhs *> genStmt c rhs *> T.putStr (I.pop rdi) *> T.putStr (I.pop rax) *> case k of
    ATAdd -> T.putStr $ I.add rax rdi 
    ATSub -> T.putStr $ I.sub rax rdi 
    ATAddPtr -> flip (maybe (err "The type is not pointer")) (CR.derefMaybe t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.add rax rdi
    ATSubPtr -> flip (maybe (err "The type is not pointer")) (CR.derefMaybe t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.sub rax rdi
    ATPtrDis -> flip (maybe (err "The type is not pointer")) (CR.derefMaybe $ atype lhs) $ \dt -> T.putStr $ I.sub rax rdi <> I.cqo <> I.mov rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.idiv rdi
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
repSpace = flip (>>) (putStrErr (T.singleton '^')) . mapM_ (putStrErr . T.pack . flip replicate ' ' . pred) . toInts

tokenizeErrExit :: (Integral i, Show i) => T.Text -> (HT.TokenLCNums i, T.Text) -> IO ()
tokenizeErrExit xs e = let errMesPre = T.replicate 4 " " <> tshow (HT.tkLn (fst e)) in do
    putStrLnErr (tshow (fst e) <> ": error: stray '" <> snd e <> "' in program")
    putStrErr $ errMesPre <> " | "
    putStrLnErr (T.lines xs !! pred (fromIntegral $ HT.tkLn (fst e)))
    putStrErr $ T.replicate (T.length errMesPre) " " <> " | "
    repSpace (HT.tkCn $ fst e) >> putStrLnErr ""
    exitFailure

parseErrExit :: (Integral i, Show i) => T.Text -> (T.Text, HT.TokenLC i) -> IO ()
parseErrExit xs (s, (i, etk)) = let errMesPre = T.replicate 4 " " <> tshow (HT.tkLn i) in do
    putStrLnErr (tshow i <> ": error: " <> s)
    putStrErr $ errMesPre <> " | "
    putStrLnErr (T.lines xs !! pred (fromIntegral $ HT.tkLn i))
    putStrErr $ T.replicate (T.length errMesPre) " " <> " | "
    repSpace (HT.tkCn i) >> putStrLnErr (T.replicate (pred $ HT.length etk) "~")
    exitFailure

dataSection :: M.Map T.Text GVar -> [Literal] -> IO ()
dataSection gvars lits = do
    T.putStrLn ".data"
    mapM_ (\(Literal _ n cnt) -> T.putStrLn (".L.data." <> tshow n <> ":") >> T.putStr "\t.byte " >> T.putStrLn (T.intercalate ", " $ map tshow $ B.unpack cnt)) lits
    mapM_ (\(n, GVar t) -> T.putStrLn (n <> T.singleton ':') >> T.putStrLn ("\t.zero " <> tshow (CR.sizeof t))) $ M.toList gvars

textSection :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => [ATree i] -> IO ()
textSection tk = do
    inc <- counter 0
    T.putStrLn ".text" >> mapM_ (genStmt inc) tk

-- | Generate full assembly code from C language program
casm :: T.Text -> IO ()
casm xs = flip (either (tokenizeErrExit xs)) (f xs) $ \x -> 
    flip (either $ parseErrExit xs) (parse x) $ \(tk, gvars, lits) -> T.putStr I.declIS >> dataSection gvars lits >> textSection tk
        where
            f = HT.tokenize :: T.Text -> Either (HT.TokenLCNums Int, T.Text) [HT.TokenLC Int]

