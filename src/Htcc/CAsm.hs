{-|
Module      : Htcc.CAsm
Description : Assembly code generator
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.CAsm (
    genStmt,
    casm
) where

import Control.Exception (finally)
import Control.Monad ((>=>))
import Data.List (find)
import Data.Either (either)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

import Htcc.Utils (err, putStrLnErr, counter)
import Htcc.Token (Token (..), tokenize)
import Htcc.Parse (ATKind (..), ATree (..), fromATKindFor, isATForInit, isATForCond, isATForStmt, isATForIncr, parse)

{-# INLINE tshow #-}
tshow :: Show a => a -> T.Text
tshow = T.pack . show

{-# INLINE declMain #-}
declMain :: T.Text
declMain = ".intel_syntax noprefix\n.global main\nmain:"

{-# INLINE prologue #-}
prologue :: (Num i, Show i) => i -> T.Text
prologue = T.append "\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, " . tshow . (*8)

{-# INLINE epilogue #-}
epilogue :: T.Text
epilogue = "\tmov rsp, rbp\n\tpop rbp\n\tret"

genLVal :: Show i => ATree i -> IO ()
genLVal (ATNode (ATLVar v) _ _) = T.putStr "\tmov rax, rbp\n\tsub rax, " >> print v >> T.putStrLn "\tpush rax"
genLVal _ = err "lvalue required as left operand of assignment"

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
genStmt :: Show i => IO Int -> ATree i -> IO ()
genStmt _ (ATNode (ATCallFunc x Nothing) _ _) = T.putStrLn "\tmov rbx, rsp\n\tand rsp, ~0x0f" >> T.putStr "\tcall " >> T.putStrLn x >> T.putStrLn "\tmov rsp, rbx\n\tpush rax"
genStmt c (ATNode (ATBlock stmts) _ _) = mapM_ (genStmt c >=> const (T.putStrLn "\tpop rax")) stmts
genStmt c (ATNode (ATFor exps) _ _) = do
    n <- show <$> c
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForInit exps
    T.putStr ".Lbegin" >> putStrLn (n ++ ":")
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForCond exps
    T.putStr "\tpop rax\n\tcmp rax, 0\n\tje .Lend" >> putStrLn n
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForStmt exps
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForIncr exps
    T.putStr "\tjmp .Lbegin" >> putStrLn n >> T.putStr ".Lend" >> putStrLn (n ++ ":")
genStmt c (ATNode ATWhile lhs rhs) = do
    n <- show <$> c
    T.putStr ".Lbegin" >> putStrLn (n ++ ":") >> genStmt c lhs >> T.putStr "\tpop rax\n\tcmp rax, 0\n\tje .Lend" >> putStrLn n >> genStmt c rhs >> T.putStr "\tjmp .Lbegin" >> putStrLn n >> T.putStr ".Lend" >> putStrLn (n ++ ":") 
genStmt c (ATNode ATIf lhs rhs) = do
    genStmt c lhs >> T.putStr "\tpop rax\n\tcmp rax, 0\n\tje .Lend"
    n <- show <$> c
    putStrLn n >> genStmt c rhs >> T.putStr ".Lend" >> putStrLn (n ++ ":")
genStmt c (ATNode ATElse (ATNode ATIf llhs rrhs) rhs) = do
    genStmt c llhs >> T.putStr "\tpop rax\n\tcmp rax, 0\n\tje .Lelse"
    n <- show <$> c
    putStrLn n >> genStmt c rrhs >> T.putStr "\tjmp .Lend" >> putStrLn n >> T.putStr ".Lelse" >> putStrLn (n ++ ":") >> genStmt c rhs >> T.putStr ".Lend" >> putStrLn (n ++ ":")
genStmt _ (ATNode ATElse _ _) = error "Asm code generator shold not reached here. Maybe abstract tree is broken it cause (bug)."
genStmt c (ATNode ATReturn lhs _) = genStmt c lhs >> T.putStrLn "\tpop rax\n\tmov rsp, rbp\n\tpop rbp\n\tret"
genStmt c (ATNode ATNot lhs _) = genStmt c lhs >> T.putStrLn "\tpop rax\n\tnot rax\n\tpush rax"
genStmt _ (ATNode (ATNum x) _ _) = T.putStrLn $ T.append "\tpush " $ tshow x
genStmt _ n@(ATNode (ATLVar _) _ _) = genLVal n >> T.putStrLn "\tpop rax" >> T.putStrLn "\tmov rax, [rax]" >> T.putStrLn "\tpush rax"
genStmt c (ATNode ATAssign lhs rhs) = genLVal lhs >> genStmt c rhs >> T.putStrLn "\tpop rdi" >> T.putStrLn "\tpop rax" >> T.putStrLn "\tmov [rax], rdi" >> T.putStrLn "\tpush rdi"
genStmt c (ATNode k lhs rhs) = flip finally (T.putStrLn "\tpush rax") $ genStmt c lhs >> genStmt c rhs >> T.putStrLn "\tpop rdi" >> T.putStrLn "\tpop rax" >> case k of
    ATAdd -> T.putStrLn "\tadd rax, rdi"
    ATSub -> T.putStrLn "\tsub rax, rdi"
    ATMul -> T.putStrLn "\timul rax, rdi"
    ATDiv -> T.putStrLn "\tcqo\n\tidiv rdi"
    ATMod -> T.putStrLn "\tcqo\n\tidiv rdi\n\tmov rax, rdx"
    ATAnd -> T.putStrLn "\tand rax, rdi"
    ATOr -> T.putStrLn "\tor rax, rdi"
    ATXor -> T.putStrLn "\txor rax, rdi"
    ATShl -> T.putStrLn "\tmov edx, eax\n\tmov rax, rdi\n\tmov ecx, edx\n\tshl rax, cl"
    ATShr -> T.putStrLn "\tpush rax\n\tmov rax, rdi\n\tmov edx, eax\n\tpop rax\n\tmov ecx, edx\n\tsar rax, cl"
    ATEQ -> T.putStrLn "\tcmp rax, rdi\n\tsete al\n\tmovzb rax, al"
    ATNEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetne al\n\tmovzb rax, al"
    ATLT -> T.putStrLn "\tcmp rax, rdi\n\tsetl al\n\tmovzb rax, al"
    ATLEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetle al\n\tmovzb rax, al"
    ATGT -> T.putStrLn "\tcmp rax, rdi\n\tsetg al\n\tmovzb rax, al"
    ATGEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetge al\n\tmovzb rax, al"
    _ -> err "Failed to assemble."
genStmt _ _ = return ()

-- | Generate full assembly code from C language program
casm :: String -> IO ()
casm xs = flip (either (outErr (T.pack xs))) (tokenize xs :: Either Int [Token Int]) $ \x -> flip (maybe (err "Syntax error (Failed to construct abstract tree)")) (parse x) $ \(ys, n) -> do
    inc <- counter 0
    T.putStrLn declMain >> T.putStrLn (prologue n) >> mapM_ (genStmt inc) ys >> T.putStrLn "\tpop rax" >> T.putStrLn epilogue
    where
        outErr ys n = putStrLnErr ys >> putStrLnErr (T.pack $ replicate n ' ') >> putStrLnErr "^ Invalid token here" >> exitFailure
