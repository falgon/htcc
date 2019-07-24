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
import Data.Either (either)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

import Htcc.Utils (err, putStrLnErr)
import Htcc.Token (Token (..), tokenize)
import Htcc.Parse (ATKind (..), ATree (..), parse)

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
genLVal (ATNode (ATLVar v) _ _) = T.putStr "\tmov rax, rbp\n\tsub rax, " >> putStrLn (show v) >> T.putStrLn "\tpush rax"
genLVal _ = err "lvalue required as left operand of assignment"

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
genStmt :: Show i => ATree i -> IO ()
genStmt (ATNode ATReturn lhs _) = genStmt lhs >> T.putStrLn "\tpop rax\n\tmov rsp, rbp\n\tpop rbp\n\tret"
genStmt (ATNode (ATNum x) _ _) = T.putStrLn $ T.append "\tpush " $ tshow x
genStmt n@(ATNode (ATLVar _) _ _) = genLVal n >> T.putStrLn "\tpop rax" >> T.putStrLn "\tmov rax, [rax]" >> T.putStrLn "\tpush rax"
genStmt (ATNode ATAssign lhs rhs) = genLVal lhs >> genStmt rhs >> T.putStrLn "\tpop rdi" >> T.putStrLn "\tpop rax" >> T.putStrLn "\tmov [rax], rdi" >> T.putStrLn "\tpush rdi"
genStmt (ATNode k lhs rhs) = flip finally (T.putStrLn "\tpush rax") $ genStmt lhs >> genStmt rhs >> T.putStrLn "\tpop rdi" >> T.putStrLn "\tpop rax" >> case k of
    ATAdd -> T.putStrLn "\tadd rax, rdi"
    ATSub -> T.putStrLn "\tsub rax, rdi"
    ATMul -> T.putStrLn "\timul rax, rdi"
    ATDiv -> T.putStrLn "\tcqo\n\tidiv rdi"
    ATEQ -> T.putStrLn "\tcmp rax, rdi\n\tsete al\n\tmovzb rax, al"
    ATNEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetne al\n\tmovzb rax, al"
    ATLT -> T.putStrLn "\tcmp rax, rdi\n\tsetl al\n\tmovzb rax, al"
    ATLEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetle al\n\tmovzb rax, al"
    ATGT -> T.putStrLn "\tcmp rax, rdi\n\tsetg al\n\tmovzb rax, al"
    ATGEQ -> T.putStrLn "\tcmp rax, rdi\n\tsetge al\n\tmovzb rax, al"
    _ -> err "Failed to assemble."
genStmt _ = return ()

-- | Generate full assembly code from C language program
casm :: String -> IO ()
casm xs = flip (either (outErr (T.pack xs))) (tokenize xs :: Either Int [Token Int]) $ \x -> flip (maybe (err "Failed to construct abstract tree")) (parse x) $ \(ys, n) ->
        T.putStrLn declMain >> T.putStrLn (prologue n) >> mapM_ genStmt ys >> T.putStrLn "\tpop rax" >> T.putStrLn epilogue
    where
        outErr ys n = putStrLnErr ys >> putStrLnErr (T.pack $ replicate n ' ') >> putStrLnErr "^ Invalid token here" >> exitFailure
