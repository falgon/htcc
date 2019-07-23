{-|
Module      : C.Generate
Description : Assembly code generator
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
module C.Generate (
    generate
) where

import Control.Exception (finally)
import C.Utils (err)
import C.Parse (ATKind (..), ATree (..))

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
generate :: Show i => ATree i -> IO ()
generate (ATNode (ATNum x) _ _) = putStrLn $ "\tpush " ++ show x
generate (ATNode k lhs rhs) = flip finally (putStrLn "\tpush rax") $ generate lhs >> generate rhs >> putStrLn "\tpop rdi" >> putStrLn "\tpop rax" >> case k of
    ATAdd -> putStrLn "\tadd rax, rdi"
    ATSub -> putStrLn "\tsub rax, rdi"
    ATMul -> putStrLn "\timul rax, rdi"
    ATDiv -> putStrLn "\tcqo" >> putStrLn "\tidiv rdi"
    ATEQ -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsete al" >> putStrLn "\tmovzb rax, al"
    ATNEQ -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsetne al" >> putStrLn "\tmovzb rax, al"
    ATLT -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsetl al" >> putStrLn "\tmovzb rax, al"
    ATLEQ -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsetle al" >> putStrLn "\tmovzb rax, al"
    ATGT -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsetg al" >> putStrLn "\tmovzb rax, al"
    ATGEQ -> putStrLn "\tcmp rax, rdi" >> putStrLn "\tsetge al" >> putStrLn "\tmovzb rax, al"
    _ -> err "Failed to assemble."
generate _ = return ()
