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
import System.Exit (exitFailure)
import C.Parse

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
generate :: Show i => ATree i -> IO ()
generate (ATNode (ATNum x) _ _) = putStrLn $ "\tpush " ++ show x
generate (ATNode k lhs rhs) = flip finally (putStrLn "\tpush rax") $ generate lhs >> generate rhs >> putStrLn "\tpop rdi" >> putStrLn "\tpop rax" >> case k of
    ATAdd -> putStrLn "\tadd rax, rdi"
    ATSub -> putStrLn "\tsub rax, rdi"
    ATMul -> putStrLn "\timul rax, rdi"
    ATDiv -> putStrLn "\tcqo" >> putStrLn "\tidiv rdi"
    _ -> exitFailure
generate _ = return ()
