{-# LANGUAGE OverloadedStrings #-}
module Tests.SubProcTests.StatementEqual (
    test
) where

import           Control.Exception (finally)
import qualified Data.Text         as T
import           Tests.Utils

test :: String -> IO (Int, String)
test x = flip finally (clean ["tmp"]) $ do
    htccCmd <- htccCommand
    asmCmd <- assemblerCommand ["-no-pie", "-x", "assembler", "-o", "tmp", "-"]
    execErrFin $ mconcat
        [ "echo '"
        , T.pack x
        , "' | "
        , htccCmd
        , " /dev/stdin | "
        , asmCmd
        ]
    exec "./tmp"
        >>= exitCode
            (\ec -> (ec, x) <$ (putStr x *> putStrLn " [Compiling]"))
            (return (0, x))
