{-# LANGUAGE OverloadedStrings #-}
module Tests.Test1 (
    test
) where

import           Control.Exception (finally)
import qualified Data.Text         as T
import           Tests.Utils

test :: String -> IO (Int, String)
test x = flip finally (clean ["tmp"]) $ do
    execErrFin $ "echo '" <> T.pack x <> "' | stack exec htcc -- /dev/stdin | gcc -no-pie -xassembler -o tmp -"
    exec "./tmp" >>= exitCode (\ec -> (ec, x) <$ (putStr x *> putStrLn ": [Processing]")) (return (0, x))
