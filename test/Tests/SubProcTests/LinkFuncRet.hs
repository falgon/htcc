{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.SubProcTests.LinkFuncRet (
    test
) where

import           Control.Exception (finally)
import           Control.Monad     (forM_)
import qualified Data.Text         as T
import           Tests.Utils

test :: String -> [String] -> IO (Int, String)
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s"] ++ obj) $ do
        htccCmd <- htccCommand
        linkCmd <- assemblerCommand $ obj ++ ["tmp.s", "-no-pie", "-o", "tmp"]
        execErrFin $ mconcat
            [ "echo \'"
            , T.pack x
            , "\' | "
            , htccCmd
            , " /dev/stdin > tmp.s"
            ]
        forM_ fnames $ \fname ->
            assemblerCommand
                [ "-c"
                , "test/Tests/csrc/externals/" <> fname <> ".c"
                ]
                >>= execErrFin
        execErrFin linkCmd
        exitCode (,x) (0, x) <$> exec "./tmp"
