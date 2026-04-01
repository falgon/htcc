{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Tests.SubProcTests.LinkFuncStdOut (
    test
) where

import           Control.Exception (finally)
import           Control.Monad     (forM_)
import qualified Data.Text         as T
import           Tests.Utils

-- | `test` performs a test by comparison with the standard output string.
test :: String -> [String] -> IO (Either T.Text T.Text, String)
test x fnames = let obj = map (++".o") fnames in
    flip finally (clean $ ["tmp", "tmp.s"] ++ obj) $ do
        htccCmd <- htccCommand
        linkCmd <- assemblerCommand $ obj ++ ["tmp.s", "-o", "tmp"]
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
        maybe (Left "The command did not execute successfully", x) ((, x) . Right)
            <$> execStdOut "./tmp"
