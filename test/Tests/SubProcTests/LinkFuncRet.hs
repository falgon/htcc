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
        execErrFin $ mconcat
            [ "echo \'"
            , T.pack x
            , "\' | stack exec htcc -- /dev/stdin > tmp.s"
            ]
        forM_ fnames $ \fname -> execErrFin $ mconcat
            [ "cc -c test/Tests/csrc/externals/"
            , T.pack fname
            , ".c"
            ]
        execErrFin $ mconcat
            [ "gcc "
            , T.pack (unwords obj)
            , " tmp.s -o tmp"
            ]
        exitCode (,x) (0, x) <$> exec "./tmp"
