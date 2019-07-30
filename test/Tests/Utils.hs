{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils (
    runTests,
    runTestsEx,
    Test (..),
    (~:),
    (~?=),
    exitCode,
    exec,
    execStdOut,
    execErrFin,
    clean
) where

import Control.Monad (void, zipWithM)
import Control.Exception (bracket)
import qualified Control.Foldl as F
import Data.Bool (bool)
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Text as DT
import System.IO (stderr)
import System.Directory (doesFileExist, removeFile)
import Test.HUnit (Test (..), runTestText, putTextToHandle, (~:), (~?=))
import qualified Turtle as T

runTests :: Test -> IO ()
runTests ts = bracket getCurrentTime (\st -> getCurrentTime >>= putStrLn . ("Process time: " ++) . show . flip diffUTCTime st) $ const $ void $ runTestText (putTextToHandle stderr False) ts

exitCode :: (Int -> a) -> a -> T.ExitCode -> a
exitCode _ x T.ExitSuccess = x
exitCode f _ (T.ExitFailure n) = f n

exec :: T.MonadIO m => DT.Text -> m T.ExitCode
exec = flip T.shell T.empty

execStdOut :: T.MonadIO m => DT.Text -> m (Maybe T.Text)
execStdOut cmd = fmap T.lineToText <$> T.fold (T.inshell cmd T.empty) F.head

execErrFin :: T.MonadIO m => DT.Text -> m ()
execErrFin cmd = T.shell cmd T.empty >>= exitCode (\x -> void $ T.die (cmd <> " failed with exit code: " <> T.repr x)) (return ()) 

runTestsEx :: (Eq a, Show a) => [(IO a, a)] -> IO () 
runTestsEx ts = putStrLn "\n\n== Unit Tests started ==" >> zipWithM (\(t, e) i -> (~:) ("test: " ++ show i) . (~?= e) <$> t) ts ms >>= runTests . TestList
    where
        ms = take (length ts) $ iterate (+1) (1 :: Int)

clean :: [String] -> IO ()
clean = mapM_ (\x -> doesFileExist x >>= bool (return ()) (removeFile x))
