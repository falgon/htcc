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

import qualified Control.Foldl            as F
import           Control.Monad            (void, when, zipWithM)
import           Data.Bool                (bool)
import qualified Data.Text                as DT
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           removeDirectoryRecursive, removeFile)
import           Test.Hspec               (parallel)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.Hspec.Core.Runner   (Config (..), defaultConfig,
                                           evaluateSummary, runSpec)
import           Test.HUnit               (Test (..), (~:), (~?=))
import qualified Turtle                   as T

{-# INLINE cfg #-}
cfg :: Config
cfg = defaultConfig { configPrintCpuTime = True }

runTests :: Test -> IO ()
runTests ts = runSpec (parallel $ fromHUnitTest ts) cfg >>= evaluateSummary

exitCode :: (Int -> a) -> a -> T.ExitCode -> a
exitCode _ x T.ExitSuccess     = x
exitCode f _ (T.ExitFailure n) = f n

exec :: T.MonadIO m => DT.Text -> m T.ExitCode
exec = flip T.shell T.empty

execStdOut :: T.MonadIO m => DT.Text -> m (Maybe T.Text)
execStdOut cmd = fmap T.lineToText <$> T.fold (T.inshell cmd T.empty) F.head

execErrFin :: T.MonadIO m => DT.Text -> m ()
execErrFin cmd = T.shell cmd T.empty >>= exitCode (\x -> void $ T.die (cmd <> " failed with exit code: " <> T.repr x)) (return ())

runTestsEx :: (Eq a, Show a) => [(IO (a, String), a)] -> IO ()
runTestsEx ts = putStrLn "\n\n== Unit Tests started ==" >> zipWithM (\(t, e) i -> fmap (\(ec, t') -> (~:) ("test: #" ++ show i ++ ": " ++ t' ++ "\"") $ (~?= e) ec) t) ts ms >>= runTests . TestList
    where
        ms = take (length ts) $ iterate (+1) (1 :: Int)

clean :: [FilePath] -> IO ()
clean = mapM_ $ \x -> (>>=) (doesFileExist x) $ flip bool (removeFile x) $
    doesDirectoryExist x >>= flip when (removeDirectoryRecursive x)
