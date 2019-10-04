module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetNewlineMode, stdin, universalNewlineMode)
import Data.Bool (bool)
import qualified Data.Text.IO as T
import Data.Tuple.Extra (second, dupe)

import Htcc.Asm.Generate (casm)

checkArgs :: IO (Maybe [String])
checkArgs = uncurry (bool Nothing . Just) . second (not . null) . dupe <$> getArgs

maybeExit :: Maybe a -> IO a
maybeExit = maybe exitFailure return

main :: IO ()
main = hSetNewlineMode stdin universalNewlineMode >> checkArgs >>= maybeExit >>= T.readFile . head >>= casm
