{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, filterM)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (hSetNewlineMode, stdin, universalNewlineMode)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Tuple.Extra (first, second)

import Htcc.Utils (putStrLnErr, tshow)
import Htcc.Asm (casm)
import Htcc.Visualizer (visualize)

class HelpMessage a where
    helpm :: a -> T.Text

data Options = SupressAllWarnings 
    | VisualizeAST
    deriving (Eq, Ord, Enum)

instance Show Options where
    show SupressAllWarnings = "-w"
    show VisualizeAST = "--visualize-ast"

instance HelpMessage Options where
    helpm SupressAllWarnings = "Disable all warning messages."
    helpm VisualizeAST = "Visualize an AST built from source code. The code is not compiled."

options :: M.Map String Options
options = M.fromList [(show x, x) | x <- [toEnum 0 ..]]

help :: IO ()
help = putStrLnErr "Usage: htcc [options] file...\nOptions:" >> mapM_ (\x -> putStrLnErr $ "\t" <> tshow x <> "\t" <> helpm x) (M.elems options)

splitArgs :: [String] -> ([FilePath], [Options])
splitArgs = foldr (\x acc -> maybe (first (x:) acc) (\y -> second (y:) acc) $ M.lookup x options) ([], [])

main :: IO ()
main = do
    void $ hSetNewlineMode stdin universalNewlineMode
    (fpath, ops) <- splitArgs <$> getArgs
    if null fpath then help >> exitFailure else do
        invalid <- filterM (fmap not . doesFileExist) fpath
        if not (null invalid) then putStrLnErr $ "htcc: error: " <> T.pack (intercalate ", " invalid) <> ": No such file or directory\ncompilation terminated." else 
            T.readFile (head fpath) >>=
                if VisualizeAST `elem` ops then visualize (SupressAllWarnings `elem` ops) else casm (SupressAllWarnings `elem` ops)
