{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Maybe (isJust, isNothing)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (both, dupe)
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import Options.Applicative
import Diagrams.TwoD.Size (mkSizeSpec2D)

import Htcc.Asm (casm)
import Htcc.Utils (putStrLnErr, tshow)
import Htcc.Visualizer (visualize)

data Options = Options 
    { visualizeAST :: Bool
    , resolution :: String
    , inputFName :: FilePath
    , outputFName :: FilePath
    , supressWarn :: Bool
    } deriving Show

visualizeASTP :: Parser Bool
visualizeASTP = switch $ mconcat
    [ long "visualize-ast"
    , help "Visualize an AST built from source code"
    ]

resolutionP :: Parser String
resolutionP = strOption $ mconcat
    [ metavar "RESOLUTION"
    , long "img-resolution"
    , help "Specify the resolution of the AST graph to be generated"
    , value "640x480"
    , showDefaultWith id
    ]

inputFNameP :: Parser FilePath
inputFNameP = strArgument $ mconcat
    [ metavar "file"
    , action "file"
    , help "Specify the input file name"
    ]

outputFNameP :: Parser FilePath
outputFNameP = strOption $ mconcat
    [ metavar "file"
    , short 'o'
    , long "out"
    , help "Specify the output destination file name, supported only svg"
    , value "./out.svg"
    , showDefaultWith id
    ]

supressWarnP :: Parser Bool
supressWarnP = switch $ mconcat 
    [ short 'w'
    , long "supress-warns"
    , help "Disable all warning messages"
    ]

optionsP :: Parser Options
optionsP = (<*>) helper $
    Options <$> visualizeASTP <*> resolutionP <*> inputFNameP <*> outputFNameP <*> supressWarnP

parseResolution :: (Num a, Read a) => String -> (Maybe a, Maybe a)
parseResolution xs = let rs = splitOn "x" xs in if length rs /= 2 then dupe Nothing else
    let rs' = map readMaybe rs in if any isNothing rs' then dupe Nothing else (head rs', rs' !! 1)

unlessVis :: Monad m => Options -> m Options -> m Options
unlessVis ops f
    | visualizeAST ops = return ops
    | otherwise = f

whenVis :: Monad m => Options -> m Options -> m Options
whenVis ops f
    | visualizeAST ops = f
    | otherwise = return ops

execVisualize :: Options -> IO Options
execVisualize ops = whenVis ops $ let rlt = parseResolution $ resolution ops in do
    rs <- if uncurry (&&) (both isJust rlt) then return rlt else
        (Just 640, Just 480) <$ putStrLnErr "warning: the specified resolution is invalid, so using default resolution."
    ops <$ (T.readFile (inputFName ops) >>= visualize (supressWarn ops) (uncurry mkSizeSpec2D rs) (outputFName ops))

execCompile :: Options -> IO Options
execCompile ops = unlessVis ops $ do
    b <- doesFileExist (inputFName ops)
    (<$) ops $ if b then T.readFile (inputFName ops) >>= casm (supressWarn ops) else
        putStrLnErr ("error: " <> tshow (inputFName ops) <> ": No such file or directory.\ncompilation terminated.") >> 
            exitFailure

main :: IO ()
main = execParser (info optionsP fullDesc) >>= execVisualize >>= void . execCompile
