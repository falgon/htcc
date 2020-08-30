{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Conditional                    (ifM)
import           Data.Bool                              (bool)
import           Data.List.Split                        (splitOn)
import           Data.Maybe                             (isJust, isNothing)
import qualified Data.Text.IO                           as T
import           Data.Tuple.Extra                       (both, dupe, fst3)
import           Diagrams.TwoD.Size                     (mkSizeSpec2D)
import           Options.Applicative
import           System.Directory                       (doesFileExist)
import           System.Exit                            (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen           (char, linebreak, text,
                                                         (<+>))
import           Text.Read                              (readMaybe)

import           Htcc.Asm                               (InputCCode, casm,
                                                         execAST)
import           Htcc.Parser                            (ASTs)
import           Htcc.Parser.ConstructionData.Scope.Var (GlobalVars, Literals)
import           Htcc.Utils                             (errTxtDoc, locTxtDoc,
                                                         putDocLnErr,
                                                         putStrLnErr)
import           Htcc.Visualizer                        (visualize)

data Options = Options
    { visualizeAST :: Bool
    , resolution   :: String
    , inputFName   :: FilePath
    , outputFName  :: FilePath
    , supressWarn  :: Bool
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

execVisualize :: Show i => Options -> ASTs i -> IO ()
execVisualize ops ast = let rlt = parseResolution $ resolution ops in do
    rs <- if uncurry (&&) (both isJust rlt) then return rlt else
        (Just 640, Just 480) <$ putStrLnErr "warning: the specified resolution is invalid, so using default resolution."
    visualize ast (uncurry mkSizeSpec2D rs) (outputFName ops)

main :: IO ()
main = do
    ops <- execParser $ info optionsP fullDesc
    ifM (not <$> doesFileExist (inputFName ops)) (notFould (inputFName ops) >> exitFailure) $
        T.readFile (inputFName ops) >>= execAST' (supressWarn ops) (inputFName ops) >>= maybe (return ()) (bool casm (execVisualize ops . fst3) (visualizeAST ops))
    where
        execAST' :: Bool -> FilePath -> InputCCode -> IO (Maybe (ASTs Integer, GlobalVars Integer, Literals Integer))
        execAST' = execAST
        notFould fpath = putDocLnErr $
            locTxtDoc "htcc:" <+>
            errTxtDoc "error:" <+>
            text fpath <> char ':' <+>
            text "no such file" <> linebreak <>
            text "compilation terminated."
