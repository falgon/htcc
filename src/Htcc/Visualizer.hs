{-|
Module      : Htcc.Visualizer
Description : Build AST from C source code
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Build AST from C source code
-}
module Htcc.Visualizer (
    validateVisualizationOutputPath,
    writeVisualization,
    module Htcc.Visualizer.Core
) where

import           Data.Char            (toLower)
import           Diagrams.Prelude     (V2)
import           Diagrams.Size        (SizeSpec)
import           Htcc.Output          (ReplacementOutputMode (..),
                                       resolveReplacementOutputPath,
                                       withReplacementOutputPath)
import           Htcc.Parser          (ASTs)
import           Htcc.Visualizer.Core
import           System.FilePath      (takeExtension)

validateVisualizationOutputPath :: FilePath -> Either String ()
validateVisualizationOutputPath outputPath
    | map toLower (takeExtension outputPath) == ".svg" =
        Right ()
    | otherwise =
        Left $
            "AST visualization output path must use the .svg extension: "
                <> outputPath

writeVisualization :: Show i => ASTs i -> SizeSpec V2 Double -> FilePath -> IO ()
writeVisualization asts sizeSpec outputPath = do
    resolvedOutputPath <- resolveReplacementOutputPath outputPath
    either (ioError . userError) pure $
        validateVisualizationOutputPath resolvedOutputPath
    if null renderableAsts
        then ioError $ userError "There is nothing to describe"
        else
            withReplacementOutputPath PreserveReplacementOutputMode outputPath $
                visualize renderableAsts sizeSpec
    where
        renderableAsts = filter hasRenderableTree asts
