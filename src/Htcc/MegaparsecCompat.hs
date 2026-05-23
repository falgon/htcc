module Htcc.MegaparsecCompat (
    module Text.Megaparsec,
    errorBundlePretty
) where

import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import qualified Data.Text           as T
import qualified Text.Megaparsec     as M
import           Text.Megaparsec     hiding (errorBundlePretty)
import qualified Text.Megaparsec.Pos as MP

errorBundlePretty :: M.ShowErrorComponent e => M.ParseErrorBundle T.Text e -> String
errorBundlePretty bundle =
    intercalate "\n\n" (renderError <$> toList (M.bundleErrors bundle)) <> "\n"
    where
        basePosState = M.bundlePosState bundle

        renderError err =
            renderAt (errorSourcePos err) $ lines $ M.parseErrorTextPretty err

        errorSourcePos err
            | M.errorOffset err <= M.pstateOffset basePosState =
                M.pstateSourcePos basePosState
            | otherwise =
                M.pstateSourcePos $ snd $ M.reachOffset (M.errorOffset err) basePosState

        renderAt pos msgs =
            intercalate "\n" $
                [ renderLoc pos ]
                    <> maybe [] (\(srcLn, caretCol) -> [srcLn, replicate (pred caretCol) ' ' <> "^"]) (sourceLineAt pos)
                    <> msgs

        renderLoc pos =
            intercalate
                ":"
                [ MP.sourceName pos
                , show $ MP.unPos $ MP.sourceLine pos
                , show $ MP.unPos $ MP.sourceColumn pos
                ]

        sourceLineAt pos =
            let lineNo = MP.unPos (MP.sourceLine pos) - 1
                inputLines = T.splitOn (T.singleton '\n') $ M.pstateInput basePosState
             in if lineNo < 0
                    then Nothing
                    else case drop lineNo inputLines of
                            srcLn : _ -> Just $ truncateSourceLine srcLn (MP.unPos $ MP.sourceColumn pos)
                            []        -> Nothing

        truncateSourceLine srcLine caretCol
            | T.length srcLine <= maxSourceLineWidth =
                (T.unpack srcLine, caretCol)
            | caretCol <= edgeContextWidth =
                (T.unpack (T.take (maxSourceLineWidth - suffixWidth) srcLine) <> truncationSuffix, caretCol)
            | otherwise =
                let startCol = max 1 (caretCol - innerContextWidth)
                    shown = T.take (maxSourceLineWidth - prefixWidth - suffixWidth) $ T.drop (pred startCol) srcLine
                    suffix =
                        if T.length srcLine > pred startCol + T.length shown
                            then truncationSuffix
                            else ""
                    adjustedCaretCol = prefixWidth + caretCol - startCol + 1
                 in (truncationPrefix <> T.unpack shown <> suffix, adjustedCaretCol)
            where
                maxSourceLineWidth = 160
                edgeContextWidth = 120
                innerContextWidth = 80
                truncationPrefix = "... "
                truncationSuffix = " ..."
                prefixWidth = length truncationPrefix
                suffixWidth = length truncationSuffix
