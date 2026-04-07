{-|
Module      : Htcc.Utils.Print
Description : Utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Utilities of print
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Htcc.Utils.Print (
    -- * Shortcuts of print
    putStrErr, putStrLnErr, err,
    putDocLn, putDocErr, putDocLnErr,
    errTxtDoc, errCharDoc, warnTxtDoc,
    warnCharDoc, locTxtDoc, locCharDoc,
) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Prelude                       hiding (toInteger)
import qualified Prettyprinter                 as PP
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (Magenta, Red),
                                                bold, color, hPutDoc, putDoc)
import           System.Exit                   (exitFailure)
import           System.IO                     (stderr)

type Doc = PP.Doc AnsiStyle

{-# INLINE putDocLn #-}
-- | Execute `putDoc` by appending a trailing line break to the given `Doc`.
putDocLn :: Doc -> IO ()
putDocLn = putDoc . flip (<>) PP.hardline

{-# INLINE putDocErr #-}
-- | The shortcut of @hPutDoc stderr@
putDocErr :: Doc -> IO ()
putDocErr = hPutDoc stderr

{-# INLINE putDocLnErr #-}
-- | Execute `putDocErr` by appending a trailing line break to the given `Doc`.
putDocLnErr :: Doc -> IO ()
putDocLnErr = putDocErr . flip (<>) PP.hardline

{-# INLINE errTxtDoc #-}
-- | Doc used to output an error message (`String`).
errTxtDoc :: String -> Doc
errTxtDoc = PP.annotate (color Red) . PP.pretty

{-# INLINE errCharDoc #-}
-- | Doc used to output an error message (`Char`).
errCharDoc :: Char -> Doc
errCharDoc = PP.annotate (color Red) . PP.pretty

{-# INLINE warnTxtDoc #-}
-- | Doc used to output a warning message (`String`).
warnTxtDoc :: String -> Doc
warnTxtDoc = PP.annotate (color Magenta) . PP.pretty

{-# INLINE warnCharDoc #-}
-- | Doc used to output a warning message (`Char`).
warnCharDoc :: Char -> Doc
warnCharDoc = PP.annotate (color Magenta) . PP.pretty

{-# INLINE locTxtDoc #-}
-- | Doc used to output a message (`String`) about the location, such as the file name and its location,
-- it is shortcut of @annotate bold . pretty@
locTxtDoc :: String -> Doc
locTxtDoc = PP.annotate bold . PP.pretty

{-# INLINE locCharDoc #-}
-- | Doc used to output a message (`Char`) about the location, such as the file name and its location,
-- it is shortcut of @annotate bold . pretty@
locCharDoc :: Char -> Doc
locCharDoc = PP.annotate bold . PP.pretty

-- | Standard error output shortcut (with new line).
putStrLnErr :: T.Text -> IO ()
putStrLnErr = T.hPutStrLn stderr

-- | Standard error output shortcut.
putStrErr :: T.Text -> IO ()
putStrErr = T.hPutStr stderr

-- | Standard error output and exit shortcut.
err :: T.Text -> IO ()
err = flip (>>) exitFailure . putStrLnErr
