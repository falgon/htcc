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

import Prelude hiding (toInteger)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stderr)
import System.Exit (exitFailure)
import Text.PrettyPrint.ANSI.Leijen (Doc, putDoc, hPutDoc, text, char, red, magenta, bold, linebreak)

{-# INLINE putDocLn #-}
-- | Execute `Text.PrettyPrint.ANSI.Leijen.putDoc` by applying `Text.PrettyPrint.ANSI.Leijen.linebreak`
-- to `Text.PrettyPrint.ANSI.Leijen.<>` at the end of given `Text.PrettyPrint.ANSI.Leijen.Doc`
putDocLn :: Doc -> IO ()
putDocLn = putDoc . flip (<>) linebreak

{-# INLINE putDocErr #-}
-- | The shortcut of @hPutDoc stderr@
putDocErr :: Doc -> IO ()
putDocErr = hPutDoc stderr

{-# INLINE putDocLnErr #-}
-- | Execute `putDocErr` by applying `Text.PrettyPrint.ANSI.Leijen.linebreak`
-- to `Text.PrettyPrint.ANSI.Leijen.<>` at the end of given `Text.PrettyPrint.ANSI.Leijen.Doc`
putDocLnErr :: Doc -> IO ()
putDocLnErr = putDocErr . flip (<>) linebreak

{-# INLINE errTxtDoc #-}
-- | The `Text.PrettyPrint.ANSI.Leijen.Doc` used to output an error message (`String`),
-- it is shortcut of @red . text@
errTxtDoc :: String -> Doc
errTxtDoc = red . text

{-# INLINE errCharDoc #-}
-- | The `Text.PrettyPrint.ANSI.Leijen.Doc` used to output an error message (`Char`),
-- it is shortcut of @red. char@
errCharDoc :: Char -> Doc
errCharDoc = red . char

{-# INLINE warnTxtDoc #-}
-- | The `Text.PrettyPrint.ANSI.Leijen.Doc` used to output an warning message (`String`),
-- it is shortcut of @magenta . text@
warnTxtDoc :: String -> Doc
warnTxtDoc = magenta . text

{-# INLINE warnCharDoc #-}
-- | The `Text.PrettyPrint.ANSI.Leijen.Doc` used to output an warning message (`Char`),
-- it is shortcut of @magenta . char@
warnCharDoc :: Char -> Doc
warnCharDoc = magenta . char

{-# INLINE locTxtDoc #-}
-- | Doc used to output a message (`String`) about the location, such as the file name and its location,
-- it is shortcut of @bold . text@
locTxtDoc :: String -> Doc
locTxtDoc = bold . text

{-# INLINE locCharDoc #-}
-- | Doc used to output a message (`Char`) about the location, such as the file name and its location,
-- it is shortcut of @bold . char@
locCharDoc :: Char -> Doc
locCharDoc = bold . char

-- | Standard error output shortcut (with new line).
putStrLnErr :: T.Text -> IO ()
putStrLnErr = T.hPutStrLn stderr

-- | Standard error output shortcut.
putStrErr :: T.Text -> IO ()
putStrErr = T.hPutStr stderr

-- | Standard error output and exit shortcut.
err :: T.Text -> IO ()
err = flip (>>) exitFailure . putStrLnErr
