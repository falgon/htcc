{-|
Module      : Htcc.Parser.ConstructionData.Scope.Utils
Description : Utilities used to handle scopes
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Utilities used to handle scopes
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope.Utils (
    internalCE
) where

import qualified Data.Text as T

{-# INLINE internalCE #-}
-- | the message of an internal compiler error
internalCE :: T.Text
internalCE = "internal compiler error: Please submit a bug report with preprocessed source if appropriate.\nPlease see this repository: <URL> "
