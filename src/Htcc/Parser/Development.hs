{-|
Module      : Htcc.Parser.Development
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Development (
    defMainFn
) where

import           Htcc.CRules.Types            as CT
import           Htcc.Parser.AST.Core         (ATree (..),
                                               atDefFunc)

defMainFn :: ATree i -> ATree i
defMainFn = atDefFunc "main" Nothing (CT.SCAuto CT.CTInt)
