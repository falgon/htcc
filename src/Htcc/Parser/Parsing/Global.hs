{-|
Module      : Htcc.Parser.Parsing.Global
Description : The C languge parser and AST constructor
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The module of the globals
-}
module Htcc.Parser.Parsing.Global (
    -- * The module of global variable
    module Htcc.Parser.Parsing.Global.Var,
    -- * The module of function declaration
    module Htcc.Parser.Parsing.Global.Function
) where

import Htcc.Parser.Parsing.Global.Var
import Htcc.Parser.Parsing.Global.Function
