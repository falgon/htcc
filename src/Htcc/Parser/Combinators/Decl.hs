{-|
Module      : Htcc.Parser.Combinators.Decl
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
module Htcc.Parser.Combinators.Decl (
    module Htcc.Parser.Combinators.Decl.AbstractDeclarator
  , module Htcc.Parser.Combinators.Decl.Spec
  , module Htcc.Parser.Combinators.Decl.Declarator
) where

import           Htcc.Parser.Combinators.Decl.AbstractDeclarator
import           Htcc.Parser.Combinators.Decl.Declarator
import           Htcc.Parser.Combinators.Decl.Spec
