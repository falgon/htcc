{-|
Module      : Htcc.Parser.Combinators.BasicOperator
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
module Htcc.Parser.Combinators.BasicOperator (
    binaryOperator
) where

import           Control.Applicative          ((<|>))
import           Control.Monad.Combinators    (choice)
import           Control.Monad.Fix            (fix)
import           Htcc.Parser.AST.Core         (ATree (..))
import           Htcc.Parser.Combinators.Core
import qualified Text.Megaparsec as M

-- | A parser combinator that builds a parser for basic binary operators. 
-- This is useful for syntax such as:
-- \\[X::=X'\left("\text{op}_1"\ X'\ \mid\ "\text{op}_2"\ X'\ \mid\cdots\right)\ast\\]
binaryOperator ::
    Parser i (ATree i)
    -> [(Parser i a, ATree i -> ATree i -> Parser i (ATree i))]
    -> Parser i (ATree i)
binaryOperator p opndMs = do
    m <- p
    ($ m) . fix $ \f nd ->
        choice [M.try (opM >> p) >>= ndM nd >>= f | (opM, ndM) <- opndMs]
            <|> return nd
