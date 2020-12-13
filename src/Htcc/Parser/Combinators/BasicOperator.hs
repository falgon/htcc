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
  , binOpBool
  , binOpCon
  , binOpIntOnly
) where

import           Control.Applicative          (Alternative (..))
import           Control.Monad.Combinators    (choice)
import           Control.Monad.Fix            (fix)
import           Data.Bits                    (Bits (..))
import           Htcc.CRules.Types            as CT
import           Htcc.Parser.AST.Core         (ATKind (..), ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Utils                   (lor)
import qualified Text.Megaparsec              as M

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
        M.option nd $ choice [M.try (opM >> p) >>= ndM nd >>= f | (opM, ndM) <- opndMs]

binOpBool, binOpCon :: (Monad m, Ord i, Bits i, Show i)
    => ATKind i
    -> ATree i
    -> ATree i
    -> m (ATree i)
binOpBool k lhs rhs = return $ ATNode k (CT.SCAuto CT.CTBool) lhs rhs
binOpCon k lhs rhs = return $ ATNode k (CT.conversion (atype lhs) (atype rhs)) lhs rhs

binOpIntOnly :: (Monad m, Alternative m, Ord i, Bits i, Show i)
    => ATKind i
    -> ATree i
    -> ATree i
    -> m (ATree i)
binOpIntOnly k lhs rhs
    | lor [CT.isIntegral, (CT.CTBool==) . CT.toTypeKind] (atype lhs) &&
        lor [CT.isIntegral, (CT.CTBool ==) . CT.toTypeKind] (atype rhs) =
            return $ ATNode k (CT.SCAuto $ CT.CTLong CT.CTInt) lhs rhs
    | otherwise = empty
