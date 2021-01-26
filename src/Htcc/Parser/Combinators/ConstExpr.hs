{-|
Module      : Htcc.Parser.Combinators.ConstExpr
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Htcc.Parser.Combinators.ConstExpr (
    evalConstexpr
) where
import           Data.Bits                               (Bits (..))
import           Data.Bool                               (bool)
import           Htcc.Parser.AST.Core                    (ATKind (..),
                                                          ATree (..))
import           Htcc.Parser.Combinators.Core
import {-# SOURCE #-} Htcc.Parser.Combinators.Program         (conditional)

evalConstexpr :: (Bits i, Integral i, Show i, Read i) => Parser i i
evalConstexpr = conditional >>= constantExp'
    where
        fromBool = fromIntegral . fromEnum :: Num i => Bool -> i
        toBool x | x == 0 = False | otherwise = True

        constantExp' (ATNode k _ lhs rhs) = case k of
            ATAdd -> binop (+)
            ATSub -> binop (-)
            ATMul -> binop (*)
            ATDiv -> binop div
            ATAnd -> binop (.&.)
            ATXor -> binop xor
            ATOr -> binop (.|.)
            ATShl -> binop (flip (.) fromIntegral . shiftL)
            ATShr -> binop (flip (.) fromIntegral . shiftR)
            ATEQ -> binop ((.) fromBool . (==))
            ATNEQ -> binop ((.) fromBool . (/=))
            ATLT -> binop ((.) fromBool . (<))
            ATGT -> binop ((.) fromBool . (>))
            ATLEQ -> binop ((.) fromBool . (<=))
            ATGEQ -> binop ((.) fromBool . (>=))
            ATConditional cn th el -> constantExp' cn
                >>= bool (constantExp' el) (constantExp' th) . toBool
            ATComma -> constantExp' rhs
            ATNot -> fromIntegral . fromEnum . not . toBool <$> constantExp' lhs
            ATBitNot -> complement <$> constantExp' lhs
            ATLAnd -> binop ((.) fromBool . flip (.) toBool . (&&) . toBool)
            ATLOr -> binop ((.) fromBool . flip (.) toBool . (||) . toBool)
            ATNum v -> pure v
            _ -> fail "The expression is not constant-expression"
            where
                binop f = constantExp' lhs
                    >>= \lhs' -> fromIntegral . f lhs' <$> constantExp' rhs
        constantExp' ATEmpty = fail "The expression is not constant-expression"

