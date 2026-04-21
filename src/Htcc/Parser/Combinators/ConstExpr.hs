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
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.ConstExpr (
    evalConstexpr
) where
import                          Data.Bits                       (Bits (..))
import                          Data.Bool                       (bool)
import                qualified Htcc.CRules.Types               as CT
import                          Htcc.Parser.AST.Core            (ATKind (..),
                                                                 ATree (..))
import                          Htcc.Parser.Combinators.Core
import                          Htcc.Parser.ConstructionData.Core
                                                                 (hasIncompleteObjectType)
import {-# SOURCE #-}           Htcc.Parser.Combinators.Program (conditional)

evalConstexpr :: (Bits i, Integral i, Show i, Read i) => Parser i i
evalConstexpr = conditional >>= constantExp'
    where
        fromBool = fromIntegral . fromEnum :: Num i => Bool -> i
        toBool x | x == 0 = False | otherwise = True

        constantExp' (ATNode k _ lhs rhs) = case k of
            ATAdd -> binop (+)
            ATSub -> binop (-)
            ATMul -> binop (*)
            ATDiv -> nonZeroBinop quot
            ATMod -> nonZeroBinop rem
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
                >>= bool (constantExp' el) (constantExp' trueExpr) . toBool
                where
                    trueExpr = case th of
                        ATEmpty -> cn
                        _       -> th
            ATComma -> fail "The expression is not constant-expression"
            ATNot -> fromIntegral . fromEnum . not . toBool <$> constantExp' lhs
            ATBitNot -> complement <$> constantExp' lhs
            ATLAnd -> constantExp' lhs >>= logicalAnd
            ATLOr -> constantExp' lhs >>= logicalOr
            ATSizeof -> memOp "sizeof" CT.sizeof lhs
            ATAlignof -> memOp "_Alignof" CT.alignof lhs
            ATNum v -> pure v
            _ -> fail "The expression is not constant-expression"
            where
                binop f = constantExp' lhs
                    >>= \lhs' -> fromIntegral . f lhs' <$> constantExp' rhs
                logicalAnd lhs'
                    | not (toBool lhs') = pure $ fromBool False
                    | otherwise = fromBool . toBool <$> constantExp' rhs
                logicalOr lhs'
                    | toBool lhs' = pure $ fromBool True
                    | otherwise = fromBool . toBool <$> constantExp' rhs
                nonZeroBinop f =
                    constantExp' lhs >>= \lhs' ->
                        constantExp' rhs >>= \rhs' ->
                            if rhs' == 0
                                then fail "The expression is not constant-expression"
                                else pure $ fromIntegral $ f lhs' rhs'
                memOp opName op expr
                    | hasIncompleteObjectType (atype expr) =
                        fail $ "invalid application of '" <> opName <> "' to incomplete type"
                    | otherwise =
                        pure $ fromIntegral $ op $ atype expr
        constantExp' ATEmpty = fail "The expression is not constant-expression"
