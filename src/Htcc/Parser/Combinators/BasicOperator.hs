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
  , notFollowedOp
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
    flip fix m $ \f nd ->
        M.option nd $ choice [M.try (opM >> p) >>= ndM nd >>= f | (opM, ndM) <- opndMs]

isFunctionType :: CT.StorageClass i -> Bool
isFunctionType ty = case CT.toTypeKind ty of
    CT.CTFunc _ _ -> True
    _             -> False

binOpBool :: (Monad m, Ord i, Bits i, Show i)
    => ATKind i
    -> ATree i
    -> ATree i
    -> m (ATree i)
binOpBool k lhs rhs = return $ ATNode k (CT.SCAuto CT.CTBool) lhs rhs

binOpCon :: (MonadFail m, Ord i, Bits i, Show i)
    => ATKind i
    -> ATree i
    -> ATree i
    -> m (ATree i)
binOpCon k lhs rhs
    | isFunctionOperand lhs || isFunctionOperand rhs = fail $ mconcat
        [ "invalid operands of types '"
        , show (atype lhs)
        , "' and '"
        , show (atype rhs)
        , "' to binary '"
        , show k
        , "'"
        ]
    | otherwise = pure $ ATNode k (CT.conversion (atype lhs) (atype rhs)) lhs rhs
    where
        isFunctionOperand expr =
            isFunctionType (atype expr) || carriesUncastFunctionDesignatorValue expr

        carriesUncastFunctionDesignatorValue expr = case expr of
            ATNode (ATFuncPtr _) _ _ _ ->
                True
            ATNode ATAddr _ inner _ ->
                carriesUncastFunctionDesignatorValue inner
            ATNode ATCast ty inner _
                | CT.isIntegral ty ->
                    False
                | otherwise ->
                    carriesUncastFunctionDesignatorValue inner
            ATNode (ATNull inner) _ _ _ ->
                carriesUncastFunctionDesignatorValue inner
            ATNode ATExprStmt _ inner _ ->
                carriesUncastFunctionDesignatorValue inner
            ATNode ATComma _ _ rhs ->
                carriesUncastFunctionDesignatorValue rhs
            ATNode (ATConditional cond ATEmpty el) _ _ _ ->
                any carriesUncastFunctionDesignatorValue [cond, el]
            ATNode (ATConditional _ th el) _ _ _ ->
                any carriesUncastFunctionDesignatorValue [th, el]
            ATNode (ATStmtExpr stmts) _ _ _ ->
                maybe False carriesUncastFunctionDesignatorValue (lastMaybe stmts)
            _ ->
                False

        lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

binOpIntOnly :: (Monad m, MonadFail m, Alternative m, Ord i, Bits i, Show i)
    => ATKind i
    -> ATree i
    -> ATree i
    -> m (ATree i)
binOpIntOnly k lhs rhs
    | lor [CT.isIntegral, (CT.CTBool==) . CT.toTypeKind] (atype lhs) &&
        lor [CT.isIntegral, (CT.CTBool ==) . CT.toTypeKind] (atype rhs) =
            return $ ATNode k (resultTy k (atype lhs) (atype rhs)) lhs rhs
    | otherwise = fail $ mconcat
            [ "invalid operands of types '"
            , show (atype lhs)
            , "' and '"
            , show (atype rhs)
            , "' to binary '"
            , show k
            , "'"
            ]
    where
        resultTy ATShl lhsTy _ = CT.SCAuto $ CT.integerPromotedTypeKind $ CT.toTypeKind lhsTy
        resultTy ATShr lhsTy _ = CT.SCAuto $ CT.integerPromotedTypeKind $ CT.toTypeKind lhsTy
        resultTy _ lhsTy rhsTy = CT.conversion lhsTy rhsTy

notFollowedOp :: Parser i a -> Parser i b -> Parser i a
notFollowedOp op nop = M.try $ lexeme $ op `notFollowedBy` nop
