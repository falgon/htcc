{-|
Module      : Htcc.Parser.Combinators.Type
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.Type (
    cType
  , arraySuffix
) where
import           Control.Monad.Combinators        (choice)
import           Data.Bits                        (Bits (..))
import           Data.Bool                        (bool)
import qualified Data.Text                        as T
import qualified Htcc.CRules.Types                as CT
import           Htcc.Parser.AST.Core             (ATKind (..), ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import {-# SOURCE #-} Htcc.Parser.Combinators.Program  (logicalOr)
import           Htcc.Utils                       (toNatural)
import qualified Text.Megaparsec                  as M

constantExp :: (Bits i, Integral i, Show i, Read i) => Parser i i
constantExp = logicalOr >>= constantExp'
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
            ATOr -> binop ((.) fromBool . flip (.) toBool . (||) . toBool)
            ATNum v -> pure v
            _ -> fail "The expression is not constant-expression"
            where
                binop f = constantExp' lhs
                    >>= \lhs' -> fromIntegral . f lhs' <$> constantExp' rhs
        constantExp' ATEmpty = fail "The expression is not constant-expression"

arraySuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
arraySuffix ty = choice
    [ withConstantExp
    ]
    where
        withConstantExp = do
            val <- M.try (brackets constantExp)
            M.option (CT.mapTypeKind (CT.CTArray (toNatural val)) ty) $ do
                ty' <- arraySuffix ty
                case CT.concatCTArray (CT.mapTypeKind (CT.CTArray (toNatural val)) ty) ty' of
                    Nothing -> fail $ show ty'
                    Just ty''
                        | CT.isValidIncomplete ty'' -> pure ty''
                        | otherwise -> fail $ show ty'

preType,
    cType :: (Show i, Read i, Integral i) => Parser i (CT.StorageClass i)

preType = choice
    [ kStatic   >> (CT.SCStatic . CT.toTypeKind <$> preType)
    , kRegister >> (CT.SCRegister . CT.toTypeKind <$> preType)
    , kAuto     >> preType
    , CT.SCAuto . CT.toTypeKind . CT.implicitInt . read' . T.unpack
        <$> choice kBasicTypes
    ]
    where
        read' :: (Show i, Read i, Integral i)
            => String
            -> CT.TypeKind i
        read' = read

cType = do
    pt <- preType
    fn <- CT.ctorPtr . toNatural . length <$> M.many (symbol "*")
    pure $ fn pt
