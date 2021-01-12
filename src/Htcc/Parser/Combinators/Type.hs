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
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Htcc.Parser.Combinators.Type (
    constantExp
  , cType
  , arraySuffix
) where
import           Control.Monad                     (mfilter)
import           Control.Monad.Combinators         (choice)
import           Control.Monad.Trans               (MonadTrans (..))
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)
import           Control.Monad.Trans.State         (gets)
import           Data.Bits                         (Bits (..))
import           Data.Bool                         (bool)
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import           Data.Tuple.Extra                  (dupe, first)
import qualified Htcc.CRules.Types                 as CT
import           Htcc.Parser.AST.Core              (ATKind (..), ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Keywords
import {-# SOURCE #-} Htcc.Parser.Combinators.Program   (conditional)
import           Htcc.Parser.ConstructionData.Core (incomplete)
import           Htcc.Utils                        (toNatural)
import qualified Text.Megaparsec                   as M

constantExp :: (Bits i, Integral i, Show i, Read i) => Parser i i
constantExp = conditional >>= constantExp'
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

arraySuffix :: (Show i, Read i, Bits i, Integral i)
    => CT.StorageClass i
    -> Parser i (CT.StorageClass i)
arraySuffix ty = choice
    [ withConstantExp
    , nonConstantExp
    ]
    where
        failWithTypeMaybe ty' = maybe (fail $ show ty') pure

        withConstantExp = do
            arty <- flip id ty . CT.mapTypeKind . CT.CTArray . toNatural <$> M.try (brackets constantExp)
            M.option Nothing (Just <$> arraySuffix ty)
                >>= \case
                    Nothing -> pure arty
                    Just ty' ->
                        runMaybeT (mfilter CT.isValidIncomplete $ MaybeT $ pure $ CT.concatCTArray arty ty')
                            >>= failWithTypeMaybe ty'

        nonConstantExp = let mtIncomplete ty' = MaybeT $ lift $ gets $ incomplete ty' in
            symbol "["
                *> symbol "]"
                *> M.option Nothing (Just <$> arraySuffix ty)
                >>= \case
                    Nothing ->
                        runMaybeT (CT.mapTypeKind (CT.CTIncomplete . CT.IncompleteArray) <$> mtIncomplete ty)
                            >>= failWithTypeMaybe ty
                    Just ty' ->
                        runMaybeT (multiple <$> mtIncomplete ty')
                            >>= failWithTypeMaybe ty'
            where
                multiple = CT.mapTypeKind $
                    uncurry ((.) fromJust . CT.concatCTArray)
                        . first (CT.CTIncomplete . CT.IncompleteArray . CT.removeAllExtents)
                        . dupe

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
