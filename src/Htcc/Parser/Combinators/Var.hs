{-|
Module      : Htcc.Parser.Combinators.Var
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.Var (
    varInit
) where
import           Control.Monad                 (foldM, forM, void, (>=>))
import           Control.Monad.Extra           (andM)
import           Control.Monad.Fix             (fix)
import           Control.Monad.Trans           (MonadTrans (..))
import           Control.Monad.Trans.State     (gets)
import           Data.Bits                     (Bits)
import           Data.Bool                     (bool)
import           Data.Foldable                 (toList)
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (fromJust, fromMaybe)
import qualified Data.Sequence                 as SQ
import qualified Data.Text                     as T
import qualified Htcc.CRules.Types             as CT
import           Htcc.Parser.AST               (ATKind (..), ATree (..),
                                                addKind, atAssign, atBlock,
                                                atExprStmt, atMemberAcc,
                                                atNumLit, atUnary, treealize)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Utils (bracket, maybeToParser,
                                                registerLVar)
import           Htcc.Parser.ConstructionData  (incomplete, lookupLVar)
import           Htcc.Utils                    (tshow)
import qualified Text.Megaparsec               as M

import           Text.Megaparsec.Debug         (dbg)

fromValidAssignAST :: Eq i => ATree i -> Parser i (ATree i)
fromValidAssignAST at@(ATNode _ ty _ _)
    | CT.toTypeKind ty == CT.CTVoid = fail "void value not ignored as it ought to be"
    | otherwise = pure at
fromValidAssignAST _ = fail "expected to assign"

lengthArrayBrace :: Parser i Int
lengthArrayBrace = braces (arrayBrace 0)
    where
        arrayBrace c = M.choice
            [ (+) <$> M.try (acc c <$> braces (arrayBrace (succ c)) <* comma) <*> arrayBrace c
            , acc c <$> braces (arrayBrace $ succ c)
            , (+) <$> M.try (accN c <$ validCharSets <* comma) <*> arrayBrace c
            , accN c <$ validCharSets <* M.lookAhead rbrace
            , 0 <$ M.lookAhead rbrace
            ]
        acc n | n == 0 = succ | otherwise = id
        accN n | n == 0 = 1 | otherwise = 0
        validCharSets = M.choice
            [ identifier
            , T.pack <$> stringLiteral
            , T.singleton <$> charLiteral
            , tshow <$> integer
            , semi
            , comma
            , colon
            , lnot
            , sharp
            , ampersand
            , lparen
            , rparen
            , langle
            , rangle
            , lbracket
            , rbracket
            , star
            , period
            , slash
            , equal
            , question
            , hat
            , tilda
            , vertical
            , percent
            ]

desgNode :: (Num i, Ord i, Show i)
    => T.Text
    -> ATree i
    -> SQ.Seq (CT.Desg i)
    -> Parser i (ATree i)
desgNode ident nd desg = fmap (atExprStmt . flip atAssign nd) $
    flip (`foldr` (treealize <$> (maybeToParser' =<< lift (gets $ lookupLVar ident)))) desg $ \idx acc -> case idx of
        CT.DesgIdx idx' -> do
            at <- acc
            nd' <- maybeToParser' $ addKind at $ atNumLit idx'
            flip (atUnary ATDeref) nd' <$> maybeToParser' (CT.deref (atype nd'))
        CT.DesgMem mem -> atMemberAcc mem <$> acc
    where
        maybeToParser' = maybeToParser "invalid initializer-list"

initLoop :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> Parser i (SQ.Seq (ATree i), i)
initLoop p ty ident ai desg = initLoop' ai <* rbrace
    where
        initLoop' ai' = case CT.toTypeKind ty of
            CT.CTArray _ _ -> ($ (0, ai')) . fix $ \f (idx, rl) -> do
                rs <- desgInit p (fromJust $ CT.deref ty) ident rl (CT.DesgIdx idx SQ.<| desg)
                M.choice
                    [ (rs, succ idx) <$ M.lookAhead rbrace
                    , comma *> f (succ idx, rs)
                    ]
            _ -> fail "internal compiler error"

initZero :: (Num i, Ord i, Show i, Enum i)
    => CT.TypeKind i
    -> T.Text
    -> SQ.Seq (CT.Desg i)
    -> Parser i (SQ.Seq (ATree i))
initZero (CT.CTArray n ty) ident desg =
    foldM
        (\acc idx -> (SQ.>< acc) <$> initZero ty ident (CT.DesgIdx idx SQ.<| desg))
        SQ.empty
        [0..fromIntegral (pred n)]
initZero _ ident desg = SQ.singleton <$> desgNode ident (atNumLit 0) desg

initializerList :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> Parser i (SQ.Seq (ATree i))
initializerList p ty ident ai desg = M.choice
    [ allZeroInit
    , withInitElements
    ]
    where
        allZeroInit = do
            void $ M.try (lbrace *> rbrace)
            (ai SQ.><) . SQ.fromList <$> forM
                (CT.accessibleIndices $ CT.toTypeKind ty)
                (desgNode ident (atNumLit 0) . (SQ.>< desg) . SQ.fromList)

        withInitElements
            | CT.isIncompleteArray ty = do
                newt <- bracket M.getInput M.setInput (const $ arType <$> lengthArrayBrace)
                registerLVar newt ident *> desgInit p newt ident ai desg
            | otherwise = do
                void lbrace
                case CT.toTypeKind ty of
                    CT.CTArray n bt -> do
                        (ast, idx) <- initLoop p ty ident ai desg
                        (ai SQ.><) . (ast SQ.><)
                            <$> foldM
                                (\acc idx' -> (SQ.>< acc) <$> initZero bt ident (CT.DesgIdx idx' SQ.<| desg))
                                SQ.empty
                                [fromIntegral idx..pred (fromIntegral n)]
                    _ -> fail "internal compiler error"
            where
                arType len = snd (CT.dctorArray ty) $
                    CT.mapTypeKind (CT.CTArray (fromIntegral len) . fromJust . CT.fromIncompleteArray) ty

desgInit :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> Parser i (SQ.Seq (ATree i))
desgInit p ty ident ai desg = M.choice
    [ ai <$ lookInitializerString
    , lookInitializerList *> initializerList p ty ident ai desg
    , ai <$ lookStructInit
    , p >>= (flip (desgNode ident) desg >=> pure . (SQ.|>) ai)
    ]
    where
        lookInitializerString = bool M.empty (pure ()) =<< andM
            [ pure $ CT.isArray ty
            , pure $ maybe False ((==CT.CTChar) . CT.toTypeKind) (CT.deref ty)
            , M.option False (True <$ M.lookAhead stringLiteral)
            ]
        lookInitializerList = bool M.empty (pure ()) $ CT.isArray ty
        lookStructInit = bool M.empty (pure ()) $ CT.isCTStruct ty

varInit' :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> ATree i
    -> Parser i (ATree i)
varInit' p ty ident lat
    | CT.isArray ty || CT.isCTStruct ty = atBlock . toList <$> desgInit p ty ident SQ.empty SQ.empty
    | otherwise = p >>= fromValidAssignAST <&> atExprStmt . ATNode ATAssign (atype lat) lat

varInit :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
varInit p ty ident = fromMaybe ty <$> lift (gets $ incomplete ty)
    >>= flip registerLVar ident
    >>= varInit' p ty ident
