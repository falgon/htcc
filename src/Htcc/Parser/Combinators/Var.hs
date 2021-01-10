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
import           Control.Monad.Trans.Reader    (ReaderT, asks, runReaderT)
import           Control.Monad.Trans.State     (gets)
import           Data.Bits                     (Bits)
import           Data.Bool                     (bool)
import           Data.Char                     (ord)
import           Data.Foldable                 (toList)
import           Data.Functor                  ((<&>))
import           Data.List                     (sortBy)
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

type DesignatorParser i r = ReaderT (T.Text, Parser i (ATree i)) (Parser i) r

runDesignator :: (SQ.Seq (ATree i) -> SQ.Seq (CT.Desg i) -> DesignatorParser i r)
    -> T.Text
    -> Parser i (ATree i)
    -> Parser i r
runDesignator p ident assignParser = runReaderT (p SQ.empty SQ.empty) (ident, assignParser)

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
    => ATree i
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (ATree i)
desgNode nd desg = fmap (atExprStmt . flip atAssign nd) $
    flip (`foldr` facc) desg $ \idx acc -> case idx of
        CT.DesgIdx idx' -> do
            nd' <- maybeToParser' . (`addKind` atNumLit idx') =<< acc
            flip (atUnary ATDeref) nd' <$> maybeToParser' (CT.deref (atype nd'))
        CT.DesgMem mem -> atMemberAcc mem <$> acc
    where
        facc = asks fst
            >>= lift . lift . gets . lookupLVar
            >>= maybeToParser'
            <&> treealize
        maybeToParser' = lift . maybeToParser "invalid initializer-list"

initLoop :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i), i)
initLoop ty ai desg = initLoop' ai <* lift rbrace
    where
        initLoop' ai' = case CT.toTypeKind ty of
            CT.CTArray _ _ -> ($ (0, ai')) . fix $ \f (idx, rl) -> do
                rs <- desgInit (fromJust $ CT.deref ty) rl (CT.DesgIdx idx SQ.<| desg)
                M.choice
                    [ (rs, succ idx) <$ lift (M.lookAhead rbrace)
                    , lift comma *> f (succ idx, rs)
                    ]
            _ -> fail "internal compiler error"

initZero :: (Num i, Ord i, Show i, Enum i)
    => CT.TypeKind i
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initZero (CT.CTArray n ty) desg =
    foldM
        (\acc idx -> (SQ.>< acc) <$> initZero ty (CT.DesgIdx idx SQ.<| desg))
        SQ.empty
        [0..fromIntegral (pred n)]
initZero _ desg = SQ.singleton <$> desgNode (atNumLit 0) desg

arType :: (CT.CType (a j), CT.TypeKindBase a, Integral i) => a j -> i -> a j
arType ty len = snd (CT.dctorArray ty) $
    CT.mapTypeKind (CT.CTArray (fromIntegral len) . fromJust . CT.fromIncompleteArray) ty

initializerString :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerString ty ai desg
    | CT.isIncompleteArray ty = do
        newt <- lift $ bracket M.getInput M.setInput (const $ arType ty . length <$> stringLiteral)
        asks fst >>= lift . registerLVar newt >> desgInit newt ai desg
    | otherwise = case CT.toTypeKind ty of
        CT.CTArray n _ -> do
            s <- lift stringLiteral
            let s' = s <> replicate (fromIntegral n - pred (length s)) (toEnum 0)
                inds = sortBy (flip (.) reverse . compare . reverse) $ CT.accessibleIndices $ CT.toTypeKind ty
            fmap ((ai SQ.><) . SQ.fromList)
                $ mapM (uncurry desgNode)
                $ zipWith (flip (.) ((SQ.>< desg) . SQ.fromList) . (,) . atNumLit . fromIntegral . ord) s' inds
        _ -> fail "internal compiler error"

initializerList :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
initializerList ty ai desg = M.choice
    [ allZeroInit
    , withInitElements
    ]
    where
        allZeroInit = do
            void $ lift $ M.try (lbrace *> rbrace)
            (ai SQ.><) . SQ.fromList <$> forM
                (CT.accessibleIndices $ CT.toTypeKind ty)
                (desgNode (atNumLit 0) . (SQ.>< desg) . SQ.fromList)

        withInitElements
            | CT.isIncompleteArray ty = do
                newt <- lift $ bracket M.getInput M.setInput (const $ arType ty <$> lengthArrayBrace)
                asks fst
                    >>= lift . registerLVar newt
                    >> desgInit newt ai desg
            | otherwise = do
                void $ lift lbrace
                case CT.toTypeKind ty of
                    CT.CTArray n bt -> do
                        (ast, idx) <- initLoop ty ai desg
                        (ai SQ.><) . (ast SQ.><)
                            <$> foldM
                                (\acc idx' -> (SQ.>< acc) <$> initZero bt (CT.DesgIdx idx' SQ.<| desg))
                                SQ.empty
                                [fromIntegral idx..pred (fromIntegral n)]
                    _ -> fail "internal compiler error"

desgInit :: (Integral i, Bits i, Read i, Show i)
    => CT.StorageClass i
    -> SQ.Seq (ATree i)
    -> SQ.Seq (CT.Desg i)
    -> DesignatorParser i (SQ.Seq (ATree i))
desgInit ty ai desg = M.choice
    [ lift lookInitializerString *> initializerString ty ai desg
    , lift lookInitializerList *> initializerList ty ai desg
    , ai <$ lift lookStructInit
    , asks snd >>= lift >>= (flip desgNode desg >=> pure . (SQ.|>) ai)
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
    | CT.isArray ty || CT.isCTStruct ty = atBlock . toList <$> runDesignator (desgInit ty) ident p
    | otherwise = p >>= fromValidAssignAST <&> atExprStmt . ATNode ATAssign (atype lat) lat

varInit :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
varInit p ty ident = fromMaybe ty <$> lift (gets $ incomplete ty)
    >>= flip registerLVar ident
    >>= varInit' p ty ident
