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
module Htcc.Parser.Combinators.Var (
    varInit
) where
import           Control.Monad                 (forM)
import           Control.Monad.Trans           (MonadTrans (..))
import           Control.Monad.Trans.State     (gets)
import           Data.Bits                     (Bits)
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import qualified Htcc.CRules.Types             as CT
import           Htcc.Parser.AST               (ATKind (..), ATree (..),
                                                addKind, atAssign, atExprStmt,
                                                atMemberAcc, atNumLit, atUnary,
                                                treealize)
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.Combinators.Utils (maybeToParser, registerLVar)
import           Htcc.Parser.ConstructionData  (incomplete, lookupLVar)
import qualified Text.Megaparsec               as M

chkValidAssign :: Eq i => ATree i -> Parser i (ATree i)
chkValidAssign at@(ATNode _ ty _ _)
    | CT.toTypeKind ty == CT.CTVoid = fail "void value not ignored as it ought to be"
    | otherwise = pure at
chkValidAssign _ = fail "expected to assign"

desgNode :: (Num i, Ord i, Show i)
    => T.Text
    -> ATree i
    -> [CT.Desg i]
    -> Parser i (ATree i)
desgNode ident rhs desg = fmap (atExprStmt . flip atAssign rhs) $
    flip (`foldr` ntLVarTree) desg $ \idx acc -> case idx of
        CT.DesgIdx idx' -> do
            at <- acc
            nd' <- maybeToParser' $ addKind at $ atNumLit idx'
            flip (atUnary ATDeref) nd' <$> maybeToParser' (CT.deref (atype nd'))
        CT.DesgMem mem -> atMemberAcc mem <$> acc
    where
        maybeToParser' = maybeToParser "invalid initializer-list"
        ntLVarTree = treealize
            <$> (maybeToParser' =<< lift (gets $ lookupLVar ident))

initZero :: (Num i, Ord i, Show i, Enum i)
    => CT.TypeKind i
    -> T.Text
    -> [CT.Desg i]
    -> Parser i [ATree i]
initZero (CT.CTArray n t) ident desg = fmap concat $
    forM [0..fromIntegral (pred n)] $ initZero t ident . (:desg) . CT.DesgIdx
initZero _ ident desg = (:[]) <$> desgNode ident (atNumLit 0) desg

varInit' :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> ATree i
    -> Parser i (ATree i)
varInit' p ty ident lat
    | CT.isArray ty || CT.isCTStruct ty = error "sorry, not support yet"
    | otherwise = p >>= chkValidAssign <&> atExprStmt . ATNode ATAssign (atype lat) lat

varInit :: (Integral i, Bits i, Read i, Show i)
    => Parser i (ATree i)
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
varInit p ty ident = do
    ty' <- fromMaybe ty <$> lift (gets $ incomplete ty)
    registerLVar ty' ident >>= varInit' p ty ident
