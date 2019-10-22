{-|
Module      : Htcc.Parser.ASTStatus
Description : Data types and type synonyms used during AST construction
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Data types and type synonyms used during AST construction
-}
{-# LANGUAGE DeriveGeneric #-}
module Htcc.Parser.ASTStatus (
    ASTError (..),
    ASTSuccess (..),
    ASTStatus,
    cstat,
    extract,
    (&>>=),
    astErr,
    astSucc
) where

import qualified Data.Sequence as S

import qualified Data.Text as T
import qualified Htcc.Tokenizer as HT
import Htcc.Parser.AST
import Htcc.Parser.Scope
import Htcc.Utils (uncurry4)

-- | The type that will be held if there is an error during AST construction
data ASTError i = ASTError T.Text (HT.TokenLC i) -- ^ The constructor of `ASTError`
    deriving Show

-- | The type to use when the AST is successfully constructed
data ASTSuccess i = ASTSuccess (S.Seq (T.Text, HT.TokenLC i)) [HT.TokenLC i] (ATree i) (Scoped i) -- ^ The constructor of `ASTSuccess`
    deriving Show

-- | A type synonym representing the state of compilation
type ASTStatus i = Either (ASTError i) (ASTSuccess i)

{-# INLINE uncurryASTSucc #-}
uncurryASTSucc :: ASTSuccess i -> (S.Seq (T.Text, HT.TokenLC i), [HT.TokenLC i], ATree i, (Scoped i))
uncurryASTSucc (ASTSuccess warns tokens ast scp) = (warns, tokens, ast, scp)

-- | Case analysis for the `ASTError` and `ASTSuccess`. 
-- If the value is `ASTError`, apply the first function with `ASTError` removed; 
-- if it is `ASTSuccess`, apply the second function with `ASTSuccess` removed.
cstat :: (T.Text -> (HT.TokenLC i) -> a) -> (S.Seq (T.Text, HT.TokenLC i) -> [HT.TokenLC i] -> ATree i -> (Scoped i) -> a) -> ASTStatus i -> a
cstat f _ (Left (ASTError txt tklc)) = f txt tklc
cstat _ f (Right (ASTSuccess warns tokens ast scp)) = f warns tokens ast scp

-- | Remove `ASTSuccess` and bind.
{-# INLINE extract #-}
extract :: (S.Seq (T.Text, HT.TokenLC i) -> [HT.TokenLC i] -> ATree i -> (Scoped i) -> ASTStatus i) -> ASTStatus i -> ASTStatus i
extract f cs = cs >>= uncurry4 f . uncurryASTSucc 

infixl 1 &>>=

-- | `&>>=` is a shorthand of @flip extract@.
{-# INLINE (&>>=) #-}
(&>>=) :: ASTStatus i -> (S.Seq (T.Text, HT.TokenLC i) -> [HT.TokenLC i] -> ATree i -> (Scoped i) -> ASTStatus i) -> ASTStatus i
(&>>=) = flip extract

-- | Generates an error value for `ASTStatus`. This is equivalent to following.
--
-- >>> (.) Left . ASTError
{-# INLINE astErr #-}
astErr :: T.Text -> HT.TokenLC i -> Either (ASTError i) a
astErr = (.) Left . ASTError

-- | Generates an success value for `ASTStatus`. This is equivalent to following.
--
-- >>> astSucc warn token at scp = Right $ ASTSuccess warn token at scp
{-# INLINE astSucc #-}
astSucc :: S.Seq (T.Text, HT.TokenLC i) -> [HT.TokenLC i] -> ATree i -> Scoped i -> Either a (ASTSuccess i)
astSucc = (.) ((.) ((.) Right)) . ASTSuccess


