{-|
Module      : Htcc.Parser.ConstructionData.Scope.Function
Description : The Data type of typedef and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Htcc.Parser.ConstructionData.Scope.Function (
    Function (..),
    Functions,
    add
) where

import           Control.DeepSeq                                 (NFData (..))
import qualified Data.Map                                        as M
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic (..))

import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.ConstructionData.Scope.ManagedScope
import           Htcc.Parser.ConstructionData.Scope.Utils        (internalCE)
import qualified Htcc.Tokenizer.Token                            as HT

-- | The data type of a typedef tag
data Function a = Function -- ^ The contypedefor of a typedef tag
    {
        fntype     :: CT.StorageClass a, -- ^ The type of this typedef
        fnDefined  :: Bool, -- ^ If the function is defined, it will be `True`, otherwise will be `False`.
        fnImplicit :: Bool -- ^ `True` only when the function only exists because of an implicit declaration synthesized from a call site.
    } deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (Function a)

instance ManagedScope (Function i) where
    lookup = M.lookup
    fallBack = flip const
    initial = M.empty

-- | The typedefs data typedefs
type Functions i = M.Map T.Text (Function i)

-- | Given the flag (when that is added function, it is `True`. otherwise `False`), type, identifier token, and `Functions`,
-- if the specified identifier already exists in the same scope,
-- return an error message and its location as a pair.
-- Otherwise, add a new tag to `Functions` and return it.
-- If the token does not indicate an identifier, an error indicating internal compiler error is returned.
add :: (Eq i, Num i) => Bool -> Bool -> CT.StorageClass i -> HT.TokenLC i -> Functions i -> Either (ASTError i) (Functions i)
add df isImplicit t cur@(_, HT.TKIdent ident) sts = case M.lookup ident sts of
    Just foundFunc ->
        case mergeFunctionTypes (fntype foundFunc) t of
            Nothing ->
                Left ("conflicting types for '" <> ident <> "'", cur)
            Just mergedType
                | fnDefined foundFunc && df ->
                    Left ("conflicting types for '" <> ident <> "'", cur)
                | otherwise ->
                    Right $
                        M.insert
                            ident
                            Function
                                { fntype = mergedType
                                , fnDefined = fnDefined foundFunc || df
                                , fnImplicit = fnImplicit foundFunc && isImplicit
                                }
                            sts
    Nothing ->
        Right $
            M.insert
                ident
                Function
                    { fntype = t
                    , fnDefined = df
                    , fnImplicit = isImplicit
                    }
                sts
add _ _ _ _ _ = Left (internalCE, (HT.TokenLCNums 0 0, HT.TKEmpty))

mergeFunctionTypes :: Eq i => CT.StorageClass i -> CT.StorageClass i -> Maybe (CT.StorageClass i)
mergeFunctionTypes (CT.SCAuto lhs) (CT.SCAuto rhs) =
    CT.SCAuto <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeFunctionTypes (CT.SCStatic lhs) (CT.SCStatic rhs) =
    CT.SCStatic <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeFunctionTypes (CT.SCRegister lhs) (CT.SCRegister rhs) =
    CT.SCRegister <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeFunctionTypes (CT.SCUndef lhs) (CT.SCUndef rhs) =
    CT.SCUndef <$> CT.mergeCompatibleTypeKinds lhs rhs
mergeFunctionTypes _ _ = Nothing
