{-|
Module      : Htcc.Parser.ConstructionData.Scope
Description : The Data type of scope and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric #-}
module Htcc.Parser.ConstructionData.Scope (
    -- * The types
    Scoped (..),
    LookupVarResult (..),
    -- * Operations for scope
    addLVar,
    addGVar,
    addLiteral,
    addTag,
    addTypedef,
    addFunction,
    addEnumerator,
    succNest,
    fallBack,
    lookupLVar,
    lookupGVar,
    lookupVar,
    lookupTag,
    lookupTypedef,
    lookupFunction,
    lookupEnumerator,
    initScope,
    resetLocal
) where

import GHC.Generics (Generic (..), Generic1 (..))
import Data.Bits (Bits (..))
import Numeric.Natural
import Data.Tuple.Extra (second)
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import qualified Htcc.CRules.Types as CT
import Htcc.Parser.AST.Core (ATree (..))
import qualified Htcc.Parser.ConstructionData.Scope.ManagedScope as SM
import qualified Htcc.Parser.ConstructionData.Scope.Var as PV
import qualified Htcc.Parser.ConstructionData.Scope.Tag as PS
import qualified Htcc.Parser.ConstructionData.Scope.Typedef as PT
import qualified Htcc.Parser.ConstructionData.Scope.Function as PF
import qualified Htcc.Parser.ConstructionData.Scope.Enumerator as SE
import qualified Htcc.Tokenizer.Token as HT

-- | The data type of a struct tag
data Scoped i = Scoped -- ^ The constructor of a struct tag
    {
        curNestDepth :: !Natural, -- ^ The nest depth of the parsing process
        vars :: PV.Vars i, -- ^ scoped all identifiers of variables (local variables, global variables and literals) visible during processing
        structs :: PS.Tags i, -- ^ scoped all struct tags
        typedefs :: PT.Typedefs i, -- ^ scoped all typedefs
        functions :: PF.Functions i, -- ^ scoped all identifires of functions
        enumerators :: SE.Enumerators i -- ^ scoped all identifiers of enumerators
    } deriving (Show, Generic, Generic1)

instance NFData i => NFData (Scoped i)

-- | A type that represents the result of a variable search
data LookupVarResult i = FoundGVar (PV.GVar i)  -- ^ A type constructor indicating that a global variable has been found
    | FoundLVar (PV.LVar i) -- ^ A type constructor indicating that a local variable has been found
    | FoundEnum (SE.Enumerator i) -- ^ A type constructor indicating that a enumerator has been found
    | NotFound -- ^ A type constructor indicating that it was not found
    deriving (Show, Eq)

{-# INLINE addVar #-}
addVar :: (Integral i, Bits i) => (CT.StorageClass i -> HT.TokenLC i -> PV.Vars i -> Either (T.Text, HT.TokenLC i) (ATree i, PV.Vars i)) -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addVar f ty tkn sc = second (\x -> sc { vars = x }) <$> f ty tkn (vars sc)

-- | `addLVar` has a scoped type argument and is the same function as `PV.addLVar` internally.
{-# INLINE addLVar #-}
addLVar :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addLVar ty tkn scp = addVar (PV.addLVar $ curNestDepth scp) ty tkn scp

-- | `addGVar` has a scoped type argument and is the same function as `PV.addGVar` internally.
{-# INLINE addGVar #-}
addGVar :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addGVar = addVar PV.addGVar

-- | `addLiteral` has a scoped type argument and is the same function as `PV.addLiteral` internally.
{-# INLINE addLiteral #-}
addLiteral :: (Integral i, Bits i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (ATree i, Scoped i)
addLiteral = addVar PV.addLiteral

-- | `succNest` has a scoped type argument and is the same function as `PV.succNest` internally.
{-# INLINE succNest #-}
succNest :: Scoped i -> Scoped i
succNest sc = sc { curNestDepth = succ $ curNestDepth sc } 

-- | `fallBack` has a scoped type argument and is the same function as `PV.fallBack` internally.
{-# INLINE fallBack #-}
fallBack :: Scoped i -> Scoped i -> Scoped i
fallBack pre post = pre 
    { 
        vars = PV.fallBack (vars pre) (vars post), 
        structs = SM.fallBack (structs pre) (structs post), 
        typedefs = SM.fallBack (typedefs pre) (typedefs post),
        functions = SM.fallBack (functions pre) (functions post),
        enumerators = SM.fallBack (enumerators pre) (enumerators post)
    } 

{-# INLINE lookupVar' #-}
lookupVar' :: (T.Text -> PV.Vars a -> b) -> T.Text -> Scoped a -> b
lookupVar' f s sc = f s $ vars sc

-- | `lookupLVar` has a scoped type argument and is the same function as `PV.lookupLVar` internally.
{-# INLINE lookupLVar #-}
lookupLVar :: T.Text -> Scoped i -> Maybe (PV.LVar i)
lookupLVar = lookupVar' PV.lookupLVar

-- | `lookupGVar` has a scoped type argument and is the same function as `PV.lookupGVar` internally.
{-# INLINE lookupGVar #-}
lookupGVar :: T.Text -> Scoped i -> Maybe (PV.GVar i)
lookupGVar = lookupVar' PV.lookupGVar

-- | `lookupVar` has a scoped type argument and is the same function as `PV.lookupVar` internally.
{-# INLINE lookupVar #-}
lookupVar :: T.Text -> Scoped i -> LookupVarResult i
lookupVar ident scp = case lookupLVar ident scp of
    Just local -> FoundLVar local
    _ -> case lookupEnumerator ident scp of
        Just enum -> FoundEnum enum
        _ -> maybe NotFound FoundGVar $ lookupGVar ident scp

-- | `lookupTag` has a scoped type argument and is the same function as `PS.lookupTag` internally.
{-# INLINE lookupTag #-}
lookupTag :: T.Text -> Scoped i -> Maybe (PS.Tag i)
lookupTag t sc = SM.lookup t $ structs sc

-- | `lookupTypedef` has a scoped type argument and is the same function as `PT.lookupTypedef` internally.
{-# INLINE lookupTypedef #-}
lookupTypedef :: T.Text -> Scoped i -> Maybe (PT.Typedef i)
lookupTypedef t sc = SM.lookup t $ typedefs sc

-- | `lookupFunction` has a scoped type argument and is the same function as `PF.lookupFunction` internally.
{-# INLINE lookupFunction #-}
lookupFunction :: T.Text -> Scoped i -> Maybe (PF.Function i)
lookupFunction t sc = SM.lookup t $ functions sc

{-# INLINE lookupEnumerator #-}
-- | `lookupEnumerator` has a scoped type argument and is the same function as `PF.lookupFunction` internally.
lookupEnumerator :: T.Text -> Scoped i -> Maybe (SE.Enumerator i)
lookupEnumerator t sc = SM.lookup t $ enumerators sc

-- | `addTag` has a scoped type argument and is the same function as `PS.add` internally.
{-# INLINE addTag #-}
addTag :: Num i => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addTag ty tkn sc = (\x -> sc { structs = x }) <$> PS.add (curNestDepth sc) ty tkn (structs sc)

-- | `addTypedef` has a scoped type argument and is the same function as `PT.add` internally.
{-# INLINE addTypedef #-}
addTypedef :: (Eq i, Num i) => CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addTypedef ty tkn sc = (\x -> sc { typedefs = x }) <$> PT.add (curNestDepth sc) ty tkn (typedefs sc)

-- | `addFunction` has a scoped type argument and is the same function as `PT.add` internally.
{-# INLINE addFunction #-}
addFunction :: Num i => Bool -> CT.StorageClass i -> HT.TokenLC i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addFunction fd ty tkn sc = (\x -> sc { functions = x }) <$> PF.add fd ty tkn (functions sc)

-- | `addEnumerator` has a scoped type argument and is the same function as `SE.add` internally.
{-# INLINE addEnumerator #-}
addEnumerator :: Num i => CT.StorageClass i -> HT.TokenLC i -> i -> Scoped i -> Either (SM.ASTError i) (Scoped i)
addEnumerator ty tkn val sc = (\x -> sc { enumerators = x }) <$> SE.add ty tkn val (enumerators sc)

{-# INLINE initScope #-}
-- | Helper function representing an empty scoped data
initScope :: Scoped i
initScope = Scoped 0 PV.initVars SM.initial SM.initial SM.initial SM.initial

{-# INLINE resetLocal #-}
-- | `resetLocal` has a scoped type argument and is the same function as `PV.resetLocal` internally.
resetLocal :: Scoped i -> Scoped i
resetLocal sc = sc { vars = PV.resetLocal (vars sc) }
