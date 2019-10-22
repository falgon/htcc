{-|
Module      : Htcc.Parser.Scope
Description : The Data type of scope and its utilities used in parsing
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The Data type of variables and its utilities used in parsing
-}
{-# LANGUAGE DeriveGeneric #-}
module Htcc.Parser.Scope (
    Scoped (..),
    addLVar,
    addGVar,
    addLiteral,
    addStructTag,
    addTypedef,
    addFunction,
    succNest,
    fallBack,
    lookupLVar,
    lookupGVar,
    lookupVar,
    lookupStructTag,
    lookupTypedef,
    lookupFunction,
    initScope,
    resetLocal
) where

import GHC.Generics (Generic (..), Generic1 (..))
import Data.Bits (Bits (..))
import Numeric.Natural
import Data.Tuple.Extra (second)
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

import Htcc.Parser.AST (ATree (..))
import qualified Htcc.CRules.Types as CT
import qualified Htcc.Parser.Scope.ManagedScope as SM
import qualified Htcc.Parser.Scope.Var as PV
import qualified Htcc.Parser.Scope.Struct as PS
import qualified Htcc.Parser.Scope.Typedef as PT
import qualified Htcc.Parser.Scope.Function as PF
import qualified Htcc.Tokenizer.Token as HT

-- | The data type of a struct tag
data Scoped i = Scoped -- ^ The constructor of a struct tag
    {
        curNestDepth :: !Natural, -- ^ The nest depth of the parsing process
        vars :: PV.Vars i, -- ^ scoped all identifiers of variables (local variables, global variables and literals) visible during processing
        structs :: PS.Structs, -- ^ scoped all struct tags
        typedefs :: PT.Typedefs, -- ^ scoped all typedefs
        functions :: PF.Functions -- ^ scoped all identifires of functions
    } deriving (Show, Generic, Generic1)

instance NFData i => NFData (Scoped i)

{-# INLINE addVar #-}
addVar :: (Integral i, Bits i) => (CT.TypeKind -> HT.TokenLC i -> PV.Vars i -> Either (T.Text, HT.TokenLC i) (ATree i, PV.Vars i)) -> CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (ATree i, Scoped i)
addVar f ty tkn sc = second (\x -> sc { vars = x }) <$> f ty tkn (vars sc)

-- | `addLVar` has a scoped type argument and is the same function as `PV.addLVar` internally.
addLVar :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (ATree i, Scoped i)
addLVar ty tkn scp = addVar (PV.addLVar $ curNestDepth scp) ty tkn scp

-- | `addGVar` has a scoped type argument and is the same function as `PV.addGVar` internally.
addGVar :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (ATree i, Scoped i)
addGVar = addVar PV.addGVar

-- | `addLiteral` has a scoped type argument and is the same function as `PV.addLiteral` internally.
addLiteral :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (ATree i, Scoped i)
addLiteral = addVar PV.addLiteral

-- | `succNest` has a scoped type argument and is the same function as `PV.succNest` internally.
succNest :: Scoped i -> Scoped i
succNest sc = sc { curNestDepth = succ $ curNestDepth sc } 

-- | `fallBack` has a scoped type argument and is the same function as `PV.fallBack` internally.
fallBack :: Scoped i -> Scoped i -> Scoped i
fallBack pre post = pre 
    { 
        vars = PV.fallBack (vars pre) (vars post), 
        structs = SM.fallBack (structs pre) (structs post), 
        typedefs = SM.fallBack (typedefs pre) (typedefs post),
        functions = SM.fallBack (functions pre) (functions post)
    } 

{-# INLINE lookupVar' #-}
lookupVar' :: (T.Text -> PV.Vars a -> b) -> T.Text -> Scoped a -> b
lookupVar' f s sc = f s $ vars sc

-- | `lookupLVar` has a scoped type argument and is the same function as `PV.lookupLVar` internally.
lookupLVar :: T.Text -> Scoped i -> Maybe (PV.LVar i)
lookupLVar = lookupVar' PV.lookupLVar

-- | `lookupGVar` has a scoped type argument and is the same function as `PV.lookupGVar` internally.
lookupGVar :: T.Text -> Scoped i -> Maybe PV.GVar
lookupGVar = lookupVar' PV.lookupGVar

-- | `lookupVar` has a scoped type argument and is the same function as `PV.lookupVar` internally.
lookupVar :: T.Text -> Scoped i -> Maybe (Either PV.GVar (PV.LVar i))
lookupVar = lookupVar' PV.lookupVar

-- | `lookupStructTag` has a scoped type argument and is the same function as `PS.lookupStructTag` internally.
lookupStructTag :: T.Text -> Scoped i -> Maybe PS.StructTag
lookupStructTag t sc = SM.lookup t $ structs sc

-- | `lookupTypedef` has a scoped type argument and is the same function as `PT.lookupTypedef` internally.
lookupTypedef :: T.Text -> Scoped i -> Maybe PT.Typedef
lookupTypedef t sc = SM.lookup t $ typedefs sc

-- | `lookupFunction` has a scoped type argument and is the same function as `PF.lookupFunction` internally.
lookupFunction :: T.Text -> Scoped i -> Maybe PF.Function
lookupFunction t sc = SM.lookup t $ functions sc

-- | `addStructTag` has a scoped type argument and is the same function as `PS.addStructTag` internally.
addStructTag :: Num i => CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (Scoped i)
addStructTag ty tkn sc = (\x -> sc { structs = x }) <$> PS.add (curNestDepth sc) ty tkn (structs sc)

-- | `addTypedef` has a scoped type argument and is the same function as `PT.addTypedef` internally.
addTypedef :: Num i => CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (Scoped i)
addTypedef ty tkn sc = (\x -> sc { typedefs = x }) <$> PT.add (curNestDepth sc) ty tkn (typedefs sc)

-- | `addFunction` has a scoped type argument and is the same function as `PT.addTypedef` internally.
addFunction :: Num i => Bool -> CT.TypeKind -> HT.TokenLC i -> Scoped i -> Either (T.Text, HT.TokenLC i) (Scoped i)
addFunction fd ty tkn sc = (\x -> sc { functions = x }) <$> PF.add fd ty tkn (functions sc)

{-# INLINE initScope #-}
-- | Helper function representing an empty scoped data
initScope :: Scoped i
initScope = Scoped 0 PV.initVars SM.initial SM.initial SM.initial

{-# INLINE resetLocal #-}
-- | `resetLocal` has a scoped type argument and is the same function as `PV.resetLocal` internally.
resetLocal :: Scoped i -> Scoped i
resetLocal sc = sc { vars = PV.resetLocal (vars sc) }
