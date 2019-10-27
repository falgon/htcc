{-|
Module      : Htcc.Parser.ConsturctData
Description : Data types and type synonyms used during AST construction
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Data types and type synonyms used during AST construction
-}
module Htcc.Parser.ConstructionData (
    ConstructionData (..),
    addLVar,
    addGVar,
    addLiteral,
    succNest,
    fallBack,
    lookupLVar,
    lookupGVar,
    lookupVar,
    lookupStructTag,
    lookupTypedef,
    lookupFunction,
    addStructTag,
    addTypedef,
    addFunction,
    initConstructionData,
    resetLocal,
    pushWarn
) where

import Data.Bits (Bits (..))
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Tuple.Extra (second)

import qualified Htcc.CRules.Types as CT
import qualified Htcc.Parser.AST.Scope as AS
import qualified Htcc.Parser.AST.Scope.Var as PV
import qualified Htcc.Parser.AST.Scope.Struct as PS
import qualified Htcc.Parser.AST.Scope.Typedef as PT
import qualified Htcc.Parser.AST.Scope.Function as PF
import Htcc.Parser.AST.Core (ATree (..))
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)
import Htcc.Tokenizer.Token (TokenLC)
import qualified Htcc.Tokenizer.Token as HT

-- | `ConstructionData` is a set of "things" used during the construction of the AST. 
-- Contains error messages and scope information.
data ConstructionData i = ConstructionData -- ^ The constructor of ConstructionData
    {
        warns :: S.Seq (T.Text, TokenLC i), -- ^ The warning messages 
        scope :: AS.Scoped i -- ^ scope type
    }

{-# INLINE addVar #-}
addVar :: (Integral i, Bits i) => (CT.TypeKind -> HT.TokenLC i -> AS.Scoped i -> Either (ASTError i) (ATree i, AS.Scoped i)) -> CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ATree i, ConstructionData i)
addVar f ty tkn cd = second (\x -> cd { scope = x }) <$> f ty tkn (scope cd)

-- | Shortcut to function `Htcc.Parser.AST.Scope.addLVar` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> second (\x -> y { scope = x }) <$> Htcc.Parser.AST.Scope.addLVar ty tkn (scope x)
addLVar :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ATree i, ConstructionData i)
addLVar = addVar AS.addLVar

-- | Shortcut to function `Htcc.Parser.AST.Scope.addGVar` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> second (\x -> y { scope = x }) <$> Htcc.Parser.AST.Scope.addGVar ty tkn (scope x)
addGVar :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ATree i, ConstructionData i)
addGVar = addVar AS.addGVar

-- | Shortcut to function `Htcc.Parser.AST.Scope.addLiteral` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to
--
-- >>> second (\x -> y { scope = x }) <$> Htcc.Parser.AST.Scope.addLiteral ty tkn (scope x)
addLiteral :: (Integral i, Bits i) => CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ATree i, ConstructionData i)
addLiteral = addVar AS.addLiteral

-- | Shortcut to function `Htcc.Parser.AST.Scope.succNest` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> x { scope = Htcc.Parser.AST.Scope.succNest (scope x) }
succNest :: ConstructionData i -> ConstructionData i
succNest cd = cd { scope = AS.succNest (scope cd) }

-- | Shortcut to function `Htcc.Parser.AST.Scope.fallBack` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> post { scope = Htcc.Parser.AST.Scope.fallBack (scope pre) (scope post) }
fallBack :: ConstructionData i -> ConstructionData i -> ConstructionData i
fallBack pre post = post { scope = AS.fallBack (scope pre) (scope post) }

{-# INLINE lookupFromScope #-}
lookupFromScope :: (T.Text -> AS.Scoped i -> a) -> T.Text -> ConstructionData i -> a
lookupFromScope f s cd = f s $ scope cd

-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupLVar` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupLVar s $ scope x
lookupLVar :: T.Text -> ConstructionData i -> Maybe (PV.LVar i)
lookupLVar = lookupFromScope AS.lookupLVar

-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupGVar` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupGVar s $ scope x
lookupGVar :: T.Text -> ConstructionData i -> Maybe PV.GVar
lookupGVar = lookupFromScope AS.lookupGVar

-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupVar` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupVar s $ scope x
lookupVar :: T.Text -> ConstructionData i -> Maybe (Either PV.GVar (PV.LVar i))
lookupVar = lookupFromScope AS.lookupVar


-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupStructTag` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupStructTag s $ scope x
lookupStructTag :: T.Text -> ConstructionData i -> Maybe PS.StructTag
lookupStructTag = lookupFromScope AS.lookupStructTag

-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupTypedef` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupTypedef s $ scope x
lookupTypedef :: T.Text -> ConstructionData i -> Maybe PT.Typedef
lookupTypedef = lookupFromScope AS.lookupTypedef

-- | Shortcut to function `Htcc.Parser.AST.Scope.lookupFunction` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> Htcc.Parser.AST.Scope.lookupFunction s $ scope x
lookupFunction :: T.Text -> ConstructionData i -> Maybe PF.Function
lookupFunction = lookupFromScope AS.lookupFunction

-- | Shortcut to function `Htcc.Parser.AST.Scope.addStructTag` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> (\y -> x { scope = y }) <$> Htcc.Parser.AST.Scope.addStructTag ty tkn (scope x)
addStructTag :: Num i => CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ConstructionData i)
addStructTag ty tkn cd = (\x -> cd { scope = x }) <$> AS.addStructTag ty tkn (scope cd)

-- | Shortcut to function `Htcc.Parser.AST.Scope.addTypedef` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> (\y -> x { scope = y }) <$> Htcc.Parser.AST.Scope.addTypedef ty tkn (scope x)
addTypedef :: Num i => CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ConstructionData i)
addTypedef ty tkn cd = (\x -> cd { scope = x }) <$> AS.addTypedef ty tkn (scope cd)

-- | Shortcut to function `Htcc.Parser.AST.Scope.addFunction` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> (\y -> x { scope = y }) <$> Htcc.Parser.AST.Scope.addFunction ty tkn (scope x)
addFunction :: Num i => Bool -> CT.TypeKind -> HT.TokenLC i -> ConstructionData i -> Either (ASTError i) (ConstructionData i)
addFunction fd ty tkn cd = (\x -> cd { scope = x }) <$> AS.addFunction fd ty tkn (scope cd)

-- | Shortcut to the initial state of `ConstructionData`.
{-# INLINE initConstructionData #-}
initConstructionData :: ConstructionData i
initConstructionData = ConstructionData S.empty AS.initScope

-- | Shortcut to function `Htcc.Parser.AST.Scope.resetLocal` for variable @x@ of type `ConstructionData`. 
-- This function is equivalent to 
--
-- >>> x { scope = Htcc.Parser.AST.Scope.resetLocal (scope x) }
resetLocal :: ConstructionData i -> ConstructionData i
resetLocal cd = cd { scope = AS.resetLocal (scope cd) }

-- | Function to add warning text.
pushWarn :: T.Text -> TokenLC i -> ConstructionData i -> ConstructionData i
pushWarn t tkn cd = cd { warns = warns cd S.|> (t, tkn) } 
