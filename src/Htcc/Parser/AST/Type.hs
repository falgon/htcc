{-|
Module      : Htcc.Parser.AST.Type
Description : Data types and type synonyms used during AST construction
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Data types and type synonyms used during AST construction
-}
module Htcc.Parser.AST.Type (
    ASTSuccess,
    ASTConstruction,
    ASTs,
    ASTResult,
    ASTState
) where

import           Htcc.Parser.AST.Core                            (ATree (..))
import           Htcc.Parser.ConstructionData.Core               (ConstructionData,
                                                                  Warnings)
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import qualified Htcc.Parser.ConstructionData.Scope.Var          as PV
import qualified Htcc.Tokenizer                                  as HT
import           Htcc.Utils.CompilationState                     (CompilationState)

-- | The type to be used when the AST construction is successful
type ASTSuccess i = ([HT.TokenLC i], ATree i, ConstructionData i)

-- | Types used during AST construction
type ASTConstruction i = Either (ASTError i) (ASTSuccess i)

-- | The type of AST list
type ASTs i = [ATree i]

-- | A type that represents the result after AST construction. Quadraple of warning list, constructed abstract syntax tree list, global variable map, literal list.
type ASTResult i = Either (ASTError i) (Warnings i, ASTs i, PV.GlobalVars i, PV.Literals i)

-- | The type synonym of ASTState
type ASTState i r = CompilationState (ConstructionData i) [HT.TokenLC i] i r
