module Htcc.Parser.ConstructionData.Core where

import qualified Data.Sequence                                 as SQ
import qualified Data.Text                                     as T
import           Data.Void
import qualified Htcc.CRules.Types                             as CT
import qualified Htcc.Parser.ConstructionData.Scope            as AS
import qualified Htcc.Parser.ConstructionData.Scope.Enumerator as SE
import qualified Htcc.Parser.ConstructionData.Scope.Tag        as PS
import qualified Text.Megaparsec                               as M

type Warnings = SQ.Seq (M.ParseErrorBundle T.Text Void)

data FunctionParamScope i = FunctionParamScope
    {
        fpsScopeId     :: CT.ScopeId,
        fpsTags        :: PS.Tags i,
        fpsEnumerators :: SE.Enumerators i
    }

data ConstructionData i = ConstructionData
    {
        warns                            :: Warnings,
        scope                            :: AS.Scoped i,
        tagHistory                       :: PS.TagHistory i,
        functionParamScopes              :: [FunctionParamScope i],
        isSwitchStmt                     :: Bool,
        suppressUnsupportedValueChecks   :: Bool,
        allowSameInputExternalCollisions :: Bool
    }

initConstructionData :: ConstructionData i
