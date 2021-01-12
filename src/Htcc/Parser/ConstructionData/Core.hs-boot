module Htcc.Parser.ConstructionData.Core where

import qualified Data.Sequence                      as SQ
import qualified Data.Text                          as T
import           Data.Void
import qualified Htcc.Parser.ConstructionData.Scope as AS
import qualified Text.Megaparsec                    as M

type Warnings = SQ.Seq (M.ParseErrorBundle T.Text Void)

data ConstructionData i = ConstructionData
    {
        warns        :: Warnings,
        scope        :: AS.Scoped i,
        isSwitchStmt :: Bool
    }
