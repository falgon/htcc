module Tests.ComponentsTests (
    exec
) where

import           Tests.Utils                              hiding (exec)
-- import Test.HUnit (Test (..))
import qualified Tests.ComponentsTests.AsmOutput          as AsmOutput
import qualified Tests.ComponentsTests.CommandSelection   as CommandSelection
import           Tests.ComponentsTests.Parser.Combinators as PC

exec :: IO ()
exec = runTestsSequential $
    TestList [
        AsmOutput.test
      , CommandSelection.test
      , PC.test
    ]
