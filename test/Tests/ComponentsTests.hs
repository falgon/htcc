module Tests.ComponentsTests (
    exec
) where

import           Tests.Utils                              hiding (exec)
-- import Test.HUnit (Test (..))
import qualified Tests.ComponentsTests.AsmOutput          as AsmOutput
import           Tests.ComponentsTests.Parser.Combinators as PC

exec :: IO ()
exec = runTests $
    TestList [
        AsmOutput.test
      , PC.test
    ]
