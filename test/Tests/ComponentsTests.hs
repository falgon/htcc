module Tests.ComponentsTests (
    exec
) where

import Tests.Utils hiding (exec)
-- import Test.HUnit (Test (..))
import Tests.ComponentsTests.Parser.Combinators as PC

exec :: IO ()
exec = runTests $ 
    TestList [
        PC.test
    ]
