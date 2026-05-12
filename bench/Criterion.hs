module Main (main) where

import           Criterion.Main                                  (bench, bgroup,
                                                                  defaultConfig,
                                                                  defaultMainWith,
                                                                  nf, whnf)
import           Criterion.Types                                 (reportFile)

import qualified Data.Text                                       as T
import           Data.Void                                       (Void)

import qualified Htcc.MegaparsecCompat                           as M
import           Htcc.Parser.AST                                 (ASTs)
import           Htcc.Parser.Combinators                         (parser,
                                                                  runParser)
import           Htcc.Parser.ConstructionData.Core               (Warnings)
import qualified Htcc.Parser.ConstructionData.Scope.Function     as PF
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import qualified Htcc.Parser.ConstructionData.Scope.Var          as PV
import qualified Htcc.Tokenizer                                  as HT
import           Htcc.Utils                                      (tshow)

tknize :: T.Text -> Either (ASTError Int) [HT.TokenLC Int]
tknize = HT.tokenize

parseProgram :: T.Text -> Either (M.ParseErrorBundle T.Text Void) (Warnings, ASTs Integer, PV.GlobalVars Integer, PV.Literals Integer, PF.Functions Integer)
parseProgram = runParser parser ""

data CCodes =
    ReturningZero
    | StrLiteral
    | CalculateFibonacci

instance Show CCodes where
    show ReturningZero = "int main() { retunr 0; }"
    show StrLiteral = "int main() { \"\\a\\bhoge\\\\hoge\"; }"
    show CalculateFibonacci = "int fib(int n) {\
        \   if (n == 0) return 1;\
        \   else if (n == 1) return 1;\
        \   else if (n >= 2) return fib(n - 1) + fib(n - 2);\
        \   else return 0;\
        \}\
        \int main() { return fib(5); }"

main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "./bench_report.html" })
    [ bgroup "tokenize programs (whnf)"
        [ bench "Returning zero" $ whnf tknize $ tshow ReturningZero
        , bench "StrLiteral" $ whnf tknize $ tshow StrLiteral
        , bench "Calculate fibonacci" $ whnf tknize $ tshow CalculateFibonacci
        ]
    , bgroup "tokenize programs (nf)"
        [ bench "Returning zero" $ nf tknize $ tshow ReturningZero
        , bench "StrLiteral" $ nf tknize $ tshow StrLiteral
        , bench "Calculate fibonacci" $ nf tknize $ tshow CalculateFibonacci
        ]
    , bgroup "parse tokens (whnf)"
        [ bench "ReturningZero" $ whnf parseProgram $ tshow ReturningZero
        , bench "StrLiteral" $ whnf parseProgram $ tshow StrLiteral
        , bench "Calculate fibonacci" $ whnf parseProgram $ tshow CalculateFibonacci
        ]
    ]
