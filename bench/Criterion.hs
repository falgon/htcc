module Main (main) where

import           Criterion.Main  (bench, bgroup, defaultConfig, defaultMainWith,
                                  nf, whnf)
import           Criterion.Types (reportFile)

import           Data.Either     (fromRight)
import qualified Data.Text       as T

import           Htcc.Parser     (parse)
import qualified Htcc.Tokenizer  as HT
import           Htcc.Utils      (tshow)

tknize :: T.Text -> Either (HT.TokenLCNums Int, T.Text) [HT.TokenLC Int]
tknize = HT.tokenize

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
        [ bench "ReturningZero" $ whnf parse $ fromRight [] $ tknize $ tshow ReturningZero
        , bench "StrLiteral" $ whnf parse $ fromRight [] $ tknize $ tshow StrLiteral
        , bench "Calculate fibonacci" $ whnf parse $ fromRight [] $ tknize $ tshow CalculateFibonacci
        ]
    ]
