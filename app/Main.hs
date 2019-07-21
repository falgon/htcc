module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Bool (bool)
import Data.Tuple.Extra (second, dupe)
import Data.Either (isLeft, fromLeft, fromRight)

import Control.Monad (unless)
import Control.Monad.Fix (fix)

import C.Token
import C.Utils

checkArgs :: IO (Maybe [String])
checkArgs = uncurry (bool Nothing . Just) . second (not . null) . dupe <$> getArgs

maybeExit :: Maybe a -> IO a
maybeExit = maybe exitFailure return

outHead :: IO ()
outHead = putStrLn ".intel_syntax noprefix" *>
    putStrLn ".global main" *>
    putStrLn "main:"

outError :: String -> Int -> IO ()
outError xs n = putStrLn xs >> putStr (replicate n ' ') >> putStrLn "^ Invalid token" >> exitFailure

srcCode :: String -> IO ()
srcCode xs = let tk = tokenize xs in 
    if isLeft tk then outError xs (fromLeft 0 tk) else outHead >> case (fromRight [] tk) of
        (TKNum n):us -> do
            putStrLn $ "\tmov rax, " ++ show n
            ($ us) . fix $ \f ts -> unless (null ts) $ do
                case head ts of
                    TKReserved c -> case ts !! 1 of
                        TKNum m -> putStrLn ("\t" ++ (if c == '+' then "add" else "sub") ++ " rax, " ++ show m)
                        _ -> err "Syntax Error"
                    _ -> err "Syntax Error"
                f $ drop 2 ts
            putStrLn "\tret"
        _ -> err "Syntax Error"

main :: IO ()
main = checkArgs >>= maybeExit >>= srcCode . head
