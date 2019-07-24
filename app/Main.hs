module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Bool (bool)
import Data.Tuple.Extra (second, dupe)
import Data.Either (isLeft, fromLeft, fromRight)

import C.Token (Token (..), tokenize)
import C.Parse (parse)
import C.Generate (generate)
import C.Utils (err)

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
srcCode xs = let tk = tokenize xs :: Either Int [Token Int] in
    if isLeft tk then outError xs (fromLeft 0 tk) else flip (maybe (err "Failed to generate abstract tree")) (parse $ fromRight [] tk) $ \(ys, n) ->
        outHead >> putStrLn "\tpush rbp" >> putStrLn "\tmov rbp, rsp" >> putStrLn ("\tsub rsp, " ++ show (n * 8)) >> mapM_ generate ys >> putStrLn "\tpop rax" 
            >> putStrLn "\tmov rsp, rbp" >> putStrLn "\tpop rbp" >> putStrLn "\tret"

main :: IO ()
main = checkArgs >>= maybeExit >>= srcCode . head
