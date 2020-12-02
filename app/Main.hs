{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad       (forM_, (>=>))
import qualified Data.Text.IO        as T
import           Data.Version        (showVersion)
import           Development.GitRev  (gitHash)
import qualified Options.Applicative as OA
import qualified Paths_htcc          as P

data Opts = Opts
    { optIsRunAsm  :: !Bool
    , optIsVerbose :: !Bool
    , optOutput    :: FilePath
    , optInput     :: [FilePath]
    } deriving (Read, Show)

output :: OA.Parser String
output = OA.strOption $ mconcat [
    OA.metavar "<file>"
  , OA.value "a.out"
  , OA.long "output"
  , OA.short 'o'
  , OA.help "Place the output into <file>"
  ]

input :: OA.Parser [String]
input = OA.some $ OA.strArgument $ mconcat [
    OA.metavar "file..."
  , OA.help "Input source files"
  ]

isRunAsm :: OA.Parser Bool
isRunAsm = OA.switch $ mconcat [
    OA.long "run-asm"
  , OA.short 'r'
  , OA.help "Generates executable binaries using the assembler built into the GCC compiler"
  ]

isVerbose :: OA.Parser Bool
isVerbose = OA.switch $ mconcat [
    OA.long "verbose"
  , OA.short 'v'
  , OA.help "Show the programs invoked by the compiler"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> isRunAsm
    <*> isVerbose
    <*> output
    <*> input

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [
    OA.long "version"
  , OA.help "Show compiler version information"
  ]
    where
        vopt = concat [
            "The C Language Compiler htcc "
          , showVersion P.version
          , "\ncommit hash: "
          , $(gitHash)
          ]

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> versionOption <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ concat [
        "The C Language Compiler htcc "
      , showVersion P.version
    ]
  ]

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    forM_ (optInput opts) $ T.readFile >=> T.putStr
