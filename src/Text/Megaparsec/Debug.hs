module Text.Megaparsec.Debug (
    dbg
) where

dbg :: String -> a -> a
dbg _ = id
