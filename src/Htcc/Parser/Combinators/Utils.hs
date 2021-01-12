{-|
Module      : Htcc.Parser.Combinators.Utils
Description : C language parser Combinators
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language parser Combinators
-}
{-# LANGUAGE LambdaCase, Rank2Types, TypeOperators #-}
module Htcc.Parser.Combinators.Utils (
    maybeToParser
  , registerLVar
  , bracket
  , tmpTKIdent
  , getPosState
) where
import           Control.Monad.State               (gets, put)
import           Control.Natural                   (type (~>))
import           Data.Bits                         (Bits (..))
import qualified Data.Text                         as T
import qualified Htcc.CRules.Types                 as CT
import           Htcc.Parser.AST.Core              (ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.ConstructionData.Core (addLVar)
import qualified Htcc.Tokenizer.Token              as HT
import qualified Text.Megaparsec                   as M

maybeToParser :: String -> Maybe ~> Parser i
maybeToParser s = maybe (fail s) pure

registerLVar :: (Bits i, Integral i) => CT.StorageClass i -> T.Text -> Parser i (ATree i)
registerLVar ty ident = gets (addLVar ty (HT.TokenLCNums 1 1, HT.TKIdent ident))
    >>= \case
        Right (lat, scp') -> lat <$ put scp'
        Left err          -> fail $ T.unpack $ fst err

bracket :: Parser i a -> (a -> Parser i b) -> (a -> Parser i c) -> Parser i c
bracket beg end m = do
    b <- beg
    M.withRecovery (\err -> end b *> M.parseError err) (m b) <* end b

tmpTKIdent :: Num i => T.Text -> HT.TokenLC i
tmpTKIdent ident = (HT.TokenLCNums 1 1, HT.TKIdent ident)

getPosState :: Parser i (M.PosState T.Text)
getPosState = do
    statePos <- M.statePosState <$> M.getParserState
    srcPos <- M.getSourcePos
    pure $ statePos { M.pstateSourcePos = srcPos }
