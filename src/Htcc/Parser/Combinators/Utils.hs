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
  , registerGVar
  , registerGVarWith
  , registerStringLiteral
  , registerFunc
  , bracket
  , getPosState
) where
import qualified Data.ByteString.UTF8 as BSU
import           Control.Monad.State                             (gets, put)
import           Control.Natural                                 (type (~>))
import           Data.Bits                                       (Bits (..))
import qualified Data.Text                                       as T
import qualified Htcc.CRules.Types                               as CT
import           Htcc.Parser.AST.Core                            (ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.ConstructionData.Core               (ConstructionData,
                                                                  addGVar,
                                                                  addGVarWith,
                                                                  addLVar,
                                                                  addLiteral,
                                                                  addFunction)
import           Htcc.Parser.ConstructionData.Scope.ManagedScope (ASTError)
import           Htcc.Parser.ConstructionData.Scope.Var (GVarInitWith)
import qualified Htcc.Tokenizer.Token                            as HT
import qualified Text.Megaparsec                                 as M

maybeToParser :: String -> Maybe ~> Parser i
maybeToParser s = maybe (fail s) pure

type PureAdder i = CT.StorageClass i
    -> HT.TokenLC i
    -> ConstructionData i
    -> Either (ASTError i) (ATree i, ConstructionData i)

registerVar :: (Bits i, Integral i)
    => PureAdder i
    -> CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerVar adder ty ident = gets (adder ty (tmpTKIdent ident))
    >>= \case
        Right (lat, scp') -> lat <$ put scp'
        Left err -> fail $ T.unpack $ fst err

registerLVar :: (Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerLVar = registerVar addLVar

registerStringLiteral :: (Bits i, Integral i)
    => String
    -> Parser i (ATree i)
registerStringLiteral s = gets (addLiteral ty (HT.TokenLCNums 1 1, HT.TKString $ BSU.fromString s))
    >>= \case
        Right (n, scp) -> n <$ put scp
        Left err -> fail $ T.unpack $ fst err
    where
        ty = CT.SCAuto $ CT.CTArray (fromIntegral $ length s) CT.CTChar

registerGVar :: (Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> Parser i (ATree i)
registerGVar = registerVar addGVar

registerGVarWith :: (Bits i, Integral i)
    => CT.StorageClass i
    -> T.Text
    -> GVarInitWith i
    -> Parser i (ATree i)
registerGVarWith ty ident to = gets (addGVarWith ty (tmpTKIdent ident) to)
    >>= \case
        Right (_, scp) -> ATEmpty <$ put scp
        Left err -> fail $ T.unpack $ fst err

registerFunc :: (Bits i, Integral i)
    => Bool
    -> CT.StorageClass i
    -> T.Text
    -> Parser i ()
registerFunc isDefined ty ident = gets (addFunction isDefined ty (tmpTKIdent ident))
    >>= \case
        Right scp -> put scp
        Left err -> fail $ T.unpack $ fst err

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
