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
) where
import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Trans.State    (gets, put)
import           Control.Natural              (type (~>))
import           Data.Bits                    (Bits (..))
import qualified Data.Text                    as T
import qualified Htcc.CRules.Types            as CT
import           Htcc.Parser.AST.Core         (ATree (..))
import           Htcc.Parser.Combinators.Core
import           Htcc.Parser.ConstructionData (addLVar)
import qualified Htcc.Tokenizer.Token         as HT
import qualified Text.Megaparsec              as M

maybeToParser :: String -> Maybe ~> Parser i
maybeToParser s = maybe (fail s) pure

registerLVar :: (Bits i, Integral i) => CT.StorageClass i -> T.Text -> Parser i (ATree i)
registerLVar ty ident = lift (gets $ addLVar ty (HT.TokenLCNums 1 1, HT.TKIdent ident))
    >>= \case
        Right (lat, scp') -> lift (lat <$ put scp')
        Left err          -> fail $ T.unpack $ fst err

bracket :: Parser i a -> (a -> Parser i b) -> (a -> Parser i c) -> Parser i c
bracket beg end m = do
    b <- beg
    M.withRecovery (\err -> end b *> M.parseError err) (m b) <* end b
