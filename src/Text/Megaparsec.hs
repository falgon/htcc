{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             LambdaCase, MultiParamTypeClasses, UndecidableInstances #-}
module Text.Megaparsec (
    Parsec,
    ParsecT (..),
    ParseErrorBundle (..),
    ParseError (..),
    ErrorFancy (..),
    PosState (..),
    ParserState (..),
    runParser,
    runParserT,
    errorBundlePretty,
    try,
    lookAhead,
    option,
    choice,
    many,
    manyTill,
    eof,
    between,
    takeWhileP,
    takeWhile1P,
    notFollowedBy,
    getInput,
    setInput,
    getSourcePos,
    getParserState,
    setParserState,
    withRecovery,
    parseError,
    empty,
    (<|>)
) where

import           Control.Applicative       (Alternative (..), many, (<|>))
import           Control.Monad             (MonadPlus, void)
import qualified Control.Monad.State.Class as MS
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Bool                 (bool)
import           Data.Foldable             (asum, toList)
import           Data.Functor.Identity     (Identity, runIdentity)
import           Data.List                 (intercalate)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Text.Parsec               as P
import qualified Text.Parsec.Error         as PE
import qualified Text.Parsec.Pos           as PP
import qualified Text.Parsec.Prim          as PPri

newtype ParsecT e s m a = ParsecT
    { unParsecT :: P.ParsecT s s m a
    }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

instance MonadTrans (ParsecT e s) where
    lift = ParsecT . lift

instance MS.MonadState st m => MS.MonadState st (ParsecT e s m) where
    get = lift MS.get
    put = lift . MS.put
    state = lift . MS.state

type Parsec e s = ParsecT e s Identity

newtype ErrorFancy e
    = ErrorFail String
    deriving (Eq, Ord, Show)

data ParseError s e
    = ParsecError Bool PE.ParseError
    | FancyError Int (S.Set (ErrorFancy e))
    deriving (Eq, Show)

data PosState s = PosState
    { pstateInput     :: s
    , pstateSourcePos :: PP.SourcePos
    }
    deriving (Eq, Show)

data ParserState s = ParserState
    { stateInput    :: s
    , statePosState :: PosState s
    }
    deriving (Eq, Show)

data ParseErrorBundle s e = ParseErrorBundle
    { bundleErrors   :: NonEmpty (ParseError s e)
    , bundlePosState :: PosState s
    }
    deriving (Eq)

instance Show (ParseErrorBundle T.Text e) where
    show = errorBundlePretty

runParser :: Parsec e T.Text a -> FilePath -> T.Text -> Either (ParseErrorBundle T.Text e) a
runParser p fp input = runIdentity $ runParserT p fp input

runParserT :: Monad m => ParsecT e T.Text m a -> FilePath -> T.Text -> m (Either (ParseErrorBundle T.Text e) a)
runParserT (ParsecT p) fp input =
    either (Left . toBundle input) Right <$>
        P.runParserT p input fp input
    where
        toBundle source err =
            ParseErrorBundle
                { bundleErrors = ParsecError False err :| []
                , bundlePosState = PosState source (PE.errorPos err)
                }

errorBundlePretty :: ParseErrorBundle T.Text e -> String
errorBundlePretty bundle =
    intercalate "\n\n" (fmap renderError $ toList $ bundleErrors bundle) <> "\n"
    where
        renderError = \case
            ParsecError _ err ->
                renderAt (PE.errorPos err) $
                    lines $
                        PE.showErrorMessages
                            "or"
                            "unknown parse error"
                            "expecting"
                            "unexpected"
                            "end of input"
                            (PE.errorMessages err)
            FancyError _ fancyErrors ->
                renderAt
                    (pstateSourcePos $ bundlePosState bundle)
                    [ msg
                    | ErrorFail msg <- S.toList fancyErrors
                    ]

        renderAt pos msgs =
            intercalate "\n" $
                [ renderLoc pos ]
                    <> maybe [] (\(srcLn, caretCol) -> [srcLn, replicate (pred caretCol) ' ' <> "^"]) (sourceLineAt pos)
                    <> msgs

        renderLoc pos =
            intercalate
                ":"
                [ PP.sourceName pos
                , show $ PP.sourceLine pos
                , show $ PP.sourceColumn pos
                ]

        sourceLineAt pos =
            let lineNo = fromIntegral (PP.sourceLine pos) - 1
                inputLines = T.splitOn (T.singleton '\n') $ pstateInput $ bundlePosState bundle
             in if lineNo < 0
                    then Nothing
                    else case drop lineNo inputLines of
                        srcLn : _ -> Just (T.unpack srcLn, fromIntegral $ PP.sourceColumn pos)
                        []        -> Nothing

try :: ParsecT e T.Text m a -> ParsecT e T.Text m a
try = ParsecT . P.try . unParsecT

lookAhead :: Monad m => ParsecT e T.Text m a -> ParsecT e T.Text m a
lookAhead = ParsecT . P.lookAhead . unParsecT

option :: Monad m => a -> ParsecT e T.Text m a -> ParsecT e T.Text m a
option x = ParsecT . P.option x . unParsecT

choice :: Alternative f => [f a] -> f a
choice = asum

manyTill :: Monad m => ParsecT e T.Text m a -> ParsecT e T.Text m end -> ParsecT e T.Text m [a]
manyTill p end = ParsecT $ P.manyTill (unParsecT p) (unParsecT end)

eof :: Monad m => ParsecT e T.Text m ()
eof = ParsecT P.eof

between :: Monad m => ParsecT e T.Text m open -> ParsecT e T.Text m close -> ParsecT e T.Text m a -> ParsecT e T.Text m a
between open close parser = ParsecT $
    P.between (unParsecT open) (unParsecT close) (unParsecT parser)

takeWhileP :: Monad m => Maybe String -> (Char -> Bool) -> ParsecT e T.Text m T.Text
takeWhileP _ predicate = ParsecT $ T.pack <$> P.many (P.satisfy predicate)

takeWhile1P :: Monad m => Maybe String -> (Char -> Bool) -> ParsecT e T.Text m T.Text
takeWhile1P _ predicate = ParsecT $ T.pack <$> P.many1 (P.satisfy predicate)

notFollowedBy :: Monad m => ParsecT e T.Text m a -> ParsecT e T.Text m ()
notFollowedBy parser = ParsecT $
    P.optionMaybe (P.try $ P.lookAhead $ unParsecT parser) >>= \case
        Just _ -> P.unexpected "unexpected trailing input"
        Nothing -> pure ()

getInput :: Monad m => ParsecT e T.Text m T.Text
getInput = ParsecT P.getInput

setInput :: Monad m => T.Text -> ParsecT e T.Text m ()
setInput = ParsecT . P.setInput

getSourcePos :: Monad m => ParsecT e T.Text m PP.SourcePos
getSourcePos = ParsecT P.getPosition

getParserState :: Monad m => ParsecT e T.Text m (ParserState T.Text)
getParserState = ParsecT $ do
    parserState <- PPri.getParserState
    pure $ ParserState
        { stateInput = PPri.stateInput parserState
        , statePosState = PosState
            { pstateInput = PPri.stateUser parserState
            , pstateSourcePos = PPri.statePos parserState
            }
        }

setParserState :: Monad m => ParserState T.Text -> ParsecT e T.Text m ()
setParserState ParserState { stateInput = input, statePosState = PosState { pstateInput = userInput, pstateSourcePos = sourcePos } } = ParsecT $
    void $ PPri.setParserState
        PPri.State
            { PPri.stateInput = input
            , PPri.statePos = sourcePos
            , PPri.stateUser = userInput
            }

withRecovery
    :: Monad m
    => (ParseError T.Text e -> ParsecT e T.Text m a)
    -> ParsecT e T.Text m a
    -> ParsecT e T.Text m a
withRecovery handler parser = ParsecT $ PPri.mkPT $ \state ->
    PPri.runParsecT (unParsecT parser) state >>= \case
        PPri.Consumed replyM -> pure . PPri.Consumed $ replyM >>= \case
            ok@(PPri.Ok _ _ _) -> pure ok
            PPri.Error err ->
                PPri.runParsecT (unParsecT $ handler $ ParsecError True err) (recoveryState err state) >>= \case
                    PPri.Consumed handledReply -> handledReply
                    PPri.Empty handledReply    -> handledReply
        PPri.Empty replyM -> replyM >>= \case
            ok@(PPri.Ok _ _ _) -> pure $ PPri.Empty (pure ok)
            PPri.Error err -> PPri.runParsecT (unParsecT $ handler $ ParsecError False err) (recoveryState err state)
    where
        recoveryState err parserState =
            parserState
                { PPri.stateInput = remainingInput (PPri.statePos parserState) (PPri.stateInput parserState)
                , PPri.statePos = PE.errorPos err
                }
            where
                targetPos = PE.errorPos err
                remainingInput pos input
                    | pos == targetPos = input
                    | otherwise =
                        maybe T.empty advance $ T.uncons input
                    where
                        advance (c, rest) = remainingInput (PP.updatePosChar pos c) rest

parseError :: Monad m => ParseError s e -> ParsecT e s m a
parseError = ParsecT . \case
    ParsecError consumed err -> PPri.mkPT $ \_ -> pure $
        bool
            (PPri.Empty $ pure $ PPri.Error err)
            (PPri.Consumed $ pure $ PPri.Error err)
            consumed
    FancyError _ fancyErrors -> fail $
        intercalate ", " [msg | ErrorFail msg <- S.toList fancyErrors]
