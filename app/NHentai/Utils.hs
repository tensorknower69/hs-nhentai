{-# LANGUAGE LambdaCase #-}

module NHentai.Utils where

import Control.Error
import Control.Exception hiding (catch, mask)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.List
import Data.List.Split
import Data.Time.Clock.POSIX
import Options.Applicative
import Options.Applicative.Types
import Refined
import Streaming as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as L
import qualified Streaming.Internal as S

-- i honestly think that this is not a good idea
instance (Functor f, MonadThrow m) => MonadThrow (Stream f m) where
	throwM = lift . throwM

-- https://hackage.haskell.org/package/streaming-0.2.3.0/docs/src/Streaming.Internal.html#line-381
instance (Functor f, MonadCatch m) => MonadCatch (Stream f m) where
	stream `catch` f = loop stream
		where
		loop x = case x of
			S.Return r -> S.Return r
			S.Effect m -> S.Effect $ fmap loop m `catch` (pure . f)
			S.Step g -> S.Step (fmap loop g)

instance (Functor f, MonadLogger m) => MonadLogger (Stream f m)
instance (Functor f, MonadLoggerIO m) => MonadLoggerIO (Stream f m)

data ScalpelException
	= ScalpelException
		{ input'ScalpelException :: BL.ByteString
		}
	deriving (Show, Eq)
instance Exception ScalpelException

data AesonParseException = AesonParseException String
	deriving (Show, Eq)
instance Exception AesonParseException

data NHentaiException = NHentaiNoGalleryException
	deriving (Show, Eq)
instance Exception NHentaiException

data ReadException = ReadException { input'ReadException :: String, part'ReadException :: String }
	deriving (Show, Eq)
instance Exception ReadException

refineReadM :: (Read x, Predicate p x) => ReadM (Refined p x)
refineReadM = eitherReader $ \string -> do
	case readMay string of
		Nothing -> Left $ "unable to parse string: " <> show string
		Just x -> refine x & _Left %~ show

listReadM :: ReadM x -> ReadM [x]
listReadM (ReadM reader') = ReadM . ReaderT $ traverse (runReaderT reader') . splitOn ","

nonEmptyReadM :: ReadM x -> ReadM (L.NonEmpty x)
nonEmptyReadM readm = do
	(L.nonEmpty <$> listReadM readm) >>= \case
		Nothing -> fail "parsed list is empty"
		Just x -> pure x

logLevelReadM :: ReadM LogLevel
logLevelReadM = eitherReader $ \string -> do
	case lookup string my_map of
		Nothing -> Left $ "must be one of: " <> (intercalate ", " $ fmap fst my_map) <> ", but received: " <> show string
		Just x -> pure x
	where
	my_map =
		[ ("debug", LevelDebug)
		, ("info", LevelInfo)
		, ("warn", LevelWarn)
		, ("error", LevelError)
		]

withTimer :: MonadIO m => m a -> m (POSIXTime, a)
withTimer f = do
	t <- liftIO getPOSIXTime
	a <- f
	t' <- liftIO getPOSIXTime
	pure (t' - t, a)
