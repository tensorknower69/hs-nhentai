module NHentai.Utils where

import Control.Error
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.List
import Data.List.Split
import Options.Applicative
import Options.Applicative.Types
import Refined
import Streaming as S

-- i honestly think that this is not a good idea
instance (Functor f, MonadThrow m) => MonadThrow (Stream f m)
instance (Functor f, MonadLogger m) => MonadLogger (Stream f m)
instance (Functor f, MonadLoggerIO m) => MonadLoggerIO (Stream f m)

data ScalpelException = ScalpelException
	deriving (Show, Eq)
instance Exception ScalpelException

data AesonParseException = AesonParseException String
	deriving (Show, Eq)
instance Exception AesonParseException

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
