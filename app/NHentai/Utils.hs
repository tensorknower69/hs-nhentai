module NHentai.Utils where

import Control.Error
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
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
		Nothing -> Left $ "unable to parse string: " <> string
		Just x -> refine x & _Left %~ show

listReadM :: ReadM x -> ReadM [x]
listReadM (ReadM reader') = ReadM . ReaderT $ traverse (runReaderT reader') . splitOn ","

