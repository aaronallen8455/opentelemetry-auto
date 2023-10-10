{-# LANGUAGE FlexibleInstances #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import           Data.Text

import qualified OpenTelemetry.Trace.Core as Otel hiding (inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel
import           UnliftIO (MonadUnliftIO)

class AutoInstrument a where
  autoInstrument :: Text -> a -> a

instance AutoInstrument b => AutoInstrument (a -> b) where
  autoInstrument name f = autoInstrument name . f

instance (Otel.MonadTracer m, MonadUnliftIO m) => AutoInstrument (m a) where
  autoInstrument name = Otel.inSpan name Otel.defaultSpanArguments
