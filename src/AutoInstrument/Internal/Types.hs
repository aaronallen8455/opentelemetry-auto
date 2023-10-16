{-# LANGUAGE FlexibleInstances #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import           Data.Text
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified OpenTelemetry.Trace.Core as Otel hiding (inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel
import           UnliftIO (MonadUnliftIO)

class AutoInstrument a where
  autoInstrument :: HasCallStack => String -> a -> a

instance {-# OVERLAPPING #-} AutoInstrument b => AutoInstrument (a -> b) where
  autoInstrument name f = withFrozenCallStack $ autoInstrument name . f

instance {-# OVERLAPPABLE #-} (Otel.MonadTracer m, MonadUnliftIO m)
    => AutoInstrument (m a) where
  autoInstrument name f =
    -- freeze the callstack so that the callsite of inSpan is not added
    withFrozenCallStack $ Otel.inSpan (pack name) Otel.defaultSpanArguments f
