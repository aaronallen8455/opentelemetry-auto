module AutoInstrument.Internal.Otel where

import           Data.Text
import           GHC.Stack (HasCallStack)
import qualified OpenTelemetry.Trace.Core as Otel hiding (inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel
import           UnliftIO (MonadUnliftIO)

inSpan
  :: (MonadUnliftIO m, Otel.MonadTracer m, HasCallStack)
  => Text
  -> m a
  -> m a
inSpan name = Otel.inSpan name Otel.defaultSpanArguments
