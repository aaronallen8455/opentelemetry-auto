{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import           Data.Text
import           GHC.Stack (HasCallStack, callStack, popCallStack)

import qualified OpenTelemetry.Trace.Core as Otel hiding (inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel
import           UnliftIO (MonadUnliftIO)

class AutoInstrument a where
  autoInstrument :: HasCallStack => String -> a -> a

instance AutoInstrument b => AutoInstrument (a -> b) where
  autoInstrument name f =
    let ?callStack = popCallStack callStack -- don't include autoInstrument in the call stack
     in autoInstrument name . f

instance (Otel.MonadTracer m, MonadUnliftIO m) => AutoInstrument (m a) where
  autoInstrument name =
    let ?callStack = popCallStack callStack
     in Otel.inSpan (pack name) Otel.defaultSpanArguments
