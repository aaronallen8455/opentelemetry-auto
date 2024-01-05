{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import qualified Data.Text as T
import           UnliftIO

import qualified OpenTelemetry.Propagator as Otel
import qualified OpenTelemetry.Trace.Core as Otel

class AutoInstrument a where
  autoInstrument
    :: String -- function name
    -> String -- module name
    -> String -- file path
    -> String -- line number
    -> String -- package name
    -> a -> a

instance {-# INCOHERENT #-} AutoInstrument b => AutoInstrument (a -> b) where
  autoInstrument funName modName filePath lineNum pkgName f =
    autoInstrument funName modName filePath lineNum pkgName . f

instance MonadUnliftIO m
    => AutoInstrument (m a) where
  autoInstrument funName modName filePath lineNum pkgName body = do
    tp <- Otel.getGlobalTracerProvider
    -- If the global tracer provider hasn't been initialized then there will
    -- be no propagators. Don't create a span if this is the case because if
    -- the function that initializes the tracer provider gets auto instrumented
    -- then its span will not emit traces and nor will its child spans.
    if null $ Otel.propagatorNames (Otel.getTracerProviderPropagators tp)
    then body -- no providers - don't create a span
    else
      -- TODO store this in a global var as an optimization? might not want to
      -- since the global tracer provider can potentially change.
      let tracer = Otel.makeTracer tp "hs-opentelemetry-instrumentation-auto" Otel.tracerOptions

          attrs =
            [ (T.pack "code.function", Otel.toAttribute $ T.pack funName)
            , (T.pack "code.namespace", Otel.toAttribute $ T.pack modName)
            , (T.pack "code.filepath", Otel.toAttribute $ T.pack filePath)
            , (T.pack "code.lineno", Otel.toAttribute $ T.pack lineNum)
            , (T.pack "code.package", Otel.toAttribute $ T.pack pkgName)
            ]
#if MIN_VERSION_hs_opentelemetry_api(0,1,0)
          spanArgs = Otel.addAttributesToSpanArguments attrs Otel.defaultSpanArguments
       in Otel.inSpan tracer (T.pack funName) spanArgs body
#else
          spanArgs = Otel.defaultSpanArguments { Otel.attributes = attrs }
       in Otel.inSpan'' tracer [] (T.pack funName) spanArgs (const body)
#endif
