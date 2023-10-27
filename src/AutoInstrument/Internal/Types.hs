{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import           Data.Text
import           UnliftIO

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
    -- TODO store this in a global var as an optimization? might not want to
    -- since the global tracer provider can potentially change.
    let tracer = Otel.makeTracer tp "hs-opentelemetry-instrumentation-auto" Otel.tracerOptions

    let attrs =
          [ (pack "code.function", Otel.toAttribute $ pack funName)
          , (pack "code.namespace", Otel.toAttribute $ pack modName)
          , (pack "code.filepath", Otel.toAttribute $ pack filePath)
          , (pack "code.lineno", Otel.toAttribute $ pack lineNum)
          , (pack "code.package", Otel.toAttribute $ pack pkgName)
          ]
#if MIN_VERSION_hs_opentelemetry_api(0,1,0)
        spanArgs = Otel.addAttributesToSpanArguments attrs Otel.defaultSpanArguments
     in Otel.inSpan tracer (pack funName) spanArgs body
#else
        spanArgs = Otel.defaultSpanArguments { Otel.attributes = attrs }
     in Otel.inSpan'' tracer [] (pack funName) spanArgs (const body)
#endif
