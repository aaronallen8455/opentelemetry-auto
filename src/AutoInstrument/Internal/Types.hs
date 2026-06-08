{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import qualified Data.Text as T
import           UnliftIO

import           OpenTelemetry.Attributes.Key (unkey)
import qualified OpenTelemetry.Propagator as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.SemanticsConfig as Otel
import qualified OpenTelemetry.SemanticConventions as SC

class AutoInstrument a where
  autoInstrument
    :: String -- function name
    -> String -- module name
    -> String -- file path
    -> String -- line number
    -> a -> a

instance {-# INCOHERENT #-} AutoInstrument b => AutoInstrument (a -> b) where
  autoInstrument funName modName filePath lineNum f =
    autoInstrument funName modName filePath lineNum . f

instance MonadUnliftIO m
    => AutoInstrument (m a) where
  autoInstrument funName modName filePath lineNum body = do
    tp <- Otel.getGlobalTracerProvider
    semOpts <- liftIO Otel.getSemanticsOptions
    -- If the global tracer provider hasn't been initialized then there will
    -- be no propagators. Don't create a span if this is the case because if
    -- the function that initializes the tracer provider gets auto instrumented
    -- then its span will not emit traces and nor will its child spans.
    if null $ Otel.propagatorFields (Otel.getTracerProviderPropagators tp)
    then body -- no providers - don't create a span
    else
      -- TODO store this in a global var as an optimization? might not want to
      -- since the global tracer provider can potentially change.
      let tracer = Otel.makeTracer tp "hs-opentelemetry-instrumentation-auto" Otel.tracerOptions

          oldAttrs =
            [ (unkey SC.code_function, Otel.toAttribute $ T.pack funName)
            , (unkey SC.code_namespace, Otel.toAttribute $ T.pack modName)
            , (unkey SC.code_filepath, Otel.toAttribute $ T.pack filePath)
            , (unkey SC.code_lineno, Otel.toAttribute $ T.pack lineNum)
            ]
          stableAttrs =
            [ (unkey SC.code_function_name, Otel.toAttribute $ T.pack modName <> "." <> T.pack funName)
            , (unkey SC.code_file_path, Otel.toAttribute $ T.pack filePath)
            , (unkey SC.code_line_number, Otel.toAttribute $ T.pack lineNum)
            ]
          attrs = case Otel.codeOption semOpts of
            Otel.Stable -> stableAttrs
            Otel.Old -> oldAttrs
            Otel.StableAndOld -> stableAttrs <> oldAttrs
          spanArgs = Otel.addAttributesToSpanArguments attrs Otel.defaultSpanArguments
       in Otel.inSpan tracer (T.pack funName) spanArgs body
