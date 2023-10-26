{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module AutoInstrument.Internal.Types
  ( AutoInstrument(..)
  ) where

import           Data.Text

import qualified OpenTelemetry.Trace.Core as Otel hiding (inSpan, inSpan'')
import qualified OpenTelemetry.Trace.Monad as Otel
import           UnliftIO (MonadUnliftIO)

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

instance (Otel.MonadTracer m, MonadUnliftIO m)
    => AutoInstrument (m a) where
  autoInstrument funName modName filePath lineNum pkgName body =
    let attrs =
          [ (pack "code.function", Otel.toAttribute $ pack funName)
          , (pack "code.namespace", Otel.toAttribute $ pack modName)
          , (pack "code.filepath", Otel.toAttribute $ pack filePath)
          , (pack "code.lineno", Otel.toAttribute $ pack lineNum)
          , (pack "code.package", Otel.toAttribute $ pack pkgName)
          ]
#if MIN_VERSION_hs_opentelemetry_api(0,1,0)
        spanArgs = Otel.addAttributesToSpanArguments attrs Otel.defaultSpanArguments
     in Otel.inSpan (pack funName) spanArgs body
#else
        spanArgs = Otel.defaultSpanArguments { Otel.attributes = attrs }
     in Otel.inSpan'' [] (pack funName) spanArgs (const body)
#endif
