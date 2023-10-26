{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Trans.Reader
import           UnliftIO
import qualified OpenTelemetry.Trace as Otel
import qualified OpenTelemetry.Trace.Monad as Otel

main :: IO ()
main = bracket
  Otel.initializeGlobalTracerProvider
  Otel.shutdownTracerProvider
  (\tracerProvider -> do
    let tracer = Otel.makeTracer tracerProvider "main" Otel.tracerOptions
    runReaderT (unTest blah) tracer
    foo 1
  )

blah :: Test Bool ()
blah = Test $ pure ()

foo :: (Ord a, Eq a, Read a) => a -> IO ()
foo = const $ pure ()

boo :: (Otel.MonadTracer m, MonadUnliftIO m) => m ()
boo = pure ()

newtype Test b a = Test { unTest :: ReaderT Otel.Tracer IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance Otel.MonadTracer (Test Bool) where
  getTracer = Test ask
