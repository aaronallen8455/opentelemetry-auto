{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import           Data.Kind (Constraint)
import           Data.Text (Text)
import qualified Data.HashMap.Strict as H
import           OpenTelemetry.Attributes
import qualified OpenTelemetry.Context as Context
import           OpenTelemetry.Context.ThreadLocal
#if MIN_VERSION_hs_opentelemetry_api(0,3,0)
import           OpenTelemetry.Exporter.InMemory.Span
#else
import           OpenTelemetry.Exporter.InMemory
#endif
import           OpenTelemetry.Trace
import           OpenTelemetry.Trace.Core
import           OpenTelemetry.Trace.Sampler
import           Test.Tasty
import           Test.Tasty.HUnit
import           UnliftIO hiding (getChanContents)

data SpanInfo = SpanInfo
  { name :: Text
  , parentName :: Maybe Text
  , attrs :: H.HashMap Text Attribute
  } deriving (Show, Eq)

mkSpanInfo :: ImmutableSpan -> IO SpanInfo
mkSpanInfo s = do
  parentSpan <- traverse unsafeReadSpan $ spanParent s
  pure SpanInfo
    { name = spanName s
    , parentName = spanName <$> parentSpan
#if MIN_VERSION_hs_opentelemetry_api(0,3,0)
    , attrs = H.delete "thread.id" . getAttributeMap $ spanAttributes s
#else
    , attrs = H.delete "thread.id" . snd . getAttributes $ spanAttributes s
#endif
    }

withGlobalTracing :: (OutChan ImmutableSpan -> IO a) -> IO a
withGlobalTracing act = do
  _ <- attachContext Context.empty
  bracket
    initializeTracing
    (shutdownTracerProvider . fst)
    (\(_, ref) -> act ref)

initializeTracing :: IO (TracerProvider, OutChan ImmutableSpan)
initializeTracing = do
  (_, tracerOptions') <- getTracerProviderInitializationOptions

  (inMemoryProc, spansChan) <- inMemoryChannelExporter
  let processors' = [inMemoryProc]

  provider <-
    createTracerProvider
      processors'
      tracerOptions' {tracerProviderOptionsSampler = alwaysOn}
  setGlobalTracerProvider provider

  pure (provider, spansChan)

type Instrumented a = IO a

t1 :: Instrumented ()
t1 = do
  t2
  t3
  t2

t2 :: Instrumented ()
t2 = pure ()

t3 :: IO ()
t3 = pure ()

type NotInstrumented = IO

t4 :: NotInstrumented ()
t4 = t2

type InstrumentC :: Constraint
type InstrumentC = ()

t5 :: InstrumentC => IO ()
t5 = t2

type NoInstrumentC :: Constraint
type NoInstrumentC = ()

t6 :: NoInstrumentC => Instrumented ()
t6 = t2

type Partial a b = IO b

t7 :: Partial Bool ()
t7 = t2

t8 :: Partial () ()
t8 = t2

type WildCard = IO
type WildCardX = IO

t9 :: WildCard (Maybe Bool)
t9 = pure Nothing

t10 :: WildCard ()
t10 = pure ()

t11 :: WildCardX (Maybe Bool)
t11 = pure Nothing

type C1 :: Constraint
type C1 = ()

type C2 :: Constraint
type C2 = ()

t12 :: C1 => IO ()
t12 = pure ()

t13 :: (C1, C2) => IO ()
t13 = pure ()

type X1 :: Constraint
type X1 = ()

type X2 :: Constraint
type X2 = ()

t14 :: (C1, X1, C2, X2) => IO ()
t14 = pure ()

t15 :: (X1, X2) => Instrumented ()
t15 = pure ()

t16 :: X2 => Instrumented ()
t16 = pure ()

t17 :: a -> Instrumented a
t17 = pure

main :: IO ()
main =
  withGlobalTracing $ \spansChan -> do
    defaultMain (testTree spansChan)

testTree :: OutChan ImmutableSpan -> TestTree
testTree spansChan = testGroup "Tests"
  [ testCase "nested spans" (nestedSpans spansChan)
  , testCase "ignore excluded constructor" (excludedCon spansChan)
  , testCase "simple constraint rule" (simpleConstraint spansChan)
  , testCase "ignore excluded constraint" (excludeConstraint spansChan)
  , testCase "partially applied constructor rule" (partialCon spansChan)
  , testCase "rule with wildcard placeholder" (wildCard spansChan)
  , testCase "multi constraint rule" (multiPred spansChan)
  , testCase "multi constraint exclusion" (multiPredX spansChan)
  , testCase "point-free" (pointFree spansChan)
  ]

nestedSpans :: OutChan ImmutableSpan -> Assertion
nestedSpans spansChan = do
  t1
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "76" "t2" (Just "t1")
    , spanInfo "76" "t2" (Just "t1")
    , spanInfo "70" "t1" Nothing
    ]

excludedCon :: OutChan ImmutableSpan -> Assertion
excludedCon spansChan = do
  t4
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "76" "t2" Nothing
    ]

simpleConstraint :: OutChan ImmutableSpan -> Assertion
simpleConstraint spansChan = do
  t5
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "76" "t2" (Just "t5")
    , spanInfo "90" "t5" Nothing
    ]

excludeConstraint :: OutChan ImmutableSpan -> Assertion
excludeConstraint spansChan = do
  t6
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "76" "t2" Nothing ]

partialCon :: OutChan ImmutableSpan -> Assertion
partialCon spansChan = do
  t7
  t8
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "76" "t2" Nothing
    , spanInfo "76" "t2" (Just "t8")
    , spanInfo "104" "t8" Nothing
    ]

wildCard :: OutChan ImmutableSpan -> Assertion
wildCard spansChan = do
  _ <- t9
  t10
  _ <- t11
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "110" "t9" Nothing ]

multiPred :: OutChan ImmutableSpan -> Assertion
multiPred spansChan = do
  t12
  t13
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "128" "t13" Nothing ]

multiPredX :: OutChan ImmutableSpan -> Assertion
multiPredX spansChan = do
  t14
  t15
  t16
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "143" "t16" Nothing ]

pointFree :: OutChan ImmutableSpan -> Assertion
pointFree spansChan = do
  t17 ()
  spans <- getSpans spansChan
  spans @?=
    [ spanInfo "146" "t17" Nothing ]

spanInfo :: Text -> Text -> Maybe Text -> SpanInfo
spanInfo lineNo funName mParentName =
  SpanInfo
    { name = funName
    , parentName = mParentName
    , attrs =
      [ ("code.lineno", AttributeValue (TextAttribute lineNo))
      , ("code.filepath", AttributeValue (TextAttribute "test/Main.hs"))
      , ("code.function", AttributeValue (TextAttribute funName))
      , ("code.namespace", AttributeValue (TextAttribute "Main"))
#if MIN_VERSION_hs_opentelemetry_api(0,3,0)
      , ("code.package", AttributeValue (TextAttribute "hs-opentelemetry-instrumentation-auto-0.1.0.3-inplace-auto-instrument-test"))
#else
      , ("code.package", AttributeValue (TextAttribute "hs-opentelemetry-instrumentation-auto-0.1.0.2-inplace-auto-instrument-test"))
#endif
      ]
    }

getSpans :: OutChan ImmutableSpan -> IO [SpanInfo]
getSpans chan = do
  (element, _) <- tryReadChan chan
  tryRead element >>= \case
    Nothing -> do
      addPlaceholderSpan
      pure []
    Just e -> do
      si <- mkSpanInfo e
      (si :) <$> getSpans chan

addPlaceholderSpan :: IO ()
addPlaceholderSpan = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp "test" tracerOptions
  inSpan tracer "_placeholder_" defaultSpanArguments $ pure ()
