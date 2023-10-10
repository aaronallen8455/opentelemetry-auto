{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.Config
  ( Config(..)
  , UserConfig
  , PluginConfig
  , Instrumentation(..)
  , Instrumentor(..)
  , ConstraintSet
  ) where

import           Data.Aeson
import           Data.Set (Set)

import qualified AutoInstrument.Internal.GhcFacade as Ghc

type UserConfig = Config Instrumentor
type PluginConfig = Config Ghc.Name

newtype Config instrumentor = MkConfig
  { instrumentations :: [Instrumentation instrumentor]
  } deriving (Functor, Foldable, Traversable)

data Instrumentation instrumentor = MkInstrumentation
  { instrumentor :: instrumentor
  , target :: Maybe Target
  } deriving (Functor, Foldable, Traversable)

data Target
  = Constructor String
  | Constraints ConstraintSet

type ConstraintSet = Set String

data Instrumentor = MkInstrumentor
  { functionName :: String
  , moduleName :: String
  , packageName :: String
  }

instance FromJSON (Config Instrumentor) where
  parseJSON = withObject "Config" $ \obj ->
    MkConfig <$> obj .: "instrumentations"

instance FromJSON (Instrumentation Instrumentor) where
  parseJSON = withObject "Instrumentation" $ \obj ->
    MkInstrumentation
    <$> obj .: "instrumentor"
    <*> obj .:? "target"

instance FromJSON Target where
  parseJSON = withObject "Target" $ \obj -> do
    tag <- obj .: "type"
    case tag of
      "constructor" -> Constructor <$> obj .: "value"
      "constraints" -> Constraints <$> obj .: "value"
      _ -> fail $ "Unrecognized targed type: " <> tag

instance FromJSON Instrumentor where
  parseJSON = withObject "Instrumentor" $ \obj ->
    MkInstrumentor
    <$> obj .: "function"
    <*> obj .: "module"
    <*> obj .: "package"
