{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.Config
  ( Config(..)
  , Target(..)
  , readConfigFile
  , getConfigFilePath
  , defaultConfigFile
  ) where

import           Data.Aeson
import           Data.Set (Set)
import qualified System.Directory as Dir

import qualified AutoInstrument.Internal.GhcFacade as Ghc

newtype Config = MkConfig { targets :: [Target] }

data Target
  = Constructor String
  | Constraints ConstraintSet

type ConstraintSet = Set String

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    MkConfig <$> obj .: "targets"

instance FromJSON Target where
  parseJSON = withObject "Target" $ \obj -> do
    tag <- obj .: "type"
    case tag of
      "constructor" -> Constructor <$> obj .: "value"
      "constraints" -> Constraints <$> obj .: "value"
      _ -> fail $ "Unrecognized targed type: " <> tag

readConfigFile :: [Ghc.CommandLineOption] -> IO (Maybe Config)
readConfigFile opts = do
  let config = getConfigFilePath opts
  exists <- Dir.doesFileExist config
  if exists
     then decodeFileStrict config
     else do
       putStrLn "================================================================="
       putStrLn "Auto instrument config not found! The plugin will have no effect."
       putStrLn "================================================================="
       pure Nothing

getConfigFilePath :: [Ghc.CommandLineOption] -> FilePath
getConfigFilePath (opt : _) = opt
getConfigFilePath [] = defaultConfigFile

defaultConfigFile :: FilePath
defaultConfigFile = "auto_instrument_config.json"
