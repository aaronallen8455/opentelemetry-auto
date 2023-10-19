{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module AutoInstrument.Internal.Config
  ( Config(..)
  , Target(..)
  , ConArg(..)
  , PredArg(..)
  , readConfigFile
  , getConfigFilePath
  , defaultConfigFile
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Set (Set)
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import qualified Text.ParserCombinators.ReadP as P

import qualified AutoInstrument.Internal.GhcFacade as Ghc

newtype Config = MkConfig { targets :: [Target] }

data Target
  = Constructor String [ConArg]
  | Constraints ConstraintSet [PredArg]

type Target' = [TargetAtom]

data TargetAtom
  = Exact String
  | Paren Target'
  | Wildcard
  deriving Show

targetParser :: P.ReadP Target'
targetParser = P.skipSpaces *> P.many1 atomParser <* P.eof

atomParser :: P.ReadP TargetAtom
atomParser = parseExact <|> parseParen <|> parseWildcard
  where
    parseWildcard = Wildcard <$ P.char '_' <* P.skipSpaces
    parseExact = Exact <$>
      P.munch1 (\c -> c `notElem` [' ', '(', ')', '_'])
        <* P.skipSpaces
    parseParen = Paren <$>
      P.between (P.char '(' <* P.skipSpaces) (P.char ')')
        (P.many1 atomParser)
        <* P.skipSpaces

type ConstraintSet = Set String

data ConArg
  = ConWildcard
  | ConArg BS.ByteString

instance FromJSON ConArg where
  parseJSON = withText "ConArg" $ \case
    "_" -> pure ConWildcard
    other -> pure . ConArg $ T.encodeUtf8 other

data PredArg
  = PredResult
  | PredWildcard
  | PredArg BS.ByteString

instance FromJSON PredArg where
  parseJSON = withText "PredArg" $ \case
    "_" -> pure PredWildcard
    "#" -> pure PredResult
    other -> pure . PredArg $ T.encodeUtf8 other

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    MkConfig <$> obj .: "targets"

instance FromJSON Target where
  parseJSON = withObject "Target" $ \obj -> do
    tag <- obj .: "type"
    case tag of
      "constructor" -> Constructor <$> obj .: "value" <*> obj .:? "args" .!= []
      "constraints" -> Constraints <$> obj .: "value" <*> obj .:? "args" .!= []
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
