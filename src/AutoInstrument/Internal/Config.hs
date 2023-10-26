{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.Config
  ( Config(..)
  , Target(..)
  , readConfigFile
  , getConfigFilePath
  , defaultConfigFile
  , TargetCon(..)
  , ConstraintSet
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Set (Set)
import qualified Data.Set as S
import qualified System.Directory as Dir
import qualified Text.ParserCombinators.ReadP as P

import qualified AutoInstrument.Internal.GhcFacade as Ghc

-- Implementing constraint targets in such a way that the target of the constraint
-- is taken into account gets rather complicated. Currently constraints are matched
-- using the same process as simple constructor matching however this can result
-- in situations where the constraint matches but its subject type does not
-- appear in the result type where it would need to. A workaround is to allow
-- the user to define a list of exclusions so that those particular constructors
-- can be ignored even if the constraint context matches.

data Config = MkConfig
  { targets :: [Target]
  , exclusions :: [Target]
  }

data Target
  = Constructor TargetCon
  | Constraints ConstraintSet

data TargetCon
  = TyVar String
  | WC
  | App TargetCon TargetCon
  | Unit
  | Tuple [TargetCon]
  deriving (Show, Eq, Ord)

targetParser :: P.ReadP TargetCon
targetParser = appP
  where
    appP = P.chainl1 (unitP <|> varP <|> parenP) (pure App) <* P.skipSpaces
    unitP = Unit <$ P.string "()" <* P.skipSpaces
    varP = do
      v <- P.munch1 (\c -> c `notElem` [' ', '(', ')', ',']) <* P.skipSpaces
      case v of
        "_" -> pure WC
        _ -> pure $ TyVar v
    parenP = do
      inParens <-
        P.between (P.char '(' <* P.skipSpaces) (P.char ')')
          (P.sepBy1 targetParser (P.char ',' <* P.skipSpaces))
          <* P.skipSpaces
      case inParens of
        [t] -> pure t
        _ -> pure $ Tuple inParens

type ConstraintSet = Set TargetCon

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    MkConfig
      <$> obj .: "targets"
      <*> obj .: "exclusions"

instance FromJSON Target where
  parseJSON = withObject "Target" $ \obj -> do
    tag <- obj .: "type"
    case tag of
      "constructor" -> do
        value <- obj .: "value"
        case P.readP_to_S (P.skipSpaces *> targetParser <* P.eof) value of
          [(targets, "")] -> pure $ Constructor targets
          _ -> fail "failed to parse target"
      "constraints" -> do
        value <- obj .: "value"
        let parsePred v =
              case P.readP_to_S (P.skipSpaces *> targetParser <* P.eof) v of
                [(target, "")] -> pure target
                _ -> fail "failed to parse constraint target"
        Constraints . S.fromList <$> traverse parsePred value
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
defaultConfigFile = "auto-instrument-config.json"
