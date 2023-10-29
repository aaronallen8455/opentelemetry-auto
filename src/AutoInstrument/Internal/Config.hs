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
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified System.Directory as Dir
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.String as P
import qualified Toml.FromValue as Toml
import qualified Toml

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

skipSpaces :: P.Parser ()
skipSpaces = P.skipMany P.space

targetParser :: P.Parser TargetCon
targetParser = appP
  where
    appP = P.chainl1 (P.try unitP <|> varP <|> parenP) (pure App) <* skipSpaces
    unitP = Unit <$ P.string "()" <* skipSpaces
    varP = do
      v <- P.many1 (P.satisfy $ \c -> c `notElem` [' ', '(', ')', ',']) <* skipSpaces
      case v of
        "_" -> pure WC
        _ -> pure $ TyVar v
    parenP = do
      inParens <-
        P.between (P.char '(' <* skipSpaces) (P.char ')')
          (P.sepBy1 targetParser (P.char ',' <* skipSpaces))
          <* skipSpaces
      case inParens of
        [t] -> pure t
        _ -> pure $ Tuple inParens

type ConstraintSet = Set TargetCon

instance Toml.FromValue Config where
  fromValue = Toml.parseTableFromValue $
    MkConfig
      <$> Toml.reqKey "targets"
      <*> (fromMaybe [] <$> Toml.optKey "exclusions")

instance Toml.FromValue Target where
  fromValue = Toml.parseTableFromValue $ do
    tag <- Toml.reqKey "type"
    case tag of
      "constructor" -> do
        value <- Toml.reqKey "value"
        case P.parse (skipSpaces *> targetParser <* P.eof) "" value of
          Right target -> pure $ Constructor target
          Left err -> fail $ showParsecError err
      "constraints" -> do
        value <- Toml.reqKey "value"
        let parsePred v =
              case P.parse (skipSpaces *> targetParser <* P.eof) "" v of
                Right target -> pure target
                Left err -> fail $ showParsecError err
        Constraints . S.fromList <$> traverse parsePred value
      _ -> fail $ "Unrecognized targed type: " <> tag

-- | Doesn't show the source location
showParsecError :: P.ParseError -> String
showParsecError
  = drop 1
  . P.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
  . P.errorMessages

readConfigFile :: [Ghc.CommandLineOption] -> IO (Maybe Config)
readConfigFile opts = do
  let cfgFile = getConfigFilePath opts
  exists <- Dir.doesFileExist cfgFile
  if exists
     then do
       result <- Toml.decode <$> readFile cfgFile
       case result of
         Toml.Success _ config -> pure $ Just config
         Toml.Failure errs -> do
           putStr $ unlines
            $ "================================================================================"
            : "Failed to parse auto instrument config file:"
            : errs
            ++ ["================================================================================"]
           pure Nothing
     else do
       putStr $ unlines
        [ "================================================================================"
        , "Auto instrument config not found! The plugin will have no effect."
        , "================================================================================"
        ]
       pure Nothing

getConfigFilePath :: [Ghc.CommandLineOption] -> FilePath
getConfigFilePath (opt : _) = opt
getConfigFilePath [] = defaultConfigFile

defaultConfigFile :: FilePath
defaultConfigFile = "auto-instrument-config.toml"
