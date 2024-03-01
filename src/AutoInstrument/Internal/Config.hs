{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.Config
  ( Config(..)
  , ConfigCache(..)
  , Target(..)
  , getConfigCache
  , getConfigFilePath
  , defaultConfigFile
  , TargetCon(..)
  , ConstraintSet
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar
import           Data.IORef
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import           Data.Time
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.String as P
import qualified Toml.Schema.FromValue as Toml
import qualified Toml

import qualified AutoInstrument.Internal.GhcFacade as Ghc

data ConfigCache = MkConfigCache
  { timestamp :: !UTCTime
  , getConfig :: !Config
  , fingerprint :: !Ghc.Fingerprint
  }

configCache :: IORef (Maybe ConfigCache)
configCache = unsafePerformIO $ newIORef Nothing
{-# NOINLINE configCache #-}

-- | Used to ensure that the config file is only read by one thread when the
-- cache expires or needs to be initialized.
semaphore :: MVar ()
semaphore = unsafePerformIO $ newMVar ()
{-# NOINLINE semaphore #-}

-- Cache expires after 20 seconds
cacheDuration :: NominalDiffTime
cacheDuration = 20

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

getConfigCache :: [Ghc.CommandLineOption] -> IO (Maybe ConfigCache)
getConfigCache opts = do
  mCached <- readIORef configCache
  case mCached of
    Nothing -> getConfigOrRefresh opts
    Just cached -> do
      expired <- isCacheExpired cached
      if expired
      then getConfigOrRefresh opts
      else pure $ Just cached

-- | This blocks on the MVar to ensure that the file is only read by one thread when necessary.
-- contention for the MVar only occurs when the cache expires or hasn't been initialized.
getConfigOrRefresh :: [Ghc.CommandLineOption] -> IO (Maybe ConfigCache)
getConfigOrRefresh opts = do
  withMVar semaphore $ \_ -> do
    mCached <- readIORef configCache
    case mCached of
      Nothing -> refreshConfigCache opts
      Just existing -> do
        expired <- isCacheExpired existing
        if expired
        then refreshConfigCache opts
        else pure $ Just existing

isCacheExpired :: ConfigCache -> IO Bool
isCacheExpired cached = do
  now <- getCurrentTime
  let diff = diffUTCTime now $ timestamp cached
  pure $ diff >= cacheDuration

refreshConfigCache :: [Ghc.CommandLineOption] -> IO (Maybe ConfigCache)
refreshConfigCache opts = do
  newCache <- mkConfigCache opts
  writeIORef configCache newCache
  pure newCache

mkConfigCache :: [Ghc.CommandLineOption] -> IO (Maybe ConfigCache)
mkConfigCache opts = do
  let cfgFile = getConfigFilePath opts
  exists <- Dir.doesFileExist cfgFile
  if exists
     then do
       result <- Toml.decode <$> T.readFile cfgFile
       case result of
         Toml.Success _ config -> do
           now <- getCurrentTime
           fp <- Ghc.getFileHash cfgFile
           pure $ Just MkConfigCache
             { timestamp = now
             , getConfig = config
             , fingerprint = fp
             }
         Toml.Failure errs -> do
           putStr $ unlines
            $ "================================================================================"
            : "Failed to parse auto instrument config file:"
            : errs
            ++ ["================================================================================"]
           pure Nothing
     else do
       pure Nothing

getConfigFilePath :: [Ghc.CommandLineOption] -> FilePath
getConfigFilePath (opt : _) = opt
getConfigFilePath [] = defaultConfigFile

defaultConfigFile :: FilePath
defaultConfigFile = "auto-instrument-config.toml"
