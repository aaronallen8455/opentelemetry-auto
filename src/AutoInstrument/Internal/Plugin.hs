module AutoInstrument.Internal.Plugin
  ( plugin
  ) where

import qualified System.Directory as Dir

import qualified AutoInstrument.Internal.GhcFacade as Ghc
import qualified AutoInstrument.Internal.Plugin.Parser as Parser
import qualified AutoInstrument.Internal.Plugin.TypeChecker as TypeChecker

defaultConfigFile :: FilePath
defaultConfigFile = "auto_instrument_config.json"

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = pluginRecompile
--  , Ghc.tcPlugin = TypeChecker.tcPlugin
  , Ghc.parsedResultAction = Parser.parsedResultAction
  }

pluginRecompile :: [Ghc.CommandLineOption] -> IO Ghc.PluginRecompile
pluginRecompile opts = do
  let config = getConfigFilePath opts
  exists <- Dir.doesFileExist config
  if exists
     then Ghc.MaybeRecompile <$> Ghc.getFileHash config
     else do
       putStrLn "================================================================="
       putStrLn "Auto instrument config not found! The plugin will have no effect."
       putStrLn "================================================================="
       pure Ghc.NoForceRecompile

getConfigFilePath :: [Ghc.CommandLineOption] -> FilePath
getConfigFilePath (opt : _) = opt
getConfigFilePath [] = defaultConfigFile
