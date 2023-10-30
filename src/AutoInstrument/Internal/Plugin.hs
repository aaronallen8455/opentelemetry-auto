module AutoInstrument.Internal.Plugin
  ( plugin
  ) where

import qualified AutoInstrument.Internal.Config as Cfg
import qualified AutoInstrument.Internal.GhcFacade as Ghc
import qualified AutoInstrument.Internal.Plugin.Parser as Parser

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = pluginRecompile
  , Ghc.parsedResultAction = Parser.parsedResultAction
  }

pluginRecompile :: [Ghc.CommandLineOption] -> IO Ghc.PluginRecompile
pluginRecompile opts = do
  mCache <- Cfg.getConfigCache opts
  case mCache of
    Nothing -> pure Ghc.NoForceRecompile
    Just cache ->
     pure . Ghc.MaybeRecompile $ Cfg.fingerprint cache
