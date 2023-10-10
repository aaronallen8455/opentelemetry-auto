module AutoInstrument.Internal.Plugin.TypeChecker
  ( tcPlugin
  ) where

import qualified AutoInstrument.Internal.GhcFacade as Ghc
import qualified GHC.TcPlugin.API as Tc

tcPlugin :: Ghc.TcPlugin
tcPlugin opts = Just $ Tc.mkTcPlugin Tc.TcPlugin
  { Tc.tcPluginInit = undefined opts
  , Tc.tcPluginSolve = undefined
  , Tc.tcPluginRewrite = mempty
  , Tc.tcPluginStop = const $ pure ()
  }
