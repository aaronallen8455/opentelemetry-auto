module AutoInstrument.Internal.GhcFacade
  ( module Ghc
  ) where

import           GHC.Plugins as Ghc hiding (getHscEnv)
import           GHC.Fingerprint as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Driver.Main as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..))
import           GHC.Hs.Extension as Ghc
import           GHC.Parser.Annotation as Ghc (SrcSpanAnnA, noAnn, noSrcSpanA)
import           GHC.Types.SourceText as Ghc
