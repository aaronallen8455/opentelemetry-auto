{-# LANGUAGE CPP #-}
module AutoInstrument.Internal.GhcFacade
  ( module Ghc
  ) where

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Plugins as Ghc hiding (getHscEnv)
import           GHC.Fingerprint as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Driver.Main as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..))
import           GHC.Hs.Extension as Ghc
import           GHC.Parser.Annotation as Ghc (SrcSpanAnnA, noAnn, noSrcSpanA, la2r)
import           GHC.Types.SourceText as Ghc
#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Plugins as Ghc hiding (getHscEnv)
import           GHC.Fingerprint as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Driver.Main as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..), HsModule(..))
import           GHC.Hs.Extension as Ghc
import           GHC.Parser.Annotation as Ghc (SrcSpanAnnA, noAnn, noSrcSpanA, la2r)
import           GHC.Types.SourceText as Ghc
#endif
