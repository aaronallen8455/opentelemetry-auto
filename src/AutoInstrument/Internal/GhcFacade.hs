{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoInstrument.Internal.GhcFacade
  ( module Ghc
  , mkParseError
  ) where

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Plugins as Ghc hiding (getHscEnv, putMsg, fatalErrorMsg, errorMsg, debugTraceMsg)
import           GHC.Fingerprint as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Driver.Main as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..))
import           GHC.Hs.Extension as Ghc
import           GHC.Parser.Annotation as Ghc (SrcSpanAnn'(..), SrcSpanAnnA, noAnn, noSrcSpanA, realSrcSpan)
import           GHC.Parser.Errors.Types as Ghc
import           GHC.Types.SourceText as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Utils.Error as Ghc
#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Plugins as Ghc hiding (getHscEnv, putMsg, fatalErrorMsg, errorMsg, debugTraceMsg)
import           GHC.Fingerprint as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Driver.Main as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..), HsModule(..))
import           GHC.Hs.Extension as Ghc
import           GHC.Parser.Annotation as Ghc (SrcSpanAnn'(..), SrcSpanAnnA, noAnn, noSrcSpanA, realSrcSpan)
import           GHC.Parser.Errors.Types as Ghc
import           GHC.Types.SourceText as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Utils.Error as Ghc
#endif

mkParseError :: String -> MsgEnvelope PsMessage
mkParseError
  = Ghc.mkPlainErrorMsgEnvelope (Ghc.mkGeneralSrcSpan "plugin")
  . Ghc.PsUnknownMessage
#if MIN_VERSION_ghc (9,6,0)
  . Ghc.UnknownDiagnostic
#endif
  . Ghc.mkPlainError Ghc.noHints
  . Ghc.text
