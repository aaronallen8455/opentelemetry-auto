{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module AutoInstrument.Internal.Plugin.Parser
  ( parsedResultAction
  ) where

import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Control.Monad.IO.Class (liftIO)

import qualified AutoInstrument.Internal.GhcFacade as Ghc
import qualified AutoInstrument.Internal.Config as Config

parsedResultAction
  :: [Ghc.CommandLineOption]
  -> Ghc.ModSummary
  -> Ghc.ParsedResult
  -> Ghc.Hsc Ghc.ParsedResult
parsedResultAction opts _modSummary
    parsedResult@Ghc.ParsedResult
      {Ghc.parsedResultModule = prm@Ghc.HsParsedModule
        {Ghc.hpm_module = Ghc.L modLoc mo@Ghc.HsModule{Ghc.hsmodDecls}}} = do
  hscEnv <- Ghc.getHscEnv
  result <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "AutoInstrument.Internal.Types") Ghc.NoPkgQual
  otelMod <-
    case result of
      Ghc.Found _ m -> pure m
      _ -> error "AutoInstrument.Internal.Types module not found"
  let occ = Ghc.mkVarOcc "autoInstrument"
  autoInstrumentName <- liftIO $ Ghc.lookupNameCache (Ghc.hsc_NC hscEnv) otelMod occ

  mConfig <- liftIO $ Config.readConfigFile opts

  case mConfig of
    Nothing -> pure parsedResult
    Just config -> do
      let targets = do
            -- TODO use constraint sets
            Config.Constructor target <- Config.targets config
            pure $ Ghc.mkFastString target
          matches = S.fromList
                  $ getMatches targets hsmodDecls

          newDecls = instrumentDecl autoInstrumentName matches <$> hsmodDecls

      -- Since using a parser, name shadowing is potentially a problem. could make
      -- a traversal that checks for all type sigs that are direct children of the
      -- current node, then find the definitions corresponding to those sigs.
      -- Perhaps the plugin should only consider type level definitions.

      pure parsedResult
        { Ghc.parsedResultModule = prm
          { Ghc.hpm_module = Ghc.L modLoc mo
            { Ghc.hsmodDecls = newDecls
            }
          }
        }

getMatches :: [Ghc.FastString] -> [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.OccName]
getMatches targets = concat . mapMaybe go where
  go (Ghc.L _ (Ghc.SigD _ (Ghc.TypeSig _ lhs (Ghc.HsWC _ (Ghc.L _ (Ghc.HsSig _ _ (Ghc.L _ ty)))))))
    | isTargetTy ty = Just (Ghc.rdrNameOcc . Ghc.unLoc <$> lhs)
  go _ = Nothing
  isTargetTy = \case
    Ghc.HsForAllTy _ _ (Ghc.L _ body) -> isTargetTy body
    Ghc.HsQualTy _ _ctx (Ghc.L _ body) -> isTargetTy body -- TODO use constraint context
    -- constraints will have to be carried to the return type to check that they
    -- are applied to it rather than some argument.
    -- Ghc.HsTyVar _ _ (Ghc.L _ _) -> True
    Ghc.HsAppTy _ (Ghc.L _ (Ghc.HsTyVar _ _ (Ghc.L _ constr))) (Ghc.L _ _inner) -- TODO look at rest of ty
      -> elem (Ghc.occNameFS (Ghc.rdrNameOcc constr)) targets
    -- Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ inner) -> True
    Ghc.HsFunTy _ _ _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsParTy _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsDocTy _ (Ghc.L _ nxt) _ -> isTargetTy nxt
    _ -> False

instrumentDecl :: Ghc.Name -> S.Set Ghc.OccName -> Ghc.LHsDecl Ghc.GhcPs -> Ghc.LHsDecl Ghc.GhcPs
instrumentDecl instrName targets
    (Ghc.L loc (Ghc.ValD vX (Ghc.FunBind fX funId (Ghc.MG mX (Ghc.L altsLoc alts)))))
  | Ghc.rdrNameOcc (Ghc.unLoc funId) `S.member` targets
  = let newAlts = (fmap . fmap) (instrumentMatch (Ghc.unLoc funId) instrName) alts
     in Ghc.L loc (Ghc.ValD vX (Ghc.FunBind fX funId (Ghc.MG mX (Ghc.L altsLoc newAlts))))
instrumentDecl _ _ x = x

instrumentMatch
  :: Ghc.RdrName
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
instrumentMatch bindName instrName match =
  match
    { Ghc.m_grhss = (Ghc.m_grhss match)
      { Ghc.grhssGRHSs = (fmap . fmap) modifyGRH (Ghc.grhssGRHSs (Ghc.m_grhss match))
      }
    }
  where
    modifyGRH :: Ghc.GRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
              -> Ghc.GRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
    modifyGRH (Ghc.GRHS x guards body) =
      Ghc.GRHS x guards (go body)
    go :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
    go (Ghc.L loc x) =
      let instrVar = Ghc.HsVar Ghc.noExtField (Ghc.L Ghc.noSrcSpanA (Ghc.Exact instrName))
          instr =
            Ghc.HsApp Ghc.noAnn (Ghc.L Ghc.noSrcSpanA instrVar)
            $ Ghc.L Ghc.noSrcSpanA (Ghc.HsLit Ghc.noAnn
                (Ghc.HsString Ghc.NoSourceText (Ghc.occNameFS $ Ghc.rdrNameOcc bindName)))
       in Ghc.L loc $ Ghc.HsApp Ghc.noAnn (Ghc.L Ghc.noSrcSpanA instr) (Ghc.L loc x)
