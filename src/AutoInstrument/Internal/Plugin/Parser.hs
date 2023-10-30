{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module AutoInstrument.Internal.Plugin.Parser
  ( parsedResultAction
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S

import qualified AutoInstrument.Internal.GhcFacade as Ghc
import qualified AutoInstrument.Internal.Config as Cfg

parsedResultAction
  :: [Ghc.CommandLineOption]
  -> Ghc.ModSummary
  -> Ghc.ParsedResult
  -> Ghc.Hsc Ghc.ParsedResult
parsedResultAction opts modSummary
    parsedResult@Ghc.ParsedResult
      {Ghc.parsedResultModule = prm@Ghc.HsParsedModule
        {Ghc.hpm_module = Ghc.L modLoc mo@Ghc.HsModule{Ghc.hsmodDecls}}} = do

  let modName = Ghc.moduleName $ Ghc.ms_mod modSummary
      unitId = Ghc.toUnitId . Ghc.moduleUnit $ Ghc.ms_mod modSummary

  hscEnv <- Ghc.getHscEnv
  result <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "AutoInstrument.Internal.Types") Ghc.NoPkgQual
  otelMod <-
    case result of
      Ghc.Found _ m -> pure m
      _ -> error "AutoInstrument.Internal.Types module not found"
  let occ = Ghc.mkVarOcc "autoInstrument"
  autoInstrumentName <- liftIO $ Ghc.lookupNameCache (Ghc.hsc_NC hscEnv) otelMod occ

  mConfig <- liftIO $ fmap Cfg.getConfig <$> Cfg.getConfigCache opts

  case mConfig of
    Nothing -> pure parsedResult
    Just config -> do
      let matches = S.fromList $ getMatches config hsmodDecls

          newDecls = instrumentDecl modName unitId autoInstrumentName matches <$> hsmodDecls

      pure parsedResult
        { Ghc.parsedResultModule = prm
          { Ghc.hpm_module = Ghc.L modLoc mo
            { Ghc.hsmodDecls = newDecls
            }
          }
        }

getMatches
  :: Cfg.Config
  -> [Ghc.LHsDecl Ghc.GhcPs]
  -> [Ghc.OccName]
getMatches cfg = concat . mapMaybe go where
  go (Ghc.L _ (Ghc.SigD _ (Ghc.TypeSig _ lhs (Ghc.HsWC _ (Ghc.L _ (Ghc.HsSig _ _ (Ghc.L _ ty)))))))
    | isTargetTy [] ty = Just (Ghc.rdrNameOcc . Ghc.unLoc <$> lhs)
  go _ = Nothing
  isTargetTy preds = \case
    Ghc.HsForAllTy _ _ (Ghc.L _ body) -> isTargetTy preds body
    Ghc.HsQualTy _ (Ghc.L _ ctx) (Ghc.L _ body) ->
      isTargetTy (preds ++ fmap Ghc.unLoc ctx) body
    app@Ghc.HsAppTy{} -> check preds app
    var@Ghc.HsTyVar{} -> check preds var
    Ghc.HsFunTy _ _ _ (Ghc.L _ nxt) -> isTargetTy preds nxt
    Ghc.HsParTy _ (Ghc.L _ nxt) -> isTargetTy preds nxt
    Ghc.HsDocTy _ (Ghc.L _ nxt) _ -> isTargetTy preds nxt
    _ -> False

  check
    :: [Ghc.HsType Ghc.GhcPs]
    -> Ghc.HsType Ghc.GhcPs
    -> Bool
  check preds expr =
    any (matchTarget preds expr) (Cfg.targets cfg)
    && not (any (matchTarget preds expr) (Cfg.exclusions cfg))

  matchTarget preds expr = \case
    Cfg.Constructor conTarget -> checkTy True conTarget expr
    Cfg.Constraints predTarget -> checkPred preds predTarget

  checkTy
    :: Bool
    -> Cfg.TargetCon
    -> Ghc.HsType Ghc.GhcPs
    -> Bool
  checkTy top t (Ghc.HsParTy _ (Ghc.L _ x)) = checkTy top t x
  checkTy top t (Ghc.HsDocTy _ (Ghc.L _ x) _) = checkTy top t x
  checkTy _ (Cfg.TyVar name) (Ghc.HsTyVar _ _ (Ghc.L _ rdrName)) =
    BS8.pack name == Ghc.bytesFS (Ghc.occNameFS $ Ghc.rdrNameOcc rdrName)
  checkTy top target@(Cfg.App x y) (Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ arg)) =
    (checkTy False y arg && checkTy False x con )
    || (top && checkTy True target con)
  checkTy True target@(Cfg.TyVar _) (Ghc.HsAppTy _ (Ghc.L _ con) _) =
    checkTy True target con
  checkTy _ Cfg.Unit (Ghc.HsTupleTy _ Ghc.HsBoxedOrConstraintTuple []) = True
  checkTy _ (Cfg.Tuple targets) (Ghc.HsTupleTy _ Ghc.HsBoxedOrConstraintTuple exprs) =
    and $ zipWith (checkTy False) targets (Ghc.unLoc <$> exprs)
  checkTy _ Cfg.WC _ = True
  checkTy _ _ _ = False

  checkPred
    :: [Ghc.HsType Ghc.GhcPs]
    -> Cfg.ConstraintSet
    -> Bool
  checkPred preds predSet =
    all (\p -> any (checkTy True p) preds)
        (S.toList predSet)

instrumentDecl
  :: Ghc.ModuleName
  -> Ghc.UnitId
  -> Ghc.Name
  -> S.Set Ghc.OccName
  -> Ghc.LHsDecl Ghc.GhcPs
  -> Ghc.LHsDecl Ghc.GhcPs
instrumentDecl modName unitId instrName targets
    (Ghc.L loc (Ghc.ValD vX fb@Ghc.FunBind
      { Ghc.fun_matches = mg@Ghc.MG
        { Ghc.mg_alts = Ghc.L altsLoc alts }, Ghc.fun_id}))
  | Ghc.rdrNameOcc (Ghc.unLoc fun_id) `S.member` targets
  = let newAlts = (fmap . fmap)
          (instrumentMatch modName unitId (Ghc.unLoc fun_id) instrName)
          alts
     in Ghc.L loc (Ghc.ValD vX (fb
       { Ghc.fun_matches = mg
         { Ghc.mg_alts = Ghc.L altsLoc newAlts }}))
instrumentDecl _ _ _ _ x = x

instrumentMatch
  :: Ghc.ModuleName
  -> Ghc.UnitId
  -> Ghc.RdrName
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
instrumentMatch modName unitId bindName instrName match =
  match
    { Ghc.m_grhss = (Ghc.m_grhss match)
      { Ghc.grhssGRHSs = (fmap . fmap) modifyGRH (Ghc.grhssGRHSs (Ghc.m_grhss match)) }
    }
  where
    modifyGRH :: Ghc.GRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
              -> Ghc.GRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
    modifyGRH (Ghc.GRHS x guards body) =
      Ghc.GRHS x guards (go body)
    go :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
    go (Ghc.L loc x) =
      let instrVar = Ghc.HsVar Ghc.noExtField (Ghc.L Ghc.noSrcSpanA (Ghc.Exact instrName))
          mkStringExpr = Ghc.L Ghc.noSrcSpanA . Ghc.HsLit Ghc.noAnn
                       . Ghc.HsString Ghc.NoSourceText
          app :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
          app l r = Ghc.L Ghc.noSrcSpanA $ Ghc.HsApp Ghc.noAnn l r
          srcSpan = Ghc.realSrcSpan . Ghc.locA $ loc :: Ghc.RealSrcSpan
          instr =
            Ghc.L Ghc.noSrcSpanA instrVar
              `app`
            (mkStringExpr . Ghc.occNameFS $ Ghc.rdrNameOcc bindName)
              `app`
            mkStringExpr (Ghc.moduleNameFS modName)
              `app`
            mkStringExpr (Ghc.srcSpanFile srcSpan)
              `app`
            (mkStringExpr . Ghc.fsLit . show $ Ghc.srcSpanStartLine srcSpan)
              `app`
            mkStringExpr (Ghc.unitIdFS unitId)

       in Ghc.L loc $ Ghc.HsApp Ghc.noAnn instr (Ghc.L loc x)
