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
    Nothing -> liftIO $ do
      putStrLn "================================================================================"
      putStrLn "Failed to parse auto instrument config file"
      putStrLn "================================================================================"
      pure parsedResult
      -- TODO emit a parse error
--         { Ghc.parsedResultMessages = Ghc.parsedResultMessages parsedResult
--           { Ghc.psErrors =
--           }
--         }
    Just config -> do
      let conTargets = do
            Config.Constructor target <- Config.targets config
            pure target
          predSets = do
            Config.Constraints predSet <- Config.targets config
            pure predSet
          matches = S.fromList
                  $ getMatches conTargets predSets hsmodDecls

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

getMatches
  :: [Config.TargetCon]
  -> [Config.ConstraintSet]
  -> [Ghc.LHsDecl Ghc.GhcPs]
  -> [Ghc.OccName]
getMatches conTargets predSets = concat . mapMaybe go where
  go (Ghc.L _ (Ghc.SigD _ (Ghc.TypeSig _ lhs (Ghc.HsWC _ (Ghc.L _ (Ghc.HsSig _ _ (Ghc.L _ ty)))))))
    | isTargetTy [] ty = Just (Ghc.rdrNameOcc . Ghc.unLoc <$> lhs)
  go _ = Nothing
  isTargetTy quals = \case
    Ghc.HsForAllTy _ _ (Ghc.L _ body) -> isTargetTy quals body
    Ghc.HsQualTy _ (Ghc.L _ ctx) (Ghc.L _ body) ->
      isTargetTy (quals ++ fmap Ghc.unLoc ctx) body
    app@Ghc.HsAppTy{} ->
      any (\t -> checkTy True t app) conTargets
      || any (checkPred quals) predSets
    var@Ghc.HsTyVar{} ->
      any (\t -> checkTy True t var) conTargets
      || any (checkPred quals) predSets
    Ghc.HsFunTy _ _ _ (Ghc.L _ nxt) -> isTargetTy quals nxt
    Ghc.HsParTy _ (Ghc.L _ nxt) -> isTargetTy quals nxt
    Ghc.HsDocTy _ (Ghc.L _ nxt) _ -> isTargetTy quals nxt
    _ -> False

  checkTy
    :: Bool
    -> Config.TargetCon
    -> Ghc.HsType Ghc.GhcPs
    -> Bool
  checkTy top t (Ghc.HsParTy _ (Ghc.L _ x)) = checkTy top t x
  checkTy top t (Ghc.HsDocTy _ (Ghc.L _ x) _) = checkTy top t x
  checkTy _ (Config.TyVar name) (Ghc.HsTyVar _ _ (Ghc.L _ rdrName)) =
    BS8.pack name == Ghc.bytesFS (Ghc.occNameFS $ Ghc.rdrNameOcc rdrName)
  checkTy top target@(Config.App x y) (Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ arg)) =
    (checkTy False y arg && checkTy False x con )
    || (top && checkTy True target con)
  checkTy True target@(Config.TyVar _) (Ghc.HsAppTy _ (Ghc.L _ con) _) =
    checkTy True target con
  checkTy _ Config.Unit (Ghc.HsTupleTy _ Ghc.HsBoxedOrConstraintTuple []) = True
  checkTy _ Config.WC _ = True
  checkTy _ _ _ = False

  checkPred
    :: [Ghc.HsType Ghc.GhcPs]
    -> Config.ConstraintSet
    -> Bool
  checkPred quals predSet =
    all (\p -> any (checkTy True p) quals)
        (S.toList predSet)

instrumentDecl :: Ghc.Name -> S.Set Ghc.OccName -> Ghc.LHsDecl Ghc.GhcPs -> Ghc.LHsDecl Ghc.GhcPs
instrumentDecl instrName targets
    (Ghc.L loc (Ghc.ValD vX fb@Ghc.FunBind
      { Ghc.fun_matches = mg@Ghc.MG
        { Ghc.mg_alts = Ghc.L altsLoc alts }, Ghc.fun_id}))
  | Ghc.rdrNameOcc (Ghc.unLoc fun_id) `S.member` targets
  = let newAlts = (fmap . fmap) (instrumentMatch (Ghc.unLoc fun_id) instrName) alts
     in Ghc.L loc (Ghc.ValD vX (fb
       { Ghc.fun_matches = mg
         { Ghc.mg_alts = Ghc.L altsLoc newAlts }}))
instrumentDecl _ _ x = x

instrumentMatch
  :: Ghc.RdrName
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
  -> Ghc.Match Ghc.GhcPs (Ghc.GenLocated Ghc.SrcSpanAnnA (Ghc.HsExpr Ghc.GhcPs))
instrumentMatch bindName instrName match =
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
          instr =
            Ghc.HsApp Ghc.noAnn (Ghc.L Ghc.noSrcSpanA instrVar)
            $ Ghc.L Ghc.noSrcSpanA (Ghc.HsLit Ghc.noAnn
                (Ghc.HsString Ghc.NoSourceText (Ghc.occNameFS $ Ghc.rdrNameOcc bindName)))
       in Ghc.L loc $ Ghc.HsApp Ghc.noAnn (Ghc.L Ghc.noSrcSpanA instr) (Ghc.L loc x)
