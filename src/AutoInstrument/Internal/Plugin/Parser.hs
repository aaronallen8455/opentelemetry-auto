{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module AutoInstrument.Internal.Plugin.Parser
  ( parsedResultAction
  ) where

import           Control.Monad
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
    Nothing -> pure parsedResult
    Just config -> do
      let targets = do
            -- TODO use constraint sets
            Config.Constructor target args <- Config.targets config
            pure (BS8.pack target, args)
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

getMatches :: [(BS8.ByteString, [Config.ConArg])] -> [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.OccName]
getMatches targets = concat . mapMaybe go where
  go (Ghc.L _ (Ghc.SigD _ (Ghc.TypeSig _ lhs (Ghc.HsWC _ (Ghc.L _ (Ghc.HsSig _ _ (Ghc.L _ ty)))))))
    | isTargetTy ty = Just (Ghc.rdrNameOcc . Ghc.unLoc <$> lhs)
  go _ = Nothing
  isTargetTy = \case
    Ghc.HsForAllTy _ _ (Ghc.L _ body) -> isTargetTy body
    Ghc.HsQualTy _ _ctx (Ghc.L _ body) -> isTargetTy body -- TODO use constraint context
    -- constraints will have to be carried to the return type to check that they
    -- are applied to it rather than some argument.
    Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ _inner)
      -> let constrBS x = Ghc.bytesFS $ Ghc.occNameFS (Ghc.rdrNameOcc x)
             f rdrName = lookup (constrBS rdrName) targets
          in (null <$> checkCon f con) == Just True
    Ghc.HsFunTy _ _ _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsParTy _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsDocTy _ (Ghc.L _ nxt) _ -> isTargetTy nxt
    _ -> False
  checkCon
    :: (Ghc.RdrName -> Maybe [Config.ConArg])
    -> Ghc.HsType Ghc.GhcPs
    -> Maybe [Config.ConArg]
  checkCon getArgs = \case
    Ghc.HsTyVar _ _ (Ghc.L _ constr) -> getArgs constr
    Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ arg) -> do
      args <- checkCon getArgs con
      case args of
        [] -> Just []
        Config.ConWildcard : rest -> Just rest
        Config.ConArg conArg : rest -> do
          let argWords = Config.ConArg <$> BS8.words conArg -- TODO allow wildcards here?
              f rdrName = case argWords of
                  Config.ConArg n : others -> do
                    guard $ n == Ghc.bytesFS (Ghc.occNameFS $ Ghc.rdrNameOcc rdrName)
                    Just others
                  _ -> Nothing
          [] <- checkCon f arg
          Just rest
    Ghc.HsParTy _ (Ghc.L _ nxt) -> checkCon getArgs nxt
    _ -> Nothing

instrumentDecl :: Ghc.Name -> S.Set Ghc.OccName -> Ghc.LHsDecl Ghc.GhcPs -> Ghc.LHsDecl Ghc.GhcPs
instrumentDecl instrName targets
    (Ghc.L loc (Ghc.ValD vX (Ghc.FunBind { Ghc.fun_matches = Ghc.MG { Ghc.mg_alts = Ghc.L altsLoc alts, ..}, ..} )))
  | Ghc.rdrNameOcc (Ghc.unLoc fun_id) `S.member` targets
  = let newAlts = (fmap . fmap) (instrumentMatch (Ghc.unLoc fun_id) instrName) alts
     in Ghc.L loc (Ghc.ValD vX (Ghc.FunBind {Ghc.fun_matches = Ghc.MG { Ghc.mg_alts = Ghc.L altsLoc newAlts, ..}, ..}))
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
