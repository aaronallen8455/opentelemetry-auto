{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module AutoInstrument.Internal.Plugin.Parser
  ( parsedResultAction
  ) where

import           Data.Maybe (mapMaybe)
import           Control.Monad.IO.Class (liftIO)

import qualified AutoInstrument.Internal.GhcFacade as Ghc

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
      _ -> error "oh no"
  let occ = Ghc.mkVarOcc "autoInstrument"
  autoInstrumentName <- liftIO $ Ghc.lookupNameCache (Ghc.hsc_NC hscEnv) otelMod occ

  -- look for type sigs where the return type matches one of
  -- the targets.
  -- find all definitions with the same name as the signature
  -- Apply the autoInstrument function in each of its branches

  -- Since using a parser, name shadowing is potentially a problem. could make
  -- a traversal that checks for all type sigs that are direct children of the
  -- current node, then find the definitions corresponding to those sigs.
  -- Perhaps the plugin should only consider type level definitions.

  pure parsedResult

getTargets :: Ghc.RdrName -> [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.RdrName]
getTargets target = concat . mapMaybe go where
  go (Ghc.L _ (Ghc.SigD _ (Ghc.TypeSig _ lhs (Ghc.HsWC _ (Ghc.L _ (Ghc.HsSig _ _ (Ghc.L _ ty)))))))
    | isTargetTy ty = Just (Ghc.unLoc <$> lhs)
  go _ = Nothing
  isTargetTy = \case
    Ghc.HsForAllTy _ _ (Ghc.L _ body) -> isTargetTy body
    Ghc.HsQualTy _ _ctx (Ghc.L _ body) -> isTargetTy body -- TODO use constraint context
    -- constraints will have to be carried to the return type to check that they
    -- are applied to it rather than some argument.
    Ghc.HsTyVar _ _ (Ghc.L _ id) -> True
    Ghc.HsAppTy _ (Ghc.L _ (Ghc.HsTyVar _ _ (Ghc.L _ constr))) (Ghc.L _ inner)
      -> constr == target
    -- Ghc.HsAppTy _ (Ghc.L _ con) (Ghc.L _ inner) -> True
    Ghc.HsFunTy _ _ _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsParTy _ (Ghc.L _ nxt) -> isTargetTy nxt
    Ghc.HsDocTy _ (Ghc.L _ nxt) _ -> isTargetTy nxt
    _ -> False
