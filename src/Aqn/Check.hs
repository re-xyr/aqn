module Aqn.Check where

import           Aqn.Common
import           Aqn.Eval
import           Aqn.Presyntax
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Unify                 (unify)
import           Aqn.Value
import           Control.Lens              ((^.))
import           Control.Monad.Extra       (fromMaybeM)
import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.Fresh (fresh)
import           Data.Functor              ((<&>))
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Traversable          (for)
import           Data.Tsil                 (List ((:>)))
import qualified Data.Tsil                 as Tsil

type Ctx = List (Local, Val)

type CheckM m = (Write m, Member (Error CheckError) m, Writing 'Locals, Writing 'Metas, Reading 'Funs)

check :: CheckM m => Ctx -> Expr -> Val -> Eff m Term
check ctx ex ty' = do
  ty <- lift $ force ty'
  case (ex, ty) of
    (XLam licit n par body, VPi licit' _ dom cod) | licit == licit' ->
      TLam licit n par <$> check (ctx :> (par, dom)) body (cod $ neuLoc par)
    (_, VPi Implicit n' dom cod) -> do
      par <- freshLocal n'
      TLam Implicit n' par <$> check (ctx :> (par, dom)) ex (cod $ neuLoc par)
    (XHole, _) ->
      freshMeta ctx ty
    (XLet ref vTy v body, _) -> do
      vTyT <- check ctx vTy VU
      vTyV <- lift $ eval vTyT
      vT <- check ctx v vTyV
      bodyT <- check (ctx :> (ref, vTyV)) body ty
      pure $ TLet ref vT bodyT
    _ -> do
      (tyIn, tm) <- fromJust <$> (infer ctx ex >>= uncurry (insert ExplicitA ctx))
      unify ty tyIn >>= \case
        Nothing -> pure tm
        Just reason -> do
          err <- CantUnify reason ex <$> quoteE tyIn <*> quoteE ty
          throwError err

infer :: CheckM m => Ctx -> Expr -> Eff m (Val, Term)
infer ctx ex = case ex of
  XTy x ty -> do
    tyT <- check ctx ty VU
    tyV <- lift $ eval tyT
    xt <- check ctx x tyV
    pure (tyV, xt)
  XPi licit n par dom cod -> do
    dom' <- check ctx dom VU
    domV' <- lift $ eval dom'
    cod' <- check (ctx :> (par, domV')) cod VU
    pure (VU, TPi licit n par dom' cod')
  XU -> pure (VU, TU)
  XApp licit f x -> do
    (oTy, oF) <- infer ctx f
    insert licit ctx oTy oF >>= \case
      Just (ty', f') -> do
        ty <- lift $ force ty'
        case ty of
          VPi _ _ dom cod -> do
            x' <- check ctx x dom
            x'E <- lift $ eval x'
            appd <- lift $ TApp (unNamedLicit licit) f' x'
            pure (cod x'E, appd)
          _ -> do
            oTyT <- quoteE oTy
            throwError $ FunctionMismatch ex licit oTyT
      _ -> do
        oTyT <- quoteE oTy
        throwError $ FunctionMismatch ex licit oTyT
  XLam licit n _ _ -> do
    (dom, clos) <- freshDomCod ctx n
    let ty = VPi licit n dom clos
    tm <- check ctx ex ty
    pure (ty, tm)
  XLet ref vTy v body -> do
    vTyT <- check ctx vTy VU
    vTyV <- lift $ eval vTyT
    vT <- check ctx v vTyV
    (bodyTyV, bodyT) <- infer (ctx :> (ref, vTyV)) body
    pure (bodyTyV, TLet ref vT bodyT)
  XLoc ref ->
    pure (fromMaybe (error "Not well-scoped variable") (Tsil.lookup ref ctx), TLoc ref)
  XFun ref -> do
    fun <- fromMaybeM (throwError $ ElaborationBlocked ex (DVFun ref)) (readFun ref <&> (^. funCore))
    args <- for (fun ^. funParams) \(l, n, _, _) -> (l, ) <$> freshLocal n
    let call = wrapLambda args $ TFun ref (fmap TLoc <$> args)
    pure (fun ^. funTy, call)
  XHole -> do
    tyMeta <- freshMeta ctx VU
    ty <- lift $ eval tyMeta
    tm <- freshMeta ctx ty
    pure (ty, tm)

freshDomCod :: CheckM m => Ctx -> Name -> Eff m (Val, Val -> Val)
freshDomCod ctx n = do
  domMeta <- freshMeta ctx VU
  dom <- lift (eval domMeta)
  tyRef <- freshLocal n
  codMeta <- freshMeta (ctx :> (tyRef, dom)) VU
  clos <- lift $ closure [] tyRef codMeta
  pure (dom, clos)

freshMeta :: (Write m, Writing 'Metas) => Ctx -> Val -> Eff m Term
freshMeta ctx _ty = do
  metavar <- MetaVar <$> fresh
  writeMeta metavar (Meta (MetaCore Nothing))
  pure $ TMeta metavar `tApplyMany` fmap (\(r, _) -> (Implicit, TLoc r)) ctx

insert :: (Write m, Writing 'Metas, Reading 'Funs, Reading 'Locals) => NamedLicit -> Ctx -> Val -> Term -> Eff m (Maybe (Val, Term))
insert licit ctx ty' tm = do
  ty <- lift $ force ty'
  case ty of
    VPi licit' n dom cod
      | matchNamedLicit licit licit' n -> pure $ Just (ty, tm)
      | licit' == Implicit -> do
        meta <- freshMeta ctx dom
        metaV <- lift (eval meta)
        appd <- lift $ TApp Implicit tm meta
        insert licit ctx (cod metaV) appd
      | otherwise -> pure Nothing
    _ -> pure $ Just (ty, tm)

-- infer :: CheckM m => Ctx -> Expr -> Eff m (Val, Term)
-- infer ctx ex = do
--   (ty, tm) <- infer' ctx ex
--   lift' $ (ty, ) <$> finalizeCall tm

-- finalizeCall :: (Retrieve, Member Fresh m) => Term -> Eff m Term
-- finalizeCall tm = case tm of
--   TFun r xs | la < lp -> do
--     args <- mapM (\(l, _, _) -> (l, ) <$> freshLocal) (Seq.take (lp - la) pars)
--     pure $ wrapLambda args $ TFun r $ (args <&> _2 %~ TLoc) <> xs
--     where
--       pars = getFun r ^. funCore . funParams
--       la = length xs
--       lp = length $ getFun r ^. funCore . funParams
--   _ -> pure tm

-- tSmartApply :: Retrieve => Licit -> Term -> Term -> Term
-- tSmartApply l f x = case f of
--   TFun r xs | Seq.length xs < lp -> TFun r ((l, x) <| xs)
--     where lp = Seq.length $ getFun r ^. funCore . funParams
--   _         -> TApp l f x
