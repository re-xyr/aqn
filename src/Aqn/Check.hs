module Aqn.Check where

import           Aqn.Common
import           Aqn.Eval
import           Aqn.Presyntax
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Unify                 (unify)
import           Aqn.Value
import           Control.Lens              (makeLenses, (%~), (^.))
import           Control.Monad.Extra       (fromMaybeM)
import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.Fresh (fresh)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Traversable          (for)
import           Data.Tsil                 (List ((:>)))
import qualified Data.Tsil                 as Tsil

data Ctx = Ctx
  { _ctxTele      :: List (Local, Val)
  , _ctxBoundTele :: List (Local, Val)
  , _ctxEnv       :: Env
  }
makeLenses ''Ctx

topCtx :: Ctx
topCtx = Ctx [] [] []
{-# INLINE topCtx #-}

bound :: Local -> Val -> Ctx -> Ctx
bound r v ctx = ctx & ctxTele %~ (:> (r, v)) & ctxBoundTele %~ (:> (r, v))
{-# INLINE bound #-}

defined :: Local -> Val -> Val -> Ctx -> Ctx
defined r t v ctx = ctx & ctxTele %~ (:> (r, t)) & ctxEnv %~ (:> (r, v))
{-# INLINE defined #-}

eval' :: EvalM => Ctx -> Term -> Val
eval' ctx = eval (ctx ^. ctxEnv)
{-# INLINE eval' #-}

type CheckM m = (Write m, Member (Error CheckError) m, Writing 'Locals, Writing 'Metas, Reading 'Funs)

check :: CheckM m => Ctx -> Expr -> Val -> Eff m Term
check ctx ex ty' = do
  ty <- lift $ force ty'
  case (ex, ty) of
    (XLam licit n par body, VPi licit' _ dom cod) | licit == licit' ->
      TLam licit n par <$> check (bound par dom ctx) body (cod $ neuLoc par)
    (_, VPi Implicit n' dom cod) -> do
      par <- freshLocal n'
      TLam Implicit n' par <$> check (bound par dom ctx) ex (cod $ neuLoc par)
    (XHole, _) ->
      freshMeta ctx ty
    (XLet ref vTy v body, _) -> do
      vTyT <- check ctx vTy VU
      vTyV <- lift $ eval' ctx vTyT
      vT <- check ctx v vTyV
      vV <- lift $ eval' ctx vT
      bodyT <- check (defined ref vTyV vV ctx) body ty
      pure $ TLet ref vT bodyT
    _ -> do
      (tm, tyIn) <- fromJust <$> (infer ctx ex >>= uncurry (insert ExplicitA ctx))
      unify ty tyIn >>= \case
        Nothing -> pure tm
        Just reason -> do
          err <- CantUnify reason ex <$> quoteE tyIn <*> quoteE ty
          throwError err

infer :: CheckM m => Ctx -> Expr -> Eff m (Term, Val)
infer ctx ex = case ex of
  XTy x ty -> do
    tyT <- check ctx ty VU
    tyV <- lift $ eval' ctx tyT
    xt <- check ctx x tyV
    pure (xt, tyV)
  XPi licit n par dom cod -> do
    dom' <- check ctx dom VU
    domV' <- lift $ eval' ctx dom'
    cod' <- check (bound par domV' ctx) cod VU
    pure (TPi licit n par dom' cod', VU)
  XU -> pure (TU, VU)
  XApp licit f x -> do
    (oF, oTy) <- infer ctx f
    insert licit ctx oF oTy >>= \case
      Just (f', ty') -> do
        ty <- lift $ force ty'
        case ty of
          VPi _ _ dom cod -> do
            x' <- check ctx x dom
            x'E <- lift $ eval' ctx x'
            appd <- lift $ TApp (unNamedLicit licit) f' x'
            pure (appd, cod x'E)
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
    pure (tm, ty)
  XLet ref vTy v body -> do
    vTyT <- check ctx vTy VU
    vTyV <- lift $ eval' ctx vTyT
    vT <- check ctx v vTyV
    vV <- lift $ eval' ctx vT
    (bodyT, bodyTyV) <- infer (defined ref vTyV vV ctx) body
    pure (TLet ref vT bodyT, bodyTyV)
  XLoc ref ->
    pure (TLoc ref, fromMaybe (error "Not well-scoped variable") (Tsil.lookup ref (ctx ^. ctxTele)))
  XFun ref -> do
    fun <- fromMaybeM (throwError $ ElaborationBlocked ex (DVFun ref)) (readFun ref <&> (^. funCore))
    args <- for (fun ^. funParams) \(Tp l n _ ::: _) -> (l :::) <$> freshLocal n
    let call = wrapLambda args $ TFun ref (fmap TLoc <$> args)
    pure (call, fun ^. funTy)
  XHole -> do
    tyMeta <- freshMeta ctx VU
    ty <- lift $ eval' ctx tyMeta
    tm <- freshMeta ctx ty
    pure (tm, ty)

freshDomCod :: CheckM m => Ctx -> Name -> Eff m (Val, Val -> Val)
freshDomCod ctx n = do
  domMeta <- freshMeta ctx VU
  dom <- lift (eval' ctx domMeta)
  tyRef <- freshLocal n
  codMeta <- freshMeta (bound tyRef dom ctx) VU
  clos <- lift $ closure [] tyRef codMeta
  pure (dom, clos)

freshMeta :: (Write m, Writing 'Metas) => Ctx -> Val -> Eff m Term
freshMeta ctx _ty = do
  metavar <- MetaVar <$> fresh
  writeMeta metavar (Meta (MetaCore Nothing))
  pure $ TMeta metavar `tApplyMany` fmap (\(r, _) -> Implicit ::: TLoc r) (ctx ^. ctxBoundTele)

insert :: (Write m, Writing 'Metas, Reading 'Funs, Reading 'Locals) => NamedLicit -> Ctx -> Term -> Val -> Eff m (Maybe (Term, Val))
insert licit ctx tm ty' = do
  ty <- lift $ force ty'
  case ty of
    VPi licit' n dom cod
      | matchNamedLicit licit licit' n -> pure $ Just (tm, ty)
      | licit' == Implicit -> do
        meta <- freshMeta ctx dom
        metaV <- lift (eval' ctx meta)
        let appd = TApp Implicit tm meta
        insert licit ctx appd (cod metaV)
      | otherwise -> pure Nothing
    _ -> pure $ Just (tm, ty)

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
