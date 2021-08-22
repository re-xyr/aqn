module Aqn.Unify where

import           Aqn.Common
import           Aqn.Eval
import           Aqn.Prettyprint
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Value
import           Control.Lens              ((?~), (^.))
import           Control.Monad             (unless)
import           Control.Monad.Extra       (fromMaybeM)
import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Error (Error, catchError, runError, throwError)
import           Control.Monad.Zip         (MonadZip (mzip))
import           Data.Foldable             (Foldable (toList), foldlM)
import           Data.Function             ((&))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import           Data.Tsil                 (List (Empty, (:>)))
import qualified Data.Tsil                 as Tsil
import qualified Debug.Trace               as Debug

type UnifyM m = (Write m, Writing 'Locals, Writing 'Metas, Reading 'Funs)
type UnifyE m = Member (Error UnifyError) m

unify :: UnifyM m => Val -> Val -> Eff m (Maybe UnifyError)
unify l r =
  either (\(x :: UnifyError) -> Just x) (\() -> Nothing) <$> runError (unify' l r)

unify' :: (UnifyM m, UnifyE m) => Val -> Val -> Eff m ()
unify' l r = unifyShallow l r `catchError` \(_ :: UnifyError) -> do
  (x, y) <- lift (force l, force r)
  unifyShallow x y `catchError` \(_ :: UnifyError) -> unifyDeep x y

unifyShallow :: (UnifyM m, UnifyE m) => Val -> Val -> Eff m ()
unifyShallow l r = do
  -- ql <- quoteE l
  -- qr <- quoteE r
  -- tl <- lift $ prettyTerm ql
  -- tr <- lift $ prettyTerm qr
  -- Debug.trace ("checking " ++ show tl ++ " and " ++ show tr) $ pure ()
  case (l, r) of
    -- Approximate
    (VNeu (HLoc ref) els ~_, VNeu (HLoc ref') els' ~_) | ref == ref' ->
      unifySpine els els'
    (VNeu (HMeta ref) els ~_, VNeu (HMeta ref') els' ~_) | ref == ref' ->
      unifySpine els els'
    (VNeu (HFun ref args) els ~_, VNeu (HFun ref' args') els' ~_) | ref == ref' -> do
      unifyArgs args args'
      unifySpine els els'
    _ -> throwError CantUnifyApprox

unifyDeep :: (UnifyM m, UnifyE m) => Val -> Val -> Eff m ()
unifyDeep l r = do
  -- ql <- quoteE l
  -- qr <- quoteE r
  -- tl <- lift $ prettyTerm ql
  -- tr <- lift $ prettyTerm qr
  -- Debug.trace ("checking " ++ show tl ++ " and " ++ show tr) $ pure ()
  case (l, r) of
    (VU, VU) -> pure ()
    (VPi licit n dom cod, VPi licit' _ dom' cod') -> do
      unless (licit == licit') $ throwError CantUnifyLicit
      unify' dom dom'
      fresh <- neuLoc <$> freshLocal n
      unify' (cod fresh) (cod' fresh)

    (VLam licit n cod, x) -> do
      fresh <- neuLoc <$> freshLocal n
      unify' (cod fresh) (vApply licit x fresh)
    (_, VLam {}) -> unify' r l

    (VNeu (HMeta ref) els Nothing, VNeu (HMeta ref') els' Nothing) ->
      -- This is a temporary solution; use intersection in the future
      solveMeta ref els r `catchError` \(_ :: UnifyError) -> solveMeta ref' els' l
    (VNeu (HMeta ref) els Nothing, _) ->
      solveMeta ref els r
    (_, VNeu (HMeta _) _ Nothing) -> unify' r l

    _ -> throwError DontKnowHowToUnify

solveMeta :: (UnifyM m, UnifyE m) => MetaVar -> List Elim -> Val -> Eff m ()
solveMeta ref els sln = do
  meta <- readMeta ref
  args <- ensure NoPatternCondition $ allApps els
  refs <- ensure NoPatternCondition $ allVars args
  ensure NoPatternCondition $ allDistinct Set.empty refs
  pars <- mapM (\(l, r) -> do
    loc <- readLocal r
    fr <- freshLocal (loc ^. localName)
    pure (l, fr)) refs
  let cor = Map.fromList $ toList $ Tsil.zip (snd <$> refs) (snd <$> pars)
  slnT <- wellScoped ref cor sln
  let body = wrapLambda pars slnT
  bodyV <- lift $ eval body
  bodyStr <- lift $ prettyTerm body
  Debug.trace ("Wrote meta " ++ show ref ++ ": " ++ show bodyStr) $
    writeMeta ref (meta & (metaCore . metaBody) ?~ (body, bodyV))
  where
    ensure e = fromMaybeM (throwError e) . pure
    allApps Empty = Just []
    allApps (xs :> x) = case x of
      EApp l a -> (:> (l, a)) <$> allApps xs
      -- _        -> Nothing -- Note here! In the future you need to put this back.
    allVars Empty = Just []
    allVars (xs :> (l, x)) = case x of
      VNeu (HLoc r) [] _ -> (:> (l, r)) <$> allVars xs
      _                  -> Nothing
    allDistinct set Empty = Just set
    allDistinct set (xs :> (_, x))
      | x `elem` set = Nothing
      | otherwise = allDistinct (Set.insert x set) xs

type SolveM m = (Write m, Writing 'Locals, Reading 'Metas, Reading 'Funs)

wellScoped :: (SolveM m, UnifyE m) => MetaVar -> Map Local Local -> Val -> Eff m Term
wellScoped self refs vl' = do
  vl <- lift $ force vl'
  case vl of
    VPi l n dom cod -> do
      ref <- freshLocal n
      let refs' = Map.insert ref ref refs
      TPi l n ref <$> wellScoped self refs' dom <*> wellScoped self refs' (cod $ neuLoc ref)
    VLam l n body -> do
      ref <- freshLocal n
      TLam l n ref <$> wellScoped self (Map.insert ref ref refs) (body $ neuLoc ref)
    VNeu hd els' _ -> do
      hd' <- case hd of
        HLoc lo -> case Map.lookup lo refs of
          Nothing -> throwError CantOccursCheck
          Just x  -> pure $ TLoc x
        HMeta mv -> if mv == self
          then throwError CantOccursCheck
          else pure $ TMeta mv
        HFun f args ->
          TFun f . Tsil.toSeq <$> mapM (mapM (wellScoped self refs)) args
      foldlM (wellScopedElim self refs) hd' els'
    VU -> pure TU

wellScopedElim :: (SolveM m, UnifyE m) => MetaVar -> Map Local Local -> Term -> Elim -> Eff m Term
wellScopedElim self refs tm el = case el of
  EApp li val -> TApp li tm <$> wellScoped self refs val

unifyArgs :: (UnifyM m, UnifyE m, Foldable f, MonadZip f) => f (Licit, Val) -> f (Licit, Val) -> Eff m ()
unifyArgs l r
  | length l /= length r = throwError DifferentSpineLength
  | otherwise = sequence_ $ uncurry unifyArg <$> mzip l r

unifyArg :: (UnifyM m, UnifyE m) => (Licit, Val) -> (Licit, Val) -> Eff m ()
unifyArg (licit, arg) (licit', arg') = do
  unless (licit == licit') $ throwError CantUnifyLicit
  unify' arg arg'

unifySpine :: (UnifyM m, UnifyE m, Foldable f, MonadZip f) => f Elim -> f Elim -> Eff m ()
unifySpine l r
  | length l /= length r = throwError DifferentSpineLength
  | otherwise = sequence_ $ uncurry unifyElim <$> mzip l r

unifyElim :: (UnifyM m, UnifyE m) => Elim -> Elim -> Eff m ()
unifyElim l r = case (l, r) of
  (EApp licit arg, EApp licit' arg') ->
    unifyArg (licit, arg) (licit', arg')
