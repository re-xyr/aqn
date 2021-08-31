module Aqn.Eval where

import           Aqn.Common
import           Aqn.Global
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Control.Lens    (_2, _Just, previews, (^?))
import           Data.Foldable   (Foldable (toList))
import           Data.Function   ((&))
import           Data.Maybe      (fromMaybe)
import           Data.Reflection (Given (given), give)
import           Data.Sequence   (Seq ((:<|)))
import qualified Data.Sequence   as Seq
import           Data.Tsil       (List ((:>)))
import qualified Data.Tsil       as Tsil
import           Effectful.Monad (Eff)

-- | The evaluation environment that corresponds 'Local' variables to 'Val' values.
type Env = List (Local, Val)

type EvalM = (Pure, Reading 'Metas, Reading 'Funs)

-- | Evaluates a core 'Term' into a beta-normal semantic value 'Val'.
eval :: EvalM => Env -> Term -> Val
eval env tm = case tm of
  TLam l n x m  -> VLam l n (closure env x m)
  TPi l n x t m -> VPi l n (eval env t) (closure env x m)
  TU            -> VU
  TApp l f x    -> vApply l (eval env f) (eval env x)
  TLoc r        -> fromMaybe (neuLoc r) (Tsil.lookup r env)
  TFun f xs     -> evalCall env f (Tsil.toList xs)
  TLet r x y    -> eval (env :> (r, eval env x)) y
  TMeta r       -> neu0 (HMeta r) (getMeta r ^? metaCore . metaBody . _Just . _2)

evalCall :: EvalM => Env -> FunVar -> List (Arg Term) -> Val
evalCall env r args =
  let args' = fmap (eval env <$>) args
  in neu0 (HFun r args') $
    getFun r & previews (funCore . _Just . funBody . _Just . _2) ($ p2 <$> args')
{-# INLINE evalCall #-}

closure :: EvalM => Env -> Local -> Term -> (Val -> Val)
closure env ref tm val = eval (env :> (ref, val)) tm
{-# INLINE closure #-}

data Unfold
  = Unfold
  | Enfold
  deriving (Show, Eq)

type QuoteM m = (Impure m, Writing 'Locals, Reading 'Metas, Reading 'Funs)

-- | Quote a 'Val' back into a syntactic 'Term' in beta-normal form.
quoteE :: QuoteM m => Val -> Eff m Term
quoteE v = give Enfold $ quote v
{-# INLINE quoteE #-}

-- | Quote a 'Val' back into a syntactic 'Term' in beta-delta-normal form.
quoteU :: QuoteM m => Val -> Eff m Term
quoteU v = give Unfold $ quote v
{-# INLINE quoteU #-}

quote :: (QuoteM m, Given Unfold) => Val -> Eff m Term
quote val = case val of
  VPi licit n dom cod -> do
    fresh <- freshLocal n
    TPi licit n fresh <$> quote dom <*> quote (cod $ neuLoc fresh)
  VLam licit n body -> do
    fresh <- freshLocal n
    TLam licit n fresh <$> quote (body $ neuLoc fresh)
  VNeu hd els pre
    | given == Unfold, Nothing <- pre -> do
      fd <- lift $ forceMaybe val
      maybe (quoteHd hd >>= flip quoteElims els) quote fd
    | given == Unfold, Just x <- pre -> quote x
    | otherwise -> quoteHd hd >>= flip quoteElims els
  VU -> pure TU

quoteHd :: (QuoteM m, Given Unfold) => Head -> Eff m Term
quoteHd hd = case hd of
  HLoc ref      -> pure $ TLoc ref
  HMeta ref     -> pure $ TMeta ref
  HFun ref args -> TFun ref . Seq.fromList . toList <$> traverse (traverse quote) args
{-# INLINE quoteHd #-}

quoteElims :: (QuoteM m, Given Unfold) => Term -> List Elim -> Eff m Term
quoteElims t Tsil.Empty = pure t
quoteElims t (xs :> x) = do
  t' <- quoteElim t x
  quoteElims t' xs

quoteElim :: (QuoteM m, Given Unfold) => Term -> Elim -> Eff m Term
quoteElim tm elim = case elim of
  EApp l val -> TApp l tm <$> quote val
{-# INLINE quoteElim #-}

type ForceM = (Pure, Reading 'Metas, Reading 'Funs)

forceMaybe :: ForceM => Val -> Maybe Val
forceMaybe val = case val of
  VNeu _ _ (Just v) -> Just $ force v
  VNeu (HMeta ref) elims Nothing ->
    getMeta ref & previews (metaCore . metaBody . _Just . _2) (`vApplyElims` elims)
  VNeu (HFun ref args) elims Nothing ->
    getFun ref & previews (funCore . _Just . funBody . _Just . _2) ((`vApplyElims` elims) . ($ p2 <$> args))
  _ -> Nothing

force :: ForceM => Val -> Val
force val = fromMaybe val (forceMaybe val)
{-# INLINE force #-}

evalTele :: EvalM => Env -> Seq (Par Term) -> Term -> Tele
evalTele env Seq.Empty ret                    = Nil $ eval env ret
evalTele env (((Tp l n r) ::: ty) :<| xs) ret = Cons l n (eval env ty) \x -> evalTele (env :> (r, x)) xs ret

evalFunBody :: EvalM => Env -> Seq Bind -> Term -> List Val -> Val
evalFunBody env params tm vals = eval (env <> Tsil.zipWith (\(Tp _ _ r) v -> (r, v)) (Tsil.toList params) vals) tm
{-# INLINE evalFunBody #-}
