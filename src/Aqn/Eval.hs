module Aqn.Eval where

import           Aqn.Common
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Value
import           Control.Lens        (_2, _Just, previews)
import           Control.Monad.Freer (Eff)
import           Data.Foldable       (Foldable (toList), foldlM)
import           Data.Function       ((&))
import           Data.Maybe          (fromMaybe)
import           Data.Sequence       (Seq ((:<|)))
import qualified Data.Sequence       as Seq
import           Data.Tsil           (List ((:>)))
import qualified Data.Tsil           as Tsil

type Env = List (Local, Val)

type EvalM = (Retrieve, Reading 'Metas, Reading 'Funs)

eval :: EvalM => Term -> Val
eval = eval' []

eval' :: EvalM => Env -> Term -> Val
eval' env tm = case tm of
  TLam l n x m  -> VLam l n (closure env x m)
  TPi l n x t m -> VPi l n (eval' env t) (closure env x m)
  TU            -> VU
  TApp l f x    -> vApply l (eval' env f) (eval' env x)
  TLoc r        -> fromMaybe (neuLoc r) (Tsil.lookup r env)
  TFun f xs     -> evalCall env f (Tsil.toList xs)
  TLet r x y    -> eval' (env :> (r, eval' env x)) y
  TMeta r ->
    let (Meta (MetaCore body)) = getMeta r
    in neu0 (HMeta r) (snd <$> body)

evalCall :: EvalM => Env -> FunVar -> List (Licit, Term) -> Val
evalCall env r args =
  let args' = fmap (eval' env <$>) args
  in neu0 (HFun r args') $
    getFun r & previews (funCore . _Just . funBody . _Just . _2) ($ snd <$> args')

closure :: EvalM => Env -> Local -> Term -> (Val -> Val)
closure env ref tm val = eval' (env :> (ref, val)) tm

data Unfold
  = Unfold
  | Enfold
  deriving (Show, Eq)

type QuoteM m = (Write m, Writing 'Locals, Reading 'Metas, Reading 'Funs)

quoteE, quoteU :: QuoteM m => Val -> Eff m Term
quoteE v = let ?unf = Enfold in quote v
quoteU v = let ?unf = Unfold in quote v

quote :: (QuoteM m, ?unf :: Unfold) => Val -> Eff m Term
quote val = case val of
  VPi licit n dom cod -> do
    fresh <- freshLocal n
    TPi licit n fresh <$> quote dom <*> quote (cod $ neuLoc fresh)
  VLam licit n body -> do
    fresh <- freshLocal n
    TLam licit n fresh <$> quote (body $ neuLoc fresh)
  VNeu hd els pre
    | ?unf == Unfold, Nothing <- pre -> do
      fd <- lift $ forceMaybe val
      maybe enfolded quote fd
    | ?unf == Unfold, Just x <- pre -> quote x
    | otherwise -> enfolded
    where
      enfolded = quoteHd hd >>= flip quoteElims els
  VU -> pure TU

quoteHd :: (QuoteM m, ?unf :: Unfold) => Head -> Eff m Term
quoteHd hd = case hd of
  HLoc ref      -> pure $ TLoc ref
  HMeta ref     -> pure $ TMeta ref
  HFun ref args -> TFun ref . Seq.fromList . toList <$> traverse (traverse quote) args

quoteElims :: (QuoteM m, ?unf :: Unfold, Foldable f) => Term -> f Elim -> Eff m Term
quoteElims = foldlM quoteElim

quoteElim :: (QuoteM m, ?unf :: Unfold) => Term -> Elim -> Eff m Term
quoteElim tm elim = case elim of
  EApp l val -> TApp l tm <$> quote val

type ForceM = (Retrieve, Reading 'Metas, Reading 'Funs)

forceMaybe :: ForceM => Val -> Maybe Val
forceMaybe val = case val of
  VNeu _ _ (Just v) -> Just $ force v
  VNeu (HMeta ref) elims Nothing ->
    getMeta ref & previews (metaCore . metaBody . _Just . _2) (`vApplyElims` elims)
  VNeu (HFun ref args) elims Nothing ->
    getFun ref & previews (funCore . _Just . funBody . _Just . _2) ((`vApplyElims` elims) . ($ snd <$> args))
  _ -> Nothing

force :: ForceM => Val -> Val
force val = fromMaybe val (forceMaybe val)

evalTele :: EvalM => Env -> Seq (Licit, Name, Local, Term) -> Term -> Tele
evalTele env Seq.Empty ret              = Nil $ eval' env ret
evalTele env ((l, n, r, ty) :<| xs) ret = Cons l n (eval' env ty) \x -> evalTele (env :> (r, x)) xs ret

evalFunBody :: EvalM => Env -> Seq (Licit, Name, Local) -> Term -> List Val -> Val
evalFunBody env params tm vals = eval' (env <> Tsil.zipWith (\(_, _, r) v -> (r, v)) (Tsil.toList params) vals) tm