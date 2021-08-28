module Aqn.Value where

import           Aqn.Common
import           Aqn.Ref
import           Data.Tsil  (List ((:>)))

data Val
  = VPi Licit Name ~Val (Val -> Val)
  | VLam Licit Name (Val -> Val)
  | VNeu Head (List Elim) (Maybe Val)
  | VU

data Head
  = HLoc Local
  | HMeta MetaVar
  | HFun FunVar (List (Licit, Val))

data Elim
  = EApp Licit ~Val

data Tele
  = Cons Licit Name ~Val (Val -> Tele)
  | Nil ~Val

neu0 :: Head -> Maybe Val -> Val
neu0 hd = VNeu hd []

neuLoc :: Local -> Val
neuLoc ref = neu0 (HLoc ref) Nothing

vApply :: Licit -> Val -> Val -> Val
vApply l f ~x = case f of
  VLam l' _ clos
    | l == l'   -> clos x
    | otherwise -> error "Applying the wrong licit"
  VNeu hd elims val -> VNeu hd (elims :> EApp l x) (flip (vApply l) x <$> val)
  _ -> error "Applying the wrong thing"

vApplyI, vApplyE :: Val -> Val -> Val
vApplyI = vApply Implicit
vApplyE = vApply Explicit

vApplyArgs :: Foldable f => Val -> f (Licit, Val) -> Val
vApplyArgs = foldl (\v' (l, x) -> vApply l v' x)

vApplyElims :: Foldable f => Val -> f Elim -> Val
vApplyElims = foldl vApplyElim

vApplyElim :: Val -> Elim -> Val
vApplyElim x el = case el of
  EApp licit arg -> vApply licit x arg

teleToTy :: Tele -> Val
teleToTy (Nil ret)          = ret
teleToTy (Cons l n ty rest) = VPi l n ty (teleToTy . rest)
