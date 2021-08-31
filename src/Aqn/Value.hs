module Aqn.Value where

import           Aqn.Common
import           Aqn.Ref
import           Data.Foldable (Foldable (foldl'))
import           Data.Tsil     (List ((:>)))

data Val
  = VPi Licit Name Val (Val -> Val)
  | VLam Licit Name (Val -> Val)
  | VNeu Head (List Elim) (Maybe Val)
  | VU

data Head
  = HLoc Local
  | HMeta MetaVar
  | HFun FunVar (List (Arg Val))

data Elim
  = EApp Licit Val

data Tele
  = Cons Licit Name Val (Val -> Tele)
  | Nil Val

neu0 :: Head -> Maybe Val -> Val
neu0 hd = VNeu hd []
{-# INLINE neu0 #-}

neuLoc :: Local -> Val
neuLoc ref = neu0 (HLoc ref) Nothing
{-# INLINE neuLoc #-}

vApply :: Licit -> Val -> Val -> Val
vApply l f x = case f of
  VLam _ _ clos     -> clos x
  VNeu hd elims val -> VNeu hd (elims :> EApp l x) (flip (vApply l) x <$> val)
  _                 -> error "Applying the wrong thing"

vApplyI, vApplyE :: Val -> Val -> Val
vApplyI = vApply Implicit
{-# INLINE vApplyI #-}
vApplyE = vApply Explicit
{-# INLINE vApplyE #-}

vApplyArgs :: Foldable f => Val -> f (Arg Val) -> Val
vApplyArgs = foldl' (\v' (l ::: x) -> vApply l v' x)
{-# INLINE vApplyArgs #-}

vApplyElims :: Foldable f => Val -> f Elim -> Val
vApplyElims = foldl' vApplyElim
{-# INLINE vApplyElims #-}

vApplyElim :: Val -> Elim -> Val
vApplyElim x el = case el of
  EApp licit arg -> vApply licit x arg
{-# INLINE vApplyElim #-}

teleToTy :: Tele -> Val
teleToTy (Nil ret)          = ret
teleToTy (Cons l n ty rest) = VPi l n ty (teleToTy . rest)
