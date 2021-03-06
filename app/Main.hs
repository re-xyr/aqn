{-# OPTIONS_GHC -Wno-everything -Wdefault #-}
module Main where

import           Aqn.Check
import           Aqn.CheckTop
import           Aqn.Common
import           Aqn.Eval
import           Aqn.Global
import           Aqn.Presyntax
import           Aqn.Pretty
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Unify                  (UnifyM, unify)
import           Aqn.Value
import           Control.DeepSeq            (NFData, deepseq)
import           Control.Lens               (_1, _Just, (^?!))
import           Control.Monad              (replicateM, replicateM_)
import           Control.Monad.Freer        (Eff, runM)
import           Control.Monad.Freer.Error  (catchError, runError)
import           Control.Monad.Freer.Fresh  (evalFresh)
import           Control.Monad.Freer.Reader (runReader)
import           Control.Monad.ST           (stToIO)
import           Data.Functor               ((<&>))
import           Data.IORef                 (newIORef)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Reflection            (give)
import qualified Data.Text                  as T
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           GHC.Generics               (Generic, Generic1)
import           GHC.IO                     (unsafePerformIO)
import           Prettyprinter              (Doc)

eLam :: Name -> Local -> Expr -> Expr
eLam = XLam Explicit

eLamI :: Name -> Local -> Expr -> Expr
eLamI = XLam Implicit

eApp :: Expr -> Expr -> Expr
eApp = XApp ExplicitA

eAppI :: Expr -> Expr -> Expr
eAppI = XApp (ImplicitN "sth")

tLam :: Name -> Local -> Term -> Term
tLam = TLam Explicit

tApp :: Term -> Term -> Term
tApp = TApp Explicit

ePi :: Name -> Local -> Expr -> Expr -> Expr
ePi = XPi Explicit

ePiI :: Name -> Local -> Expr -> Expr -> Expr
ePiI = XPi Implicit

example1 :: (CheckM m) => Eff m (Doc ann)
example1 = do
  xs <- sequence (freshLocal . T.pack . (:[]) <$> ['a'..'z'])
  case xs of
    a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q -> do
      let eap = eLam "a" a $ eLam "b" b $ eLam "c" c $ eLam "d" d $ eApp (XLoc c) (XLoc d)
      let eapty = ePi "g" g XU $ ePi "h" h XU $ ePi "i" i (ePi "j" j (XLoc g) $ XLoc h) $ ePi "k" k (XLoc g) $ XLoc h
      -- let eid = eLam "e" e $ eLam "f" f $ XLoc f
      -- let eidty = ePi "l" l XU $ ePi "m" m (XLoc l) $ XLoc l
      let eid = eLamI "e" e $ eLam "f" f $ XLoc f
      let eidty = ePiI "l" l XU $ ePi "m" m (XLoc l) $ XLoc l
      let eapt = XTy eap eapty
      let eidt = XTy eid eidty
      let eapp = eapt `eApp` eidty `eApp` eidty `eApp` (eidt `eApp` eidty) `eApp` eidt
      -- let esmty = ePi "n" n (ePi "o" o XU XU) (ePi "p" p XU XU)
      let esmty = ePi "n" n XU XU
      tapty <- check topCtx eapty VU
      vapty <- lift $ eval [] tapty
      -- tap <- check [] eap vapty
      -- tsmty <- check [] esmty VU
      -- vsmty <- lift $ eval tsmty
      tidty <- check topCtx eidty VU
      vidty <- lift $ eval [] tidty
      catchError (do
        tmm <- check topCtx (eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt) vidty
        lift (prettyTerm tmm)) \(e :: CheckError) -> lift $ prettyCheckError e
      -- tidty <- check [] eidty VU
      -- vidty <- lift $ eval tidty
      -- tid <- check [] eid vidty
      -- tapp <- check [] eapp vidty
      -- vapp <- lift $ eval tapp
      -- qapp <- lift' $ quoteE vapp
      -- pure [tapty, tap, tidty, tid]
    _ -> undefined

instance Writing a

example1' :: (Either CheckError (Doc ann))
{-# NOINLINE example1' #-}
example1' = unsafePerformIO do
  let ini = Global IntMap.empty IntMap.empty IntMap.empty
  runM $ runError $ evalStateIO ini $ evalFresh 0 example1

-- >>> example1'
-- Right ??{l}. (??{e}. ??f. f) {?27 {l}} ((??{e}. ??f. f) {?28 {l}}) ((??{e}. ??f. f) {?31 {l}}) ((??{e}. ??f. f) {?34 {l}}) ((??{e}. ??f. f) {?37 {l}}) ((??{e}. ??f. f) {?40 {l}}) ((??{e}. ??f. f) {?43 {l}}) ((??{e}. ??f. f) {?46 {l}}) ((??{e}. ??f. f) {?49 {l}}) ((??{e}. ??f. f) {?52 {l}}) ((??{e}. ??f. f) {?55 {l}}) ((??{e}. ??f. f) {?58 {l}}) ((??{e}. ??f. f) {?61 {l}}) ((??{e}. ??f. f) {?64 {l}}) ((??{e}. ??f. f) {?67 {l}}) ((??{e}. ??f. f) {?70 {l}}) ((??{e}. ??f. f) {?73 {l}}) ((??{e}. ??f. f) {?76 {l}}) ((??{e}. ??f. f) {?79 {l}}) ((??{e}. ??f. f) {?82 {l}}) ((??{e}. ??f. f) {?85 {l}}) ((??{e}. ??f. f) {?88 {l}}) ((??{e}. ??f. f) {?91 {l}}) ((??{e}. ??f. f) {?94 {l}}) ((??{e}. ??f. f) {?97 {l}}) ((??{e}. ??f. f) {?100 {l}}) ((??{e}. ??f. f) {?103 {l}}) ((??{e}. ??f. f) {?106 {l}}) ((??{e}. ??f. f) {?109 {l}}) ((??{e}. ??f. f) {?112 {l}}) ((??{e}. ??f. f) {?115 {l}}) ((??{e}. ??f. f) {?118 {l}}) ((??{e}. ??f. f) {?121 {l}}) ((??{e}. ??f. f) {?124 {l}}) ((??{e}. ??f. f) {?127 {l}}) ((??{e}. ??f. f) {?130 {l}}) ((??{e}. ??f. f) {?133 {l}}) ((??{e}. ??f. f) {?136 {l}}) ((??{e}. ??f. f) {?139 {l}}) ((??{e}. ??f. f) {?142 {l}}) ((??{e}. ??f. f) {?145 {l}}) ((??{e}. ??f. f) {?148 {l}}) ((??{e}. ??f. f) {?151 {l}}) ((??{e}. ??f. f) {?154 {l}}) ((??{e}. ??f. f) {?157 {l}}) ((??{e}. ??f. f) {?160 {l}}) ((??{e}. ??f. f) {?163 {l}}) ((??{e}. ??f. f) {?166 {l}}) ((??{e}. ??f. f) {?169 {l}}) ((??{e}. ??f. f) {?172 {l}}) ((??{e}. ??f. f) {?175 {l}}) ((??{e}. ??f. f) {?178 {l}}) ((??{e}. ??f. f) {?181 {l}}) ((??{e}. ??f. f) {?184 {l}}) ((??{e}. ??f. f) {?187 {l}}) ((??{e}. ??f. f) {?190 {l}}) ((??{e}. ??f. f) {?193 {l}})

example2 :: TopM m => Eff m (Doc ann)
example2 = do
  xs <- sequence (freshLocal <$> ((T.pack . (:[]) <$> ['a'..'z']) ++ (T.pack . (:"'") <$> ['a'..'z'])))
  let fid = FunVar 53
  let fidTest = FunVar 54
  let fNat = FunVar 55
  let fzero = FunVar 56
  let fsuc = FunVar 57
  let fVec = FunVar 58
  let fvnil = FunVar 59
  case xs of
    a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n' -> do
      let dhid = DFunHead fid [Tp Implicit "a" a ::: XU, Tp Explicit "b" b ::: XLoc a] (XLoc a)
      let dbid = DFunBody fid [Tp Implicit "c" c, Tp Explicit "d" d] (XLoc d)
      let dhidTest = DFunHead fidTest [] (ePiI "e" e XU $ ePi "f" f (XLoc e) $ XLoc e)
      let dbidTest = DFunBody fidTest [] (XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid)
      let dhNat = DFunHead fNat [] XU
      let dbNat = DFunBody fNat [] (ePi "g" g XU $ ePi "h" h (ePi "i" i (XLoc g) (XLoc g)) $ ePi "j" j (XLoc g) $ XLoc g)
      let dhzero = DFunHead fzero [] (XFun fNat)
      let dbzero = DFunBody fzero [] (eLam "j" j $ eLam "k" k $ eLam "l" l $ XLoc l)
      let dhsuc = DFunHead fsuc [] (ePi "b'" b' (XFun fNat) (XFun fNat))
      let dbsuc = DFunBody fsuc [] (eLam "m" m $ eLam "n" n $ eLam "o" o $ eLam "a'" a' $ XLoc o `eApp` (XLoc m `eApp` XLoc n `eApp` XLoc o `eApp` XLoc a'))
      let dhVec = DFunHead fVec [Tp Explicit "p" p ::: XU, Tp Explicit "q" q ::: XFun fNat] XU
      let dbVec = DFunBody fVec [Tp Explicit "r" r, Tp Explicit "s" s] (ePi "t" t (ePi "u" u (XFun fNat) XU) $ ePi "v" v (XLoc t `eApp` XFun fzero) $ ePi "w" w (ePiI "x" x XHole $ ePi "y" y (XLoc r) $ ePi "z" z (XLoc t `eApp` XLoc x) $ (XLoc t `eApp` (XFun fsuc `eApp` XLoc x))) $ XLoc t `eApp` XLoc s)
      let dhvnil = DFunHead fvnil [Tp Implicit "c'" c' ::: XHole] (XFun fVec `eApp` XLoc c' `eApp` XFun fzero)
      let dbvnil = DFunBody fvnil [Tp Implicit "d'" d'] (eLam "e'" e' $ eLam "f'" f' $ eLam "g'" g' (XLoc f'))
      catchError (do
        checkTop dhid
        checkTop dbid
        checkTop dhidTest
        checkTop dbidTest
        checkTop dhNat
        checkTop dbNat
        checkTop dhzero
        checkTop dbzero
        checkTop dhsuc
        checkTop dbsuc
        checkTop dhVec
        checkTop dbVec
        checkTop dhvnil
        checkTop dbvnil
        body <- (readFun fVec) <&> (^?! funCore . _Just . funBody . _Just . _1)
        lift $ prettyTerm body) \(e :: CheckError) -> lift $ prettyCheckError e
    _ -> undefined

example2' :: (Either CheckError (Doc ann))
{-# NOINLINE example2' #-}
example2' = unsafePerformIO do
  let ini = Global IntMap.empty (IntMap.fromList [(53, (Fun "id" Nothing)), (54, (Fun "idTest" Nothing)), (55, (Fun "Nat" Nothing)), (56, (Fun "zero" Nothing)), (57, (Fun "suc" Nothing)), (58, (Fun "Vec" Nothing)), (59, (Fun "vnil" Nothing))]) IntMap.empty
  runM $ runError $ evalStateIO ini $ evalFresh 0 example2

-- >>> example2'
-- Right ??(t : ??(u : Nat[]) -> U) -> ??(v : t zero[]) -> ??(w : ??{x : ?258 {r} {s} {t} {v}} -> ??(y : r) -> ??(z : t x) -> t (suc[] x)) -> t s

vLam :: Name -> (Val -> Val) -> Val
{-# INLINE vLam #-}
vLam = VLam Explicit

vApp :: Val -> Val -> Val
{-# INLINE vApp #-}
vApp = vApply Explicit

example3 :: (UnifyM m) => Eff m Term
example3 = do
  let zero = vLam "s" \s -> vLam "z" \z -> z
  let suc = vLam "n" \n -> vLam "s" \s -> vLam "z" \z -> s `vApp` (n `vApp` s `vApp` z)
  let add = vLam "m" \m -> vLam "n" \n -> vLam "s" \s -> vLam "z" \z -> m `vApp` s `vApp` (n `vApp` s `vApp` z)
  let mul = vLam "m" \m -> vLam "n" \n -> vLam "s" \s -> vLam "z" \z -> m `vApp` (n `vApp` s) `vApp` z
  let two = suc `vApp` (suc `vApp` zero)
  let five = suc `vApp` (suc `vApp` (suc `vApp` (suc `vApp` (suc `vApp` zero))))
  let n10 = mul `vApp` two `vApp` five
  let n10b = mul `vApp` five `vApp` two
  let n100 = mul `vApp` n10 `vApp` n10
  let n100b = mul `vApp` n10b `vApp` n10b
  let n10k = mul `vApp` n100 `vApp` n100
  let n10kb = mul `vApp` n100b `vApp` n100b
  let n100k = mul `vApp` n10k `vApp` n10
  let n100kb = mul `vApp` n10kb `vApp` n10b
  let n1M = mul `vApp` n10k `vApp` n100
  let n1Mb = mul `vApp` n10kb `vApp` n100b
  let n5M = mul `vApp` n1M `vApp` five
  let n5Mb = mul `vApp` n1Mb `vApp` five
  let n10M = mul `vApp` n1M `vApp` n10
  let n10Mb = mul `vApp` n1Mb `vApp` n10b
  unify n10M n10Mb >>= \case
    Nothing -> pure TU
    Just _  -> pure (TLoc (Local 0))
  -- m10 <- quoteE n10M
  -- pure $ m10 `deepseq` m10

deriving instance NFData a => NFData (Pr Licit a)
deriving instance Generic Licit
deriving instance NFData Licit
deriving instance Generic Local
deriving instance NFData Local
deriving instance Generic FunVar
deriving instance NFData FunVar
deriving instance Generic MetaVar
deriving instance NFData MetaVar
deriving instance Generic Term
deriving instance NFData Term

example3' :: IO Term
example3' = do
  let ini = Global IntMap.empty IntMap.empty IntMap.empty
  runM $ evalStateIO ini $ evalFresh 0 example3

main :: IO ()
main = do
  t1 <- getCurrentTime
  example3'
  t2 <- getCurrentTime
  print (diffUTCTime t2 t1)

-- >>> example3'

-- example2 :: UnifyM m => Eff m Bool
-- example2 =
--   unify (VNeu (HMeta metavar) [] Nothing) VU
--   where metavar = MetaVar 0

-- example2' :: Bool
-- {-# NOINLINE example2' #-}
-- example2' = unsafePerformIO do
--   globalR <- newIORef (Global (IntMap.fromList [(0, Meta (MetaCore Nothing))]) IntMap.empty)
--   runM $ runStore $ evalFresh 1 $ let ?globalRef = globalR in example2

-- >>> example2'
-- True

-- f :: Given Int => Int
-- f = give 100 g

-- g :: Given Int => Int
-- g = given

-- >>> give (0 :: Int) f
-- 0
