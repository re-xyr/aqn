{-# OPTIONS_GHC -Wno-everything -Wdefault #-}
module Main where

import           Aqn.Check
import           Aqn.CheckTop
import           Aqn.Common
import           Aqn.Eval
import           Aqn.Presyntax
import           Aqn.Prettyprint
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Unify                  (UnifyM, unify)
import           Aqn.Value
import           Control.DeepSeq            (NFData)
import           Control.Lens               (_1, _Just, (^?!))
import           Control.Monad              (replicateM, replicateM_)
import           Control.Monad.Freer        (Eff, runM)
import           Control.Monad.Freer.Error  (catchError, runError)
import           Control.Monad.Freer.Fresh  (evalFresh)
import           Control.Monad.Freer.Reader (runReader)
import           Data.Functor               ((<&>))
import           Data.IORef                 (newIORef)
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.Text                  as T
import           Data.Time.Clock
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
      tapty <- check [] eapty VU
      vapty <- lift $ eval tapty
      -- tap <- check [] eap vapty
      -- tsmty <- check [] esmty VU
      -- vsmty <- lift $ eval tsmty
      tidty <- check [] eidty VU
      vidty <- lift $ eval tidty
      catchError (do
        tmm <- check [] (eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt `eApp` eidt) vidty
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
  globalR <- newIORef (Global IntMap.empty IntMap.empty IntMap.empty)
  runM $ runError $ runStore $ evalFresh 0 $ runReader globalR example1

-- >>> example1'
-- Right λ{l}. (λ{e}. λf. f) {?27 {l}} ((λ{e}. λf. f) {?28 {l}}) ((λ{e}. λf. f) {?31 {l}}) ((λ{e}. λf. f) {?34 {l}}) ((λ{e}. λf. f) {?37 {l}}) ((λ{e}. λf. f) {?40 {l}}) ((λ{e}. λf. f) {?43 {l}}) ((λ{e}. λf. f) {?46 {l}}) ((λ{e}. λf. f) {?49 {l}}) ((λ{e}. λf. f) {?52 {l}}) ((λ{e}. λf. f) {?55 {l}}) ((λ{e}. λf. f) {?58 {l}}) ((λ{e}. λf. f) {?61 {l}}) ((λ{e}. λf. f) {?64 {l}}) ((λ{e}. λf. f) {?67 {l}}) ((λ{e}. λf. f) {?70 {l}}) ((λ{e}. λf. f) {?73 {l}}) ((λ{e}. λf. f) {?76 {l}}) ((λ{e}. λf. f) {?79 {l}}) ((λ{e}. λf. f) {?82 {l}}) ((λ{e}. λf. f) {?85 {l}}) ((λ{e}. λf. f) {?88 {l}}) ((λ{e}. λf. f) {?91 {l}}) ((λ{e}. λf. f) {?94 {l}}) ((λ{e}. λf. f) {?97 {l}}) ((λ{e}. λf. f) {?100 {l}}) ((λ{e}. λf. f) {?103 {l}}) ((λ{e}. λf. f) {?106 {l}}) ((λ{e}. λf. f) {?109 {l}}) ((λ{e}. λf. f) {?112 {l}}) ((λ{e}. λf. f) {?115 {l}}) ((λ{e}. λf. f) {?118 {l}}) ((λ{e}. λf. f) {?121 {l}}) ((λ{e}. λf. f) {?124 {l}}) ((λ{e}. λf. f) {?127 {l}}) ((λ{e}. λf. f) {?130 {l}}) ((λ{e}. λf. f) {?133 {l}}) ((λ{e}. λf. f) {?136 {l}}) ((λ{e}. λf. f) {?139 {l}}) ((λ{e}. λf. f) {?142 {l}}) ((λ{e}. λf. f) {?145 {l}}) ((λ{e}. λf. f) {?148 {l}}) ((λ{e}. λf. f) {?151 {l}}) ((λ{e}. λf. f) {?154 {l}}) ((λ{e}. λf. f) {?157 {l}}) ((λ{e}. λf. f) {?160 {l}}) ((λ{e}. λf. f) {?163 {l}}) ((λ{e}. λf. f) {?166 {l}}) ((λ{e}. λf. f) {?169 {l}}) ((λ{e}. λf. f) {?172 {l}}) ((λ{e}. λf. f) {?175 {l}}) ((λ{e}. λf. f) {?178 {l}}) ((λ{e}. λf. f) {?181 {l}}) ((λ{e}. λf. f) {?184 {l}}) ((λ{e}. λf. f) {?187 {l}}) ((λ{e}. λf. f) {?190 {l}}) ((λ{e}. λf. f) {?193 {l}})

-- example2 :: TopM m => Eff m (Doc ann)
-- example2 = do
--   xs <- sequence (freshLocal <$> ((T.pack . (:[]) <$> ['a'..'z']) ++ (T.pack . (:"'") <$> ['a'..'z'])))
--   let fid = FunVar 53
--   let fidTest = FunVar 54
--   let fNat = FunVar 55
--   let fzero = FunVar 56
--   let fsuc = FunVar 57
--   let fVec = FunVar 58
--   let fvnil = FunVar 59
--   case xs of
--     a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n' -> do
--       let dhid = DFunHead fid [(Implicit, "a", a, XU), (Explicit, "b", b, XLoc a)] (XLoc a)
--       let dbid = DFunBody fid [(Implicit, "c", c), (Explicit, "d", d)] (XLoc d)
--       let dhidTest = DFunHead fidTest [] (ePiI "e" e XU $ ePi "f" f (XLoc e) $ XLoc e)
--       let dbidTest = DFunBody fidTest [] (XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid `eApp` XFun fid)
--       let dhNat = DFunHead fNat [] XU
--       let dbNat = DFunBody fNat [] (ePi "g" g XU $ ePi "h" h (ePi "i" i (XLoc g) (XLoc g)) $ ePi "j" j (XLoc g) $ XLoc g)
--       let dhzero = DFunHead fzero [] (XFun fNat)
--       let dbzero = DFunBody fzero [] (eLam "j" j $ eLam "k" k $ eLam "l" l $ XLoc l)
--       let dhsuc = DFunHead fsuc [] (ePi "b'" b' (XFun fNat) (XFun fNat))
--       let dbsuc = DFunBody fsuc [] (eLam "m" m $ eLam "n" n $ eLam "o" o $ eLam "a'" a' $ XLoc o `eApp` (XLoc m `eApp` XLoc n `eApp` XLoc o `eApp` XLoc a'))
--       let dhVec = DFunHead fVec [(Explicit, "p", p, XU), (Explicit, "q", q, XFun fNat)] XU
--       let dbVec = DFunBody fVec [(Explicit, "r", r), (Explicit, "s", s)] (ePi "t" t (ePi "u" u (XFun fNat) XU) $ ePi "v" v (XLoc t `eApp` XFun fzero) $ ePi "w" w (ePiI "x" x XHole $ ePi "y" y (XLoc r) $ ePi "z" z (XLoc t `eApp` XLoc x) $ (XLoc t `eApp` (XFun fsuc `eApp` XLoc x))) $ XLoc t `eApp` XLoc s)
--       let dhvnil = DFunHead fvnil [(Implicit, "c'", c', XHole)] (XFun fVec `eApp` XLoc c' `eApp` XFun fzero)
--       let dbvnil = DFunBody fvnil [(Implicit, "d'", d')] (eLam "e'" e' $ eLam "f'" f' $ eLam "g'" g' (XLoc f'))
--       catchError (do
--         checkTop dhid
--         checkTop dbid
--         checkTop dhidTest
--         checkTop dbidTest
--         checkTop dhNat
--         checkTop dbNat
--         checkTop dhzero
--         checkTop dbzero
--         checkTop dhsuc
--         checkTop dbsuc
--         checkTop dhVec
--         checkTop dbVec
--         checkTop dhvnil
--         checkTop dbvnil
--         body <- (readFun fidTest) <&> (^?! funCore . _Just . funBody . _Just . _1)
--         lift $ prettyTerm body) \(e :: CheckError) -> lift $ prettyCheckError e
--     _ -> undefined

-- example2' :: (Either CheckError (Doc ann))
-- {-# NOINLINE example2' #-}
-- example2' = unsafePerformIO do
--   globalR <- newIORef (Global IntMap.empty (IntMap.fromList [(53, (Fun "id" Nothing)), (54, (Fun "idTest" Nothing)), (55, (Fun "Nat" Nothing)), (56, (Fun "zero" Nothing)), (57, (Fun "suc" Nothing)), (58, (Fun "Vec" Nothing)), (59, (Fun "vnil" Nothing))]) IntMap.empty)
--   runM $ runError $ runStore $ evalFresh 0 $ runReader globalR example2

-- >>> example2'
-- Right λ{e}. (λ{a}. λb. id[{a}, b]) {?55 {e}} ((λ{a}. λb. id[{a}, b]) {?58 {e}}) ((λ{a}. λb. id[{a}, b]) {?63 {e}}) ((λ{a}. λb. id[{a}, b]) {?68 {e}}) ((λ{a}. λb. id[{a}, b]) {?73 {e}}) ((λ{a}. λb. id[{a}, b]) {?78 {e}}) ((λ{a}. λb. id[{a}, b]) {?83 {e}}) ((λ{a}. λb. id[{a}, b]) {?88 {e}}) ((λ{a}. λb. id[{a}, b]) {?93 {e}}) ((λ{a}. λb. id[{a}, b]) {?98 {e}}) ((λ{a}. λb. id[{a}, b]) {?103 {e}}) ((λ{a}. λb. id[{a}, b]) {?108 {e}}) ((λ{a}. λb. id[{a}, b]) {?113 {e}}) ((λ{a}. λb. id[{a}, b]) {?118 {e}}) ((λ{a}. λb. id[{a}, b]) {?123 {e}}) ((λ{a}. λb. id[{a}, b]) {?128 {e}}) ((λ{a}. λb. id[{a}, b]) {?133 {e}}) ((λ{a}. λb. id[{a}, b]) {?138 {e}}) ((λ{a}. λb. id[{a}, b]) {?143 {e}}) ((λ{a}. λb. id[{a}, b]) {?148 {e}}) ((λ{a}. λb. id[{a}, b]) {?153 {e}}) ((λ{a}. λb. id[{a}, b]) {?158 {e}}) ((λ{a}. λb. id[{a}, b]) {?163 {e}}) ((λ{a}. λb. id[{a}, b]) {?168 {e}}) ((λ{a}. λb. id[{a}, b]) {?173 {e}}) ((λ{a}. λb. id[{a}, b]) {?178 {e}}) ((λ{a}. λb. id[{a}, b]) {?183 {e}}) ((λ{a}. λb. id[{a}, b]) {?188 {e}}) ((λ{a}. λb. id[{a}, b]) {?193 {e}}) ((λ{a}. λb. id[{a}, b]) {?198 {e}}) ((λ{a}. λb. id[{a}, b]) {?203 {e}}) ((λ{a}. λb. id[{a}, b]) {?208 {e}}) ((λ{a}. λb. id[{a}, b]) {?213 {e}}) ((λ{a}. λb. id[{a}, b]) {?218 {e}}) ((λ{a}. λb. id[{a}, b]) {?223 {e}}) ((λ{a}. λb. id[{a}, b]) {?228 {e}}) ((λ{a}. λb. id[{a}, b]) {?233 {e}}) ((λ{a}. λb. id[{a}, b]) {?238 {e}}) ((λ{a}. λb. id[{a}, b]) {?243 {e}}) ((λ{a}. λb. id[{a}, b]) {?248 {e}})

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
  globalR <- newIORef (Global IntMap.empty IntMap.empty IntMap.empty)
  runM $ runStore $ evalFresh 0 $ runReader globalR example3

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
