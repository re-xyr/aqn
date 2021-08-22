module Aqn.CheckTop where

import           Aqn.Check
import           Aqn.Common
import           Aqn.Eval
import           Aqn.Presyntax
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Value
import           Control.Lens              (_Just, (?~), (^.), (^?!))
import           Control.Monad.Extra       (fromMaybeM)
import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Error (Error, catchError, throwError)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.Sequence             (Seq ((:<|)), (<|))
import qualified Data.Sequence             as Seq
import           Data.Tsil                 (List ((:>)))
import qualified Data.Tsil                 as Tsil

type TopM m = (Write m, Writing 'Funs, Writing 'Metas, Writing 'Locals, Member (Error CheckError) m)

checkTop :: TopM m => Decl -> Eff m ()
checkTop m = case m of
  DFunHead fv params ret -> do
    (paramsT', retT) <- checkParams [] params ret
    let paramsT = Tsil.toSeq paramsT'
    tele <- lift $ evalTele [] paramsT retT
    updateFun fv (\f -> f & funCore ?~ FunCore paramsT retT tele (teleToTy tele) Nothing)
  DFunBody fv params body -> do
    let fakeup = checkTop (fakeFunHead fv params) `catchError` \(_ :: CheckError) -> throwError $ NotSuccessfullyClaimed m (DVFun fv)
    fun <- fromMaybeM (fakeup *> (readFun fv <&> (^?! funCore . _Just))) (readFun fv <&> (^. funCore))
    (ctx, ret) <- teleToCtx [] (fun ^. funParams) (fun ^. funTele) params
    bodyT <- check ctx body ret
    bodyV <- lift $ evalFunBody [] params bodyT
    updateFun fv \x -> x & funCore ?~ (fun & funBody ?~ (bodyT, bodyV))
  where
    teleToCtx :: TopM m => Ctx -> Seq (Licit, Name, Local, Term) -> Tele -> Seq (Licit, Name, Local) -> Eff m (Ctx, Val)
    teleToCtx ctx _ (Nil ret) [] = pure (ctx, ret)
    teleToCtx ctx pars (Cons l _ dom cod) ((l', _, r) :<| xs)
      | l /= l' = throwError $ IncorrectParamList m pars
      | otherwise = teleToCtx (ctx :> (r, dom)) pars (cod $ neuLoc r) xs
    teleToCtx _ pars _ _ = throwError $ IncorrectParamList m pars

fakeFunHead :: FunVar -> Seq (Licit, Name, Local) -> Decl
fakeFunHead fv pars = DFunHead fv (fmap (\(l, n, r) -> (l, n, r, XHole)) pars) XHole

checkParams :: TopM m => Ctx -> Seq (Licit, Name, Local, Expr) -> Expr -> Eff m (Seq (Licit, Name, Local, Term), Term)
checkParams ctx Seq.Empty ret = do
  retT <- check ctx ret VU
  pure ([], retT)
checkParams ctx ((l, n, r, ty) :<| xs) ret = do
  tyT <- check ctx ty VU
  tyV <- lift $ eval tyT
  (rest, retT) <- checkParams (ctx :> (r, tyV)) xs ret
  pure ((l, n, r, tyT) <| rest, retT)
