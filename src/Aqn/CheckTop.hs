module Aqn.CheckTop where

import           Aqn.Check
import           Aqn.Common
import           Aqn.Eval
import           Aqn.Global
import           Aqn.Presyntax
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Availability        (Effs)
import           Availability.Error  (Catcher, Thrower, catchError, throwError)
import           Control.Lens        (_Just, (?~), (^.), (^?!))
import           Control.Monad.Extra (fromMaybeM)
import           Data.Function       ((&))
import           Data.Functor        ((<&>))
import           Data.Sequence       (Seq ((:<|)), (<|))
import qualified Data.Sequence       as Seq
import qualified Data.Tsil           as Tsil

type TopM = (Impure, Writing 'Funs, Writing 'Metas, Writing 'Locals, Effs '[Thrower CheckError, Catcher CheckError])

-- | Check a top-level declaration and put it into global environment.
checkTop :: TopM => Decl -> TCM ()
checkTop m = case m of
  DFunHead fv params ret -> do
    (paramsT', retT) <- checkParams topCtx params ret
    let paramsT = Tsil.toSeq paramsT'
    tele <- lift $ evalTele [] paramsT retT
    updateFun fv (\f -> f & funCore ?~ FunCore paramsT retT tele (teleToTy tele) Nothing)
  DFunBody fv params body -> do
    let fakeup = checkTop (fakeFunHead fv params) `catchError` \(_ :: CheckError) -> throwError $ NotSuccessfullyClaimed m (DVFun fv)
    fun <- fromMaybeM (fakeup *> (readFun fv <&> (^?! funCore . _Just))) (readFun fv <&> (^. funCore))
    (ctx, ret) <- teleToCtx topCtx (fun ^. funParams) (fun ^. funTele) params
    bodyT <- check ctx body ret
    bodyV <- lift $ evalFunBody [] params bodyT
    updateFun fv \x -> x & funCore ?~ (fun & funBody ?~ (bodyT ::: bodyV))
  where
    teleToCtx :: Ctx -> Seq (Par Term) -> Tele -> Seq Bind -> TCM (Ctx, Val)
    teleToCtx ctx _ (Nil ret) [] = pure (ctx, ret)
    teleToCtx ctx pars (Cons l _ dom cod) ((Tp l' _ r) :<| xs)
      | l /= l' = throwError $ IncorrectParamList m pars
      | otherwise = teleToCtx (bound r dom ctx) pars (cod $ neuLoc r) xs
    teleToCtx _ pars _ _ = throwError $ IncorrectParamList m pars

fakeFunHead :: FunVar -> Seq Bind -> Decl
fakeFunHead fv pars = DFunHead fv (fmap (::: XHole) pars) XHole
{-# INLINE fakeFunHead #-}

checkParams :: TopM => Ctx -> Seq (Par Expr) -> Expr -> TCM (Seq (Par Term), Term)
checkParams ctx Seq.Empty ret = do
  retT <- check ctx ret VU
  pure ([], retT)
checkParams ctx (b@(Tp _ _ r) ::: ty :<| xs) ret = do
  tyT <- check ctx ty VU
  tyV <- lift $ eval' ctx tyT
  (rest, retT) <- checkParams (bound r tyV ctx) xs ret
  pure (b ::: tyT <| rest, retT)
