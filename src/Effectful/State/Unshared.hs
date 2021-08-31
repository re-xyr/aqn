module Effectful.State.Unshared
  ( State
  , runState
  , evalState
  , execState
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import           Effectful.Internal.Effect
import           Effectful.Internal.Env
import           Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), shareable, mutable state of type @s@.
newtype State s :: Effect where
  State :: IORef s -> State s m r

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  v <- unsafeEff_ $ newIORef s
  a <- evalEffect (IdE (State v)) m
  (a, ) <$> unsafeEff_ (readIORef v)

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s m = do
  v <- unsafeEff_ $ newIORef s
  evalEffect (IdE (State v)) m

execState :: s -> Eff (State s : es) a -> Eff es s
execState s m = do
  v <- unsafeEff_ $ newIORef s
  _ <- evalEffect (IdE (State v)) m
  unsafeEff_ $ readIORef v

get :: State s :> es => Eff es s
get = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  readIORef v

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put s = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  writeIORef v s

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  x <- readIORef v
  let (r, x') = f x
  writeIORef v x'
  pure r

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  x <- readIORef v
  (r, x') <- unEff (f x) es
  writeIORef v x'
  pure r

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
