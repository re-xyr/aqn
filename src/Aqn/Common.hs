{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Aqn.Common where

import           Control.Lens.Tuple    (Field1, Field2, Field3)
import           Data.Text             (Text)
import           Effectful.Handler     (interpret, send)
import           Effectful.Monad       (Eff, Effect, type (:>))
import           Effectful.State.Local (State, get, put)
import           GHC.Generics          (Generic)

type Name = Text

-- -- | Interpret the state as an 'STRef'.
-- runStateST :: ST th :> m => s -> Eff (State s ': m) a -> Eff m a
-- runStateST ini act = _
-- {-# INLINE runStateST #-}

-- -- | Interpret the state as an 'IORef'; this is the lifted version of 'runStateST'.
-- runStateIO :: IOE :> m => s -> Eff (State s ': m) a -> Eff m a
-- runStateIO ini act = do
--   x <- sendM $ newIORef ini
--   inter
-- {-# INLINE runStateIO #-}

-- data Fresh :: Effect where
--   Fresh :: Fresh m Int

-- fresh :: Fresh :> m => Eff m Int
-- fresh = send Fresh
-- {-# INLINE fresh #-}

-- runFresh :: State Int :> m => Eff (Fresh ': m) a -> Eff m a
-- runFresh = interpret \_ -> \case
--   Fresh -> do
--     x <- get
--     put (x + 1)
--     pure x
-- {-# INLINE runFresh #-}

fresh :: State Int :> m => Eff m Int
fresh = do
  x <- get
  put (x + 1)
  pure x

-- | Distinguishes if an argument or parameter is implicit (can be inserted) or explicit (must be filled in manually).
data Licit
  = Implicit
  | Explicit
  deriving (Eq, Show)

-- | Strict pair.
data Pr a b = (:::)
  { p1 :: a
  , p2 :: b
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance Field1 (Pr a b) (Pr a' b) a a'
instance Field2 (Pr a b) (Pr a b') b b'

-- | Strict 3-tuple.
data Tp a b c = Tp
  { t1 :: a
  , t2 :: b
  , t3 :: c
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance Field1 (Tp a b c) (Tp a' b c) a a'
instance Field2 (Tp a b c) (Tp a b' c) b b'
instance Field3 (Tp a b c) (Tp a b c') c c'

-- | Uncurry on the strict pair type.
uncurry' :: (a -> b -> c) -> Pr a b -> c
uncurry' f (x ::: y) = f x y
{-# INLINE uncurry' #-}
