{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Aqn.Common where

import           Control.Lens.Tuple        (Field1, Field2, Field3)
import           Control.Monad.Freer       (Eff, LastMember, interpretM, sendM, type (~>))
import           Control.Monad.Freer.State (State (Get, Put))
import           Control.Monad.ST          (ST)
import           Data.IORef                (newIORef, readIORef, writeIORef)
import           Data.STRef                (newSTRef, readSTRef, writeSTRef)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)

type Name = Text

-- | Interpret the state as an 'STRef'.
runStateST :: LastMember (ST s) m => a -> Eff (State a ': m) ~> Eff m
runStateST ini act = do
  x <- sendM $ newSTRef ini
  interpretM (\case
    Get   -> readSTRef x
    Put a -> writeSTRef x a) act
{-# INLINE runStateST #-}

-- | Interpret the state as an 'IORef'; this is the lifted version of 'runStateST'.
runStateIO :: LastMember IO m => a -> Eff (State a ': m) ~> Eff m
runStateIO ini act = do
  x <- sendM $ newIORef ini
  interpretM (\case
    Get   -> readIORef x
    Put a -> writeIORef x a) act
{-# INLINE runStateIO #-}

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
