{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Common where

import           Control.Lens.Tuple  (Field1, Field2, Field3)
import           Control.Monad.Freer (Eff, LastMember, Member, interpretM, send, type (~>))
import           Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

type Name = Text

data Store r a where
  NewStore :: a -> Store r (r a)
  WriteStore :: r a -> a -> Store r ()
  ReadStore :: r a -> Store r a

newStore :: Member (Store r) m => a -> Eff m (r a)
newStore x = send (NewStore x)
{-# INLINE newStore #-}

writeStore :: Member (Store r) m => r a -> a -> Eff m ()
writeStore r x = send (WriteStore r x)
{-# INLINE writeStore #-}

readStore :: Member (Store r) m => r a -> Eff m a
readStore r = send (ReadStore r)
{-# INLINE readStore #-}

type Store' = Store IORef

runStore :: LastMember IO m => Eff (Store IORef ': m) ~> Eff m
runStore = interpretM \case
  NewStore x     -> newIORef x
  WriteStore r x -> writeIORef r x
  ReadStore r    -> readIORef r
{-# INLINE runStore #-}

data Kind
  = Locals
  | Metas
  | Funs
  deriving (Show, Eq)

class Writing (a :: Kind)
class Reading (a :: Kind)
instance Writing a => Reading a

data Licit
  = Implicit
  | Explicit
  deriving (Eq, Show)

-- Strict pair.
data Pr a b = (:::)
  { p1 :: a
  , p2 :: b
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance Field1 (Pr a b) (Pr a' b) a a'
instance Field2 (Pr a b) (Pr a b') b b'

-- Strict 3-tuple.
data Tp a b c = Tp
  { t1 :: a
  , t2 :: b
  , t3 :: c
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance Field1 (Tp a b c) (Tp a' b c) a a'
instance Field2 (Tp a b c) (Tp a b' c) b b'
instance Field3 (Tp a b c) (Tp a b c') c c'

uncurry' :: (a -> b -> c) -> Pr a b -> c
uncurry' f (x ::: y) = f x y
{-# INLINE uncurry' #-}
