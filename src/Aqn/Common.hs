{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Common where

import           Control.Monad.Freer       (Eff, LastMember, Member, interpretM, send, type (~>))
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)

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

onError :: Member (Error e2) m => Eff (Error e1 ': m) a -> (e1 -> e2) -> Eff m a
onError m f = runError m >>= \case
  Left e  -> throwError (f e)
  Right x -> pure x

(.!) :: Maybe a -> a
(.!) = fromJust
