module Control.Monad.Store.Class where

import           Control.Monad.Cont          (ContT)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Identity      (IdentityT)
import qualified Control.Monad.RWS.Lazy      as Lazy
import qualified Control.Monad.RWS.Strict    as Strict
import           Control.Monad.Reader        (ReaderT)
import qualified Control.Monad.State.Lazy    as Lazy
import qualified Control.Monad.State.Strict  as Strict
import           Control.Monad.Trans         (MonadTrans (lift))
import qualified Control.Monad.Writer.Lazy   as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)

class MonadStore r m where
  newStore :: a -> m (r a)
  readStore :: r a -> m a
  writeStore :: r a -> a -> m ()

instance MonadStore IORef IO where
  newStore = newIORef
  readStore = readIORef
  writeStore r = writeIORef r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (ExceptT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (Lazy.StateT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (Strict.StateT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (ReaderT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, Monoid e, MonadStore r m) => MonadStore r (Lazy.WriterT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, Monoid e, MonadStore r m) => MonadStore r (Strict.WriterT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (IdentityT m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, MonadStore r m) => MonadStore r (ContT e m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, Monoid w, MonadStore r m) => MonadStore r (Lazy.RWST e w s m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}

instance (Monad m, Monoid w, MonadStore r m) => MonadStore r (Strict.RWST e w s m) where
  newStore = lift . newStore
  readStore = lift . readStore
  writeStore r = lift . writeStore r
  {-# INLINE newStore #-}
  {-# INLINE readStore #-}
  {-# INLINE writeStore #-}
