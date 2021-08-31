module Control.Monad.Fresh.Class where

import           Control.Monad.Cont          (ContT)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Identity      (IdentityT)
import qualified Control.Monad.RWS.Lazy      as Lazy
import qualified Control.Monad.RWS.Strict    as Strict
import           Control.Monad.Reader        (ReaderT)
import           Control.Monad.State.Class   (MonadState (get, put))
import qualified Control.Monad.State.Lazy    as Lazy
import qualified Control.Monad.State.Strict  as Strict
import           Control.Monad.Trans         (MonadTrans (lift))
import qualified Control.Monad.Writer.Lazy   as Lazy
import qualified Control.Monad.Writer.Strict as Strict

class MonadFresh m where
  fresh :: m Int

instance Monad m => MonadFresh (Lazy.StateT Int m) where
  fresh = do
    x <- get @Int
    put @Int (x + 1)
    pure x

instance Monad m => MonadFresh (Strict.StateT Int m) where
  fresh = do
    x <- get @Int
    put @Int (x + 1)
    pure x

instance (Monad m, Monoid w) => MonadFresh (Lazy.RWST r w Int m) where
  fresh = do
    x <- get @Int
    put @Int (x + 1)
    pure x

instance (Monad m, Monoid w) => MonadFresh (Strict.RWST r w Int m) where
  fresh = do
    x <- get @Int
    put @Int (x + 1)
    pure x

instance (Monad m, MonadFresh m) => MonadFresh (ExceptT e m) where
  fresh = lift fresh
  {-# INLINE fresh #-}

instance (Monad m, MonadFresh m) => MonadFresh (ReaderT e m) where
  fresh = lift fresh
  {-# INLINE fresh #-}

instance (Monad m, MonadFresh m) => MonadFresh (IdentityT m) where
  fresh = lift fresh
  {-# INLINE fresh #-}

instance (Monad m, MonadFresh m) => MonadFresh (ContT e m) where
  fresh = lift fresh
  {-# INLINE fresh #-}

instance (Monad m, Monoid e, MonadFresh m) => MonadFresh (Lazy.WriterT e m) where
  fresh = lift fresh
  {-# INLINE fresh #-}

instance (Monad m, Monoid e, MonadFresh m) => MonadFresh (Strict.WriterT e m) where
  fresh = lift fresh
  {-# INLINE fresh #-}
