{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Top where

import           Aqn.Common
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Control.Lens               (at, ix, (?~), (^?!))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Fresh.Class  (MonadFresh (fresh))
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Store.Class  (MonadStore (readStore, writeStore))
import           Data.Function              ((&))
import           Data.IORef                 (IORef)
import           Data.IntMap.Strict         (IntMap)
import           Data.Reflection            (Given (given), give)
import           Data.Sequence              (Seq)
import qualified Data.Text                  as T
import           Data.Tsil                  (List)

data TopState
  = FoundClaim
  | FoundDefinition
  | Claimed
  | Defined
  deriving (Show, Eq)

newtype MetaCore = MetaCore
  { _metaBody    :: Maybe (Pr Term Val)
  }
makeLenses ''MetaCore

newtype Meta = Meta
  { _metaCore :: MetaCore
  }
makeLenses ''Meta

data FunCore = FunCore
  { _funParams  :: Seq (Par Term)
  , _funRetType :: Term
  , _funTele    :: Tele
  , _funTy      :: Val
  , _funBody    :: Maybe (Pr Term (List Val -> Val))
  }
makeLenses ''FunCore

data Fun = Fun
  { _funName :: Name
  , _funCore :: Maybe FunCore
  }
makeLenses ''Fun

newtype LocalInfo = LocalInfo
  { _localName :: Name
  }
makeLenses ''LocalInfo

{- REFERENCES -}

data Global = Global
  { _metas  :: IntMap Meta
  , _funs   :: IntMap Fun
  , _locals :: IntMap LocalInfo
  }
makeLenses ''Global

type TCM = ReaderT (IORef Global) (StateT Int IO)

type M m = (MonadReader (IORef Global) m, MonadStore IORef m, MonadFresh m)
type Retrieve = Given Global

readGlobal :: M m => m Global
readGlobal = ask @(IORef Global) >>= readStore

writeGlobal :: M m => Global -> m ()
writeGlobal x = do
  ref <- ask @(IORef Global)
  writeStore ref x

purely :: M m => (Retrieve => a) -> m a
purely f = do
  global <- readGlobal
  pure $ give global f

getMeta :: (Retrieve, Reading 'Metas) => MetaVar -> Meta
getMeta (MetaVar r) = given ^?! (metas . ix r)
{-# INLINE getMeta #-}

readMeta :: (M m, Reading 'Metas) => MetaVar -> m Meta
readMeta r = purely (getMeta r)
-- {-# INLINE readMeta #-}

-- Do not apply eval/quote etc directly on this via fmap
-- because that will use the OLD global environmet before the update
writeMeta :: (M m, Writing 'Metas) => MetaVar -> Meta -> m ()
writeMeta (MetaVar r) x = do
  global <- readGlobal
  writeGlobal $ global & (metas . at r) ?~ x
-- {-# INLINE writeMeta #-}

getFun :: (Retrieve, Reading 'Funs) => FunVar -> Fun
getFun (FunVar r) = given ^?! (funs . ix r)
{-# INLINE getFun #-}

readFun :: (M m, Reading 'Funs) => FunVar -> m Fun
readFun r = purely (getFun r)
-- {-# INLINE readFun #-}

writeFun :: (M m, Writing 'Funs) => FunVar -> Fun -> m ()
writeFun (FunVar r) x = do
  global <- readGlobal
  writeGlobal $ global & (funs . at r) ?~ x
-- {-# INLINE writeFun #-}

updateFun :: (M m, Writing 'Funs) => FunVar -> (Fun -> Fun) -> m ()
updateFun r f = do
  fn <- readFun r
  writeFun r (f fn)
-- {-# INLINE updateFun #-}

getLocal :: (Retrieve, Reading 'Locals) => Local -> LocalInfo
getLocal (Local r) = given ^?! (locals . ix r)
{-# INLINE getLocal #-}

readLocal :: (M m, Reading 'Locals) => Local -> m LocalInfo
readLocal r = purely (getLocal r)
-- {-# INLINE readLocal #-}

freshLocal :: (M m, Writing 'Locals, MonadFresh m) => Name -> m Local
freshLocal n = do
  i <- fresh
  global <- readGlobal
  writeGlobal $ global & (locals . at i) ?~ LocalInfo n
  pure $ Local i

freshLocal' :: (M m, Writing 'Locals, MonadFresh m) => m Local
freshLocal' = do
  i <- fresh
  global <- readGlobal
  writeGlobal $ global & (locals . at i) ?~ LocalInfo (T.pack $ show i)
  pure $ Local i
