{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Global where

import           Aqn.Common
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Control.Lens              (at, ix, (?~), (^?!))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Freer       (Eff, Members)
import           Control.Monad.Freer.Fresh (Fresh, fresh)
import           Control.Monad.Freer.State (State, get, put)
import           Data.Function             ((&))
import           Data.IntMap.Strict        (IntMap)
import           Data.Reflection           (Given (given), give)
import           Data.Sequence             (Seq)
import qualified Data.Text                 as T
import           Data.Tsil                 (List)

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

-- | The global environment.
data Global = Global
  { _metas  :: IntMap Meta
  , _funs   :: IntMap Fun
  , _locals :: IntMap LocalInfo
  }
makeLenses ''Global

-- | An impure operation that may or may not manipulate global environment.
-- Note that this should be seen as opaque.
type Impure m = (Members '[State Global, Fresh] m)

-- | A pure operation that can only read the global environment.
-- Note that this should be seen as opaque.
type Pure = Given Global

-- | Kinds of global data that are used to parameterize capabilities.
data Kind
  = Locals
  | Metas
  | Funs
  deriving (Show, Eq)

-- | Capability of writing a certain kind of global data.
class Writing (a :: Kind)

-- | Capability of reading a certain kind of global data.
class Reading (a :: Kind)
instance Writing a => Reading a

-- Lift a pure action into the impure domain.
lift :: Impure m => (Pure => a) -> Eff m a
lift f = do
  global <- get @Global
  pure $ give global f

getMeta :: (Pure, Reading 'Metas) => MetaVar -> Meta
getMeta (MetaVar r) = given ^?! (metas . ix r)
{-# INLINE getMeta #-}

readMeta :: (Impure m, Reading 'Metas) => MetaVar -> Eff m Meta
readMeta r = lift (getMeta r)
{-# INLINE readMeta #-}

-- Do not apply eval/quote etc directly on this via fmap
-- because that will use the OLD global environmet before the update
writeMeta :: (Impure m, Writing 'Metas) => MetaVar -> Meta -> Eff m ()
writeMeta (MetaVar r) x = do
  global <- get
  put $ global & (metas . at r) ?~ x

getFun :: (Pure, Reading 'Funs) => FunVar -> Fun
getFun (FunVar r) = given ^?! (funs . ix r)
{-# INLINE getFun #-}

readFun :: (Impure m, Reading 'Funs) => FunVar -> Eff m Fun
readFun r = lift (getFun r)
{-# INLINE readFun #-}

writeFun :: (Impure m, Writing 'Funs) => FunVar -> Fun -> Eff m ()
writeFun (FunVar r) x = do
  global <- get
  put $ global & (funs . at r) ?~ x

updateFun :: (Impure m, Writing 'Funs) => FunVar -> (Fun -> Fun) -> Eff m ()
updateFun r f = do
  fn <- readFun r
  writeFun r (f fn)

getLocal :: (Pure, Reading 'Locals) => Local -> LocalInfo
getLocal (Local r) = given ^?! (locals . ix r)
{-# INLINE getLocal #-}

readLocal :: (Impure m, Reading 'Locals) => Local -> Eff m LocalInfo
readLocal r = lift (getLocal r)
{-# INLINE readLocal #-}

freshLocal :: (Impure m, Writing 'Locals) => Name -> Eff m Local
freshLocal n = do
  i <- fresh
  global <- get
  put $ global & (locals . at i) ?~ LocalInfo n
  pure $ Local i

freshLocal' :: (Impure m, Writing 'Locals) => Eff m Local
freshLocal' = do
  i <- fresh
  global <- get
  put $ global & (locals . at i) ?~ LocalInfo (T.pack $ show i)
  pure $ Local i
