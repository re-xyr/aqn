{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Global where

import           Aqn.Common
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Availability.Embed   (makeEffViaMonadIO)
import           Availability.Error   (makeEffViaMonadCatch, makeEffViaMonadThrow)
import           Availability.Fresh   (Fresh, fresh, makeFreshByState)
import           Availability.Impl    (Effs, M)
import           Availability.Reader  (makeEffViaMonadReader)
import           Availability.State   (Getter, Putter, get, makeStateByIORef, makeStateFromLens, put)
import           Control.Lens         (at, ix, (?~), (^?!))
import           Control.Lens.TH      (makeLenses)
import qualified Control.Monad.Reader as MTL
import           Data.Function        ((&))
import           Data.IORef           (IORef)
import           Data.IntMap.Strict   (IntMap)
import           Data.Reflection      (Given (given), give)
import           Data.Sequence        (Seq)
import qualified Data.Text            as T
import           Data.Tsil            (List)

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
  { _metas   :: IntMap Meta
  , _funs    :: IntMap Fun
  , _locals  :: IntMap LocalInfo
  , _counter :: Int
  }
makeLenses ''Global

-- | An impure operation that may or may not manipulate global environment.
-- Note that this should be seen as opaque.
type Impure = (Effs '[Getter () Global, Putter () Global, Fresh])

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

type TC = MTL.ReaderT (IORef Global) IO
type TCM = M TC

makeEffViaMonadReader [t| "impl" |] [t| IORef Global |] [t| TC |]
makeEffViaMonadIO [t| TC |]
makeStateByIORef [t| () |] [t| Global |] [t| "impl" |] [t| TC |]
makeStateFromLens [t| 1 |] [t| Int |] [t| () |] [t| Global |] [| counter |] [t| TC |]
makeFreshByState [t| 1 |] [t| TC |]
makeEffViaMonadThrow [t| TC |]
makeEffViaMonadCatch [t| TC |]

-- Lift a pure action into the impure domain.
lift :: Impure => (Pure => a) -> TCM a
lift f = do
  global <- get @()
  pure $ give (global :: Global) f

getMeta :: (Pure, Reading 'Metas) => MetaVar -> Meta
getMeta (MetaVar r) = given ^?! (metas . ix r)
{-# INLINE getMeta #-}

readMeta :: (Impure, Reading 'Metas) => MetaVar -> TCM Meta
readMeta r = lift (getMeta r)
{-# INLINE readMeta #-}

-- Do not apply eval/quote etc directly on this via fmap
-- because that will use the OLD global environmet before the update
writeMeta :: (Impure, Writing 'Metas) => MetaVar -> Meta -> TCM ()
writeMeta (MetaVar r) x = do
  global <- get @()
  put @() $ global & (metas . at r) ?~ x

getFun :: (Pure, Reading 'Funs) => FunVar -> Fun
getFun (FunVar r) = given ^?! (funs . ix r)
{-# INLINE getFun #-}

readFun :: (Impure, Reading 'Funs) => FunVar -> TCM Fun
readFun r = lift (getFun r)
{-# INLINE readFun #-}

writeFun :: (Impure, Writing 'Funs) => FunVar -> Fun -> TCM ()
writeFun (FunVar r) x = do
  global <- get @()
  put @() $ global & (funs . at r) ?~ x

updateFun :: (Impure, Writing 'Funs) => FunVar -> (Fun -> Fun) -> TCM ()
updateFun r f = do
  fn <- readFun r
  writeFun r (f fn)

getLocal :: (Pure, Reading 'Locals) => Local -> LocalInfo
getLocal (Local r) = given ^?! (locals . ix r)
{-# INLINE getLocal #-}

readLocal :: (Impure, Reading 'Locals) => Local -> TCM LocalInfo
readLocal r = lift (getLocal r)
{-# INLINE readLocal #-}

freshLocal :: (Impure, Writing 'Locals) => Name -> TCM Local
freshLocal n = do
  i <- fresh
  global <- get @()
  put @() $ global & (locals . at i) ?~ LocalInfo n
  pure $ Local i

freshLocal' :: (Impure, Writing 'Locals) => TCM Local
freshLocal' = do
  i <- fresh
  global <- get @()
  put @() $ global & (locals . at i) ?~ LocalInfo (T.pack $ show i)
  pure $ Local i
