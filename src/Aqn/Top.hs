{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Aqn.Top where

import           Aqn.Common
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Value
import           Control.Lens              (at, ix, (?~), (^?!))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Freer       (Eff, Members)
import           Control.Monad.Freer.Fresh (Fresh, fresh)
import           Data.Function             ((&))
import           Data.IORef                (IORef)
import           Data.IntMap.Strict        (IntMap)
import           Data.Reflection           (Given (given), give)
import           Data.Sequence             (Seq)
import qualified Data.Text                 as T
import           Data.Tsil                 (List)

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

type Write m = (Given (IORef Global), Members '[Store', Fresh] m)
type Retrieve = Given Global

readGlobal :: Write m => Eff m Global
readGlobal = readStore (given :: IORef Global)
{-# INLINE readGlobal #-}

writeGlobal :: Write m => Global -> Eff m ()
writeGlobal = writeStore (given :: IORef Global)
{-# INLINE writeGlobal #-}

lift :: Write m => (Retrieve => a) -> Eff m a
lift f = do
  global <- readGlobal
  pure $ give global f

getMeta :: (Retrieve, Reading 'Metas) => MetaVar -> Meta
getMeta (MetaVar r) = given ^?! (metas . ix r)
{-# INLINE getMeta #-}

readMeta :: (Write m, Reading 'Metas) => MetaVar -> Eff m Meta
readMeta r = lift (getMeta r)
{-# INLINE readMeta #-}

-- Do not apply eval/quote etc directly on this via fmap
-- because that will use the OLD global environmet before the update
writeMeta :: (Write m, Writing 'Metas) => MetaVar -> Meta -> Eff m ()
writeMeta (MetaVar r) x = do
  global <- readGlobal
  writeGlobal $ global & (metas . at r) ?~ x

getFun :: (Retrieve, Reading 'Funs) => FunVar -> Fun
getFun (FunVar r) = given ^?! (funs . ix r)
{-# INLINE getFun #-}

readFun :: (Write m, Reading 'Funs) => FunVar -> Eff m Fun
readFun r = lift (getFun r)
{-# INLINE readFun #-}

writeFun :: (Write m, Writing 'Funs) => FunVar -> Fun -> Eff m ()
writeFun (FunVar r) x = do
  global <- readGlobal
  writeGlobal $ global & (funs . at r) ?~ x

updateFun :: (Write m, Writing 'Funs) => FunVar -> (Fun -> Fun) -> Eff m ()
updateFun r f = do
  fn <- readFun r
  writeFun r (f fn)

getLocal :: (Retrieve, Reading 'Locals) => Local -> LocalInfo
getLocal (Local r) = given ^?! (locals . ix r)
{-# INLINE getLocal #-}

readLocal :: (Write m, Reading 'Locals) => Local -> Eff m LocalInfo
readLocal r = lift (getLocal r)
{-# INLINE readLocal #-}

freshLocal :: (Write m, Writing 'Locals) => Name -> Eff m Local
freshLocal n = do
  i <- fresh
  global <- readGlobal
  writeGlobal $ global & (locals . at i) ?~ LocalInfo n
  pure $ Local i

freshLocal' :: (Write m, Writing 'Locals) => Eff m Local
freshLocal' = do
  i <- fresh
  global <- readGlobal
  writeGlobal $ global & (locals . at i) ?~ LocalInfo (T.pack $ show i)
  pure $ Local i
