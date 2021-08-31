module Aqn.Ref where

import           Aqn.Common

-- | A reference to a local variable.
newtype Local = Local { runLocal :: Int }
  deriving (Show, Ord, Eq)

-- | A reference to a metavariable.
newtype MetaVar = MetaVar { runMetaVar :: Int }
  deriving (Show, Ord, Eq)

-- | A reference to a defined function.
newtype FunVar = FunVar { runFunVar :: Int }
  deriving (Show, Ord, Eq)

data Ref
  = RLocal Local
  | RMeta MetaVar
  | RFun FunVar
  deriving (Show, Eq)

newtype DefVar
  = DVFun FunVar
  deriving (Show, Eq)

-- | An argument in core syntax.
type Arg a = Pr Licit a

-- | An untyped binding in core syntax.
type Bind = Tp Licit Name Local

-- | A typed binding in core syntax.
type Par a = Pr Bind a
