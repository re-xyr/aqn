module Aqn.Ref where

import           Aqn.Common

newtype Local = Local { runLocal :: Int }
  deriving (Show, Ord, Eq)
newtype MetaVar = MetaVar { runMetaVar :: Int }
  deriving (Show, Ord, Eq)
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

type Arg a = Pr Licit a
type Bind = Tp Licit Name Local
type Par a = Pr Bind a
