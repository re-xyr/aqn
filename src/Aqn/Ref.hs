module Aqn.Ref where

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
