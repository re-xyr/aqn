module Aqn.Syntax where

import           Aqn.Common
import           Aqn.Presyntax
import           Aqn.Ref
import           Data.Sequence (Seq)

data Term
  = TLam Licit Name Local Term
  | TPi Licit Name Local Term Term
  | TU
  | TApp Licit Term Term
  | TLoc Local
  | TMeta MetaVar
  | TFun FunVar (Seq (Licit, Term))
  | TLet Local Term Term
  deriving (Show)

tApplyMany :: Foldable f => Term -> f (Licit, Term) -> Term
tApplyMany = foldl (\f (l, x) -> TApp l f x)

wrapLambda :: Foldable f => f (Licit, Local) -> Term -> Term
wrapLambda params tm = foldr (\(l, r) t -> TLam l "_" r t) tm params

data CheckError
  = CantUnify UnifyError Expr Term Term
  -- ^ Inference failed; term error
  | FunctionMismatch Expr NamedLicit Term
  -- ^ Non-function applied; term error
  | ElaborationBlocked Expr DefVar
  -- ^ Elaboration blocked by a previous failed definition head; term error
  | IncorrectParamList Decl (Seq (Licit, Name, Local, Term))
  -- ^ body param list does not match that of head; def error
  | NotSuccessfullyClaimed Decl DefVar
  -- ^ Defining without claiming; def error
  deriving (Show)

data UnifyError
  = CantUnifyApprox
  | CantUnifyLicit
  | DontKnowHowToUnify
  | NoPatternCondition
  | CantOccursCheck
  | DifferentSpineLength
  deriving (Show, Eq)
