module Aqn.Syntax where

import           Aqn.Common
import           Aqn.Presyntax
import           Aqn.Ref
import           Control.Exception (Exception)
import           Data.Foldable     (Foldable (foldl', foldr'))
import           Data.Sequence     (Seq)

-- | Well-typed core syntax.
data Term
  = TLam Licit Name Local Term
  | TPi Licit Name Local Term Term
  | TU
  | TApp Licit Term Term
  | TLoc Local
  | TMeta MetaVar
  | TFun FunVar (Seq (Arg Term))
  | TLet Local Term Term
  deriving (Show)

tApplyMany :: Foldable f => Term -> f (Arg Term) -> Term
tApplyMany = foldl' (\f (l ::: x) -> TApp l f x)
{-# INLINE tApplyMany #-}

wrapLambda :: Foldable f => f (Pr Licit Local) -> Term -> Term
wrapLambda params tm = foldr' (\(l ::: r) t -> TLam l "_" r t) tm params
{-# INLINE wrapLambda #-}

-- | Errors occurred in typechecking.
data CheckError
  = CantUnify UnifyError Expr Term Term
  -- ^ Inference failed; term error.
  | FunctionMismatch Expr NamedLicit Term
  -- ^ Non-function applied; term error.
  | ElaborationBlocked Expr DefVar
  -- ^ Elaboration blocked by a previous failed definition head; term error.
  | IncorrectParamList Decl (Seq (Par Term))
  -- ^ body param list does not match that of head; def error.
  | NotSuccessfullyClaimed Decl DefVar
  -- ^ Defining without claiming; def error.
  deriving (Show, Exception)

-- | Errors occurred in unification.
data UnifyError
  = CantUnifyApprox
  | CantUnifyLicit
  | DontKnowHowToUnify
  | NoPatternCondition
  | CantOccursCheck
  | DifferentSpineLength
  deriving (Show, Eq, Exception)
