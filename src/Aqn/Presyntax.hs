module Aqn.Presyntax where

import           Aqn.Common
import           Aqn.Ref
import           Data.Sequence (Seq)

-- | Surface expression before variable resolution.
data Preexpr
  = XLam' Licit Name Preexpr
  | XPi' Licit Name Preexpr Preexpr
  | XLet' Name Preexpr Preexpr Preexpr
  | XU'
  | XApp' NamedLicit Preexpr Preexpr
  | XUnres' Name
  | XTy' Preexpr Preexpr
  | XHole'
  deriving (Show)

-- | Well-scoped surface expressions subject to typechecking.
data Expr
  = XLam Licit Name Local Expr -- param licit, param name, param ref, body
  | XPi Licit Name Local Expr Expr -- dom licit, dom name, dom ref, dom ty, cod
  | XLet Local Expr Expr Expr -- ref, type, value, body
  | XU --
  | XApp NamedLicit Expr Expr -- arg licit, fn, arg
  | XLoc Local -- ref
  | XFun FunVar -- ref
  | XTy Expr Expr -- expr, ty
  | XHole --
  deriving (Show)

-- | Specification of insertion in surface application syntax.
data NamedLicit
  = ImplicitA
  | ImplicitN Name
  | ExplicitA
  deriving (Show, Eq)

unNamedLicit :: NamedLicit -> Licit
unNamedLicit x = case x of
  ImplicitA   -> Implicit
  ImplicitN _ -> Implicit
  ExplicitA   -> Explicit
{-# INLINE unNamedLicit #-}

matchNamedLicit :: NamedLicit -> Licit -> Name -> Bool
matchNamedLicit nl l n = case nl of
  ImplicitA   -> l == Implicit
  ImplicitN s -> l == Implicit && s == n
  ExplicitA   -> l == Explicit
{-# INLINE matchNamedLicit #-}

-- | A program is a list of declarations.
type Preprogram = [Predecl]

-- | Surface declarations before variable resolution.
data Predecl
  = DFunHead' (Either Name FunVar) (Seq (Licit, Name, Preexpr)) Preexpr
  | DFunBody' (Either Name FunVar) (Seq (Licit, Name)) Preexpr
  | DModule' Name [Predecl]
  deriving (Show)

-- | Well-scoped surface declarations subject to typechecking.
data Decl
  = DFunHead FunVar (Seq (Par Expr)) Expr
  | DFunBody FunVar (Seq Bind) Expr
  deriving (Show)
