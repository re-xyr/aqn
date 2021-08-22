module Aqn.Prettyprint where

import           Aqn.Common
import           Aqn.Presyntax
import           Aqn.Ref
import           Aqn.Syntax
import           Aqn.Top
import           Aqn.Value
import           Control.Lens  ((^.))
import           Data.Foldable (toList)
import           Prettyprinter

type PpM = (Retrieve, Reading 'Locals, Reading 'Funs)

prettyLocal :: (Retrieve, Reading 'Locals) => Local -> Doc ann
prettyLocal lo = pretty $ getLocal lo ^. localName

prettyFunVar :: (Retrieve, Reading 'Funs) => FunVar -> Doc ann
prettyFunVar fv = pretty $ getFun fv ^. funName

prettyDefVar :: (Retrieve, Reading 'Funs) => DefVar -> Doc ann
prettyDefVar dv = case dv of
  DVFun fv -> prettyFunVar fv

prettyLicit :: Licit -> Doc ann -> Doc ann
prettyLicit l = case l of
  Implicit -> braces
  Explicit -> id

prettyLicit' :: Licit -> Doc ann -> Doc ann
prettyLicit' l = case l of
  Implicit -> braces
  Explicit -> parens

prettyExpr :: PpM => Expr -> Doc ann
prettyExpr ex = case ex of
  XLam li s _ ex' ->
    "λ" <> prettyLicit li (pretty s) <> "." <+> prettyExpr ex'
  XPi li s _ ex' ex2 ->
    "Π" <> prettyLicit' li (pretty s <+> ":" <+> prettyExpr ex') <+> "->" <+> prettyExpr ex2
  XLet r ty x y ->
    "let" <+> prettyLocal r <+> ":" <+> prettyExpr ty <+> "=" <+> prettyExpr x <+> "in" <+> prettyExpr y
  XU              -> "U"
  XApp nl ex' ex2 -> enwrapLhsExpr ex' <+> enwrapRhsExpr nl ex2
  XLoc lo         -> prettyLocal lo
  XFun fv         -> prettyFunVar fv
  XTy ex' ex2     -> enwrapLhsExpr ex' <+> ":" <+> prettyExpr ex2
  XHole           -> "?"

enwrapLhsExpr :: PpM => Expr -> Doc ann
enwrapLhsExpr ex = case ex of
  XLam {} -> parens $ prettyExpr ex
  XPi {}  -> parens $ prettyExpr ex
  XTy {}  -> parens $ prettyExpr ex
  _       -> prettyExpr ex

enwrapRhsExpr :: PpM => NamedLicit -> Expr -> Doc ann
enwrapRhsExpr li ex = case li of
  ImplicitA -> braces $ prettyExpr ex
  ImplicitN n -> braces $ pretty n <+> "=" <+> prettyExpr ex
  ExplicitA -> case ex of
    XLam {} -> parens $ prettyExpr ex
    XPi {}  -> parens $ prettyExpr ex
    XTy {}  -> parens $ prettyExpr ex
    XApp {} -> parens $ prettyExpr ex
    _       -> prettyExpr ex

prettyTerm :: PpM => Term -> Doc ann
prettyTerm tm = case tm of
  TLam li _ lo te ->
    "λ" <> prettyLicit li (prettyLocal lo) <> "." <+> prettyTerm te
  TPi li _ lo te te' ->
    "Π" <> prettyLicit' li (prettyLocal lo <+> ":" <+> prettyTerm te) <+> "->" <+> prettyTerm te'
  TLet r x y ->
    "let" <+> prettyLocal r <+> "=" <+> prettyTerm x <+> "in" <+> prettyTerm y
  TU -> "U"
  TApp li te te' -> enwrapLhsTerm te <+> enwrapRhsTerm li te'
  TLoc lo -> prettyLocal lo
  TMeta (MetaVar mv) -> "?" <> pretty mv
  TFun fv args ->
    prettyFunVar fv <> brackets (hsep (punctuate "," (toList $ uncurry prettyLicit . fmap prettyTerm <$> args)))

enwrapLhsTerm :: PpM => Term -> Doc ann
enwrapLhsTerm tm = case tm of
  TLam {} -> parens $ prettyTerm tm
  TPi {}  -> parens $ prettyTerm tm
  _       -> prettyTerm tm

enwrapRhsTerm :: PpM => Licit -> Term -> Doc ann
enwrapRhsTerm li tm = case li of
  Implicit -> braces $ prettyTerm tm
  Explicit -> case tm of
    TLam {} -> parens $ prettyTerm tm
    TPi {}  -> parens $ prettyTerm tm
    TApp {} -> parens $ prettyTerm tm
    _       -> prettyTerm tm

prettyVal :: Val -> Doc ann
prettyVal _ = angles "semantic value"

prettyNamedLicit :: NamedLicit -> Doc ann
prettyNamedLicit nl = case nl of
  ImplicitA   -> "implicit"
  ImplicitN s -> "implicit named" <+> parens ("name:" <+> dquotes (pretty s))
  ExplicitA   -> "explicit"

prettyDecl :: PpM => Decl -> Doc ann
prettyDecl d = case d of
  DFunHead fv pars ret -> do
    "fun" <+> prettyFunVar fv <+> prettyTele prettyExpr pars <+> ":" <+> prettyExpr ret
  DFunBody fv pars body -> do
    "fun" <+> prettyFunVar fv <+> prettyPars pars <+> "=" <+> prettyExpr body

prettyTele :: (PpM, Functor f, Foldable f) => (PpM => a -> Doc ann) -> f (Licit, Name, Local, a) -> Doc ann
prettyTele pty = hsep . toList . fmap (\(l, _, r, x) -> prettyLicit' l (prettyLocal r <+> ":" <+> pty x))

prettyPars :: (PpM, Functor f, Foldable f) => f (Licit, Name, Local) -> Doc ann
prettyPars = hsep . toList . fmap (\(l, _, r) -> prettyLicit l (prettyLocal r))

prettyUnifyError :: UnifyError -> Doc ann
prettyUnifyError e = case e of
  CantUnifyApprox      -> "approximate unification failed"
  CantUnifyLicit       -> "explicitnesses do not match"
  DontKnowHowToUnify   -> "the types are different"
  NoPatternCondition   -> "pattern condition violated in metavariable resolution"
  CantOccursCheck      -> "occurs check failed in metavariable resolution"
  DifferentSpineLength -> "spine lengths do not match"

prettyCheckError :: PpM => CheckError -> Doc ann
prettyCheckError e = case e of
  CantUnify reason ex te te' -> vsep
    [ "Unable to check"
    , indent 2 $ prettyExpr ex
    , "of type"
    , indent 2 $ prettyTerm te
    , "against type"
    , indent 2 $ prettyTerm te'
    , "because" <+> prettyUnifyError reason
    ]
  FunctionMismatch ex nl te -> vsep
    [ "Unable to apply"
    , indent 2 $ prettyExpr ex
    , "because the function has type"
    , indent 2 $ prettyTerm te
    , "that cannot take an" <+> prettyNamedLicit nl <+> "argument"
    ]
  ElaborationBlocked ex dv -> vsep
    [ "Checking of"
    , indent 2 $ prettyExpr ex
    , "is blocked by an unsuccessful definition:" <+> prettyDefVar dv
    ]
  IncorrectParamList de pars -> vsep
    [ "The parameter list of"
    , indent 2 $ prettyDecl de
    , "is different from what was declared before:"
    , indent 2 $ prettyTele prettyTerm pars
    ]
  NotSuccessfullyClaimed de _dv -> vsep
    [ "The definition"
    , indent 2 $ prettyDecl de
    , "cannot be checked because its signature failed to typecheck"
    ]
