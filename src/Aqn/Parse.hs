module Aqn.Parse where

import           Aqn.Common
import           Aqn.Presyntax
import           Control.Applicative        (Alternative (many, (<|>)), optional)
import           Data.Char                  (isAlphaNum)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import           Text.Megaparsec            (MonadParsec (eof, notFollowedBy, takeWhile1P, try), Parsec, between)
import           Text.Megaparsec.Char       (space1, string)
import           Text.Megaparsec.Char.Lexer (skipBlockComment, skipLineComment)
import qualified Text.Megaparsec.Char.Lexer as PL

-- Extra combinators
-- Original author Andras Kovacs (https://github.com/AndrasKovacs),
-- Original file https://github.com/AndrasKovacs/smalltt/blob/master/src/Parser.hs under BSD3 License.
-- | Parse a starting value, then zero or more elements.
--   Combine the results with a function in a left-associated way.
chainl :: (Alternative m, Monad m) => (b -> a -> b) -> m a -> m b -> m b
chainl f el start = start >>= go where
  go b = do {a <- el; go (f b a)} <|> pure b
{-# INLINE chainl #-}

-- | Parse one or more elements, then parse an ending value.
--   Combine result with a function in a right-associated way.
chainr1 :: Alternative m => (a -> b -> b) -> m a -> m b -> m b
chainr1 f el end = f <$> el <*> go where
  go = (f <$> el <*> go)
       <|> end
{-# INLINE chainr1 #-}
-- End

type Parser a = Parsec () Text a

spaces :: Parser ()
spaces = PL.space space1 (skipLineComment "--") (skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = PL.lexeme spaces

entire :: Parser a -> Parser a
entire p = spaces *> p <* eof

keyword :: Text -> Parser Text
keyword s = lexeme (string s <* notFollowedBy ident)

ident :: Parser Text
ident = lexeme (takeWhile1P (Just "identifier") isAlphaNum)

braces, parens :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")
parens = between (lexeme "(") (lexeme ")")

lambda, arrow, prod :: Parser Text
lambda = lexeme "\\" <|> keyword "λ"
arrow = lexeme "->" <|> lexeme "→"
prod = keyword "Π" <|> keyword "∏"

-- ( ), .., (->), (:)

pAtom :: Parser Preexpr
pAtom = pU <|> pName <|> pHole <|> parens pExpr

pExpr :: Parser Preexpr
pExpr = pLet <|> pLam <|> pPi <|> try pTy <|> try pFun <|> pApp

pLet :: Parser Preexpr
pLet = keyword "let" *> chainr1 (\(n, t, x) -> XLet' n t x) (try defs) pExpr where
  defs = (, , ) <$> ident <*> ((lexeme ":" *> pExpr) <|> pure XHole') <* lexeme "=" <*> pExpr <* lexeme ";"

pLam :: Parser Preexpr
pLam = lambda *> chainr1 (uncurry XLam') pLamHead (lexeme "." *> pExpr)

pPi :: Parser Preexpr
pPi = optional prod *> try (chainr1 (\(l, n, t) -> XPi' l n t) pPiHead (arrow *> pExpr))

pTy :: Parser Preexpr
pTy = XTy' <$> pFun <* lexeme ":" <*> pExpr

pFun :: Parser Preexpr
pFun = chainr1 (XPi' Explicit "_") (try (pApp <* arrow)) pExpr

pApp :: Parser Preexpr
pApp = chainl (\f (l, x) -> XApp' l f x) pArg pAtom

pU :: Parser Preexpr
pU = XU' <$ keyword "Type"

pName :: Parser Preexpr
pName = XUnres' <$> ident

pHole :: Parser Preexpr
pHole = XHole' <$ keyword "_"

pArg :: Parser (NamedLicit, Preexpr)
pArg =
  (ExplicitA, ) <$> pAtom
  <|> try (braces ((, ) <$> (ImplicitN <$> ident) <* lexeme "=" <*> pExpr))
  <|> braces ((ImplicitA, ) <$> pExpr)

pPiHead :: Parser (Licit, Name, Preexpr)
pPiHead =
  parens ((Explicit, , ) <$> ident <* lexeme ":" <*> pExpr)
  <|> try (braces ((Implicit, , ) <$> ident <* lexeme ":" <*> pExpr))
  <|> braces ((Implicit, , ) <$> ident <*> pure XHole')

pLamHead :: Parser (Licit, Name)
pLamHead =
  (Implicit, ) <$> braces ident
  <|> (Explicit, ) <$> ident

pProg :: Parser Preprogram
pProg = entire (many pDecl)

pDecl :: Parser Predecl
pDecl = pFuns <|> pModule

pFuns :: Parser Predecl
pFuns = keyword "fun" *> (try pFunHead <|> pFunBody)

pFunHead :: Parser Predecl
pFunHead = DFunHead' <$> (Left <$> ident) <*> (Seq.fromList <$> many pPiHead) <* lexeme ":" <*> pExpr <* lexeme ";"

pFunBody :: Parser Predecl
pFunBody = DFunBody' <$> (Left <$> ident) <*> (Seq.fromList <$> many pLamHead) <* lexeme "=" <*> pExpr <* lexeme ";"

pModule :: Parser Predecl
pModule = DModule' <$ keyword "mod" <*> ident <*> braces (many pDecl)
