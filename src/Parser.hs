module Parser (parse, Parser, ParserError) where

import AST (Param (..), Term (..), Type (..))
import Control.Monad (join)
import Control.Monad.Combinators.Expr
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type ParserError = Void

type Parser = Parsec ParserError Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

pIdent :: Parser Text
pIdent = lexeme $ do
  ident <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if ident `elem` keywords
    then fail $ "Keyword `" <> T.unpack ident <> "` cannot be used as an identifier."
    else pure ident
  where
    keywords = ["const", "number", "boolean", "true", "false", "function"]

{-
 - Block   ::= Term (`;` Block)?
             | `const` Ident `=` Term `;` Block
 - Term    ::= AddExpr (`?` Term `:` Term)?
 - AddExpr ::= AppExpr (`+` AppExpr)*
 - AppExpr ::= Primary `(` (Term `,`)* `)`
 - Primary ::= `,` (Param `,`)* `)` `=>` Term
             | Object
             | `true`
             | `false`
             | [0-9]+
             | Ident
             | `(` Term `)`
-}

pBlock :: Parser Term
pBlock =
  pConst
    <|> pFunc
    <|> do
      term <- pTerm
      rest <- optional (symbol ";" *> skipMany (symbol ";") *> optional pBlock)
      pure $ maybe term (TmSeq term) (join rest)

pFunc :: Parser Term
pFunc = do
  name <- symbol "function" *> pIdent
  params <- parens (pParam `sepBy` symbol ",")
  tRet <- symbol ":" *> pType
  body <- curly pBlock
  rest <- skipMany (symbol ";") *> optional pBlock
  pure $ TmFunc name params tRet body (fromMaybe (TmVar name) rest)

pConst :: Parser Term
pConst = do
  name <- symbol "const" *> pIdent
  body <- symbol "=" *> pTerm
  rest <- symbol ";" *> skipMany (symbol ";") *> optional pBlock
  pure $ TmConst name body (fromMaybe (TmVar name) rest)

pTerm :: Parser Term
pTerm = do
  t1 <- pAddExpr
  (TmIf t1 <$> (symbol "?" *> pTerm) <*> (symbol ":" *> pTerm))
    <|> pure t1

pParam :: Parser Param
pParam =
  Param
    <$> pIdent
    <* symbol ":"
    <*> pType

pArrowType :: Parser Type
pArrowType =
  try $
    TyArrow
      <$> parens (pParam `sepBy` symbol ",")
      <* symbol "=>"
      <*> pType

pType :: Parser Type
pType =
  choice
    [ try pArrowType,
      TyBoolean <$ symbol "boolean",
      TyNumber <$ symbol "number",
      parens pType
    ]

pArrow :: Parser Term
pArrow =
  try $
    TmArrow
      <$> parens (pParam `sepBy` symbol ",")
      <* symbol "=>"
      <*> pTerm

pObj :: Parser Term
pObj = try $ TmObjNew . M.fromList <$> curly (pPropTerm `sepBy` symbol ",")
  where
    pPropTerm = try $ (,) <$> pIdent <* symbol ":" <*> pTerm

data Postfix = App [Term] | Get Text

pAppExpr :: Parser Term
pAppExpr =
  foldl apply
    <$> pPrimary
    <*> many
      ( (App <$> parens (pTerm `sepBy` symbol ","))
          <|> (Get <$> (symbol "." *> pIdent))
      )
  where
    apply t (App args) = TmApp t args
    apply t (Get name) = TmObjGet t name

pAddExpr :: Parser Term
pAddExpr =
  makeExprParser pAppExpr operatorTable
  where
    operatorTable :: [[Operator Parser Term]]
    operatorTable = [[InfixL (TmAdd <$ symbol "+")]]

pPrimary :: Parser Term
pPrimary =
  choice
    [ pArrow,
      pObj,
      TmTrue <$ symbol "true",
      TmFalse <$ symbol "false",
      TmNumber <$> lexeme L.decimal,
      TmVar <$> pIdent,
      parens pTerm
    ]

parse :: Text -> Either (ParseErrorBundle Text Void) Term
parse = MP.parse (sc *> pBlock <* eof) ""
