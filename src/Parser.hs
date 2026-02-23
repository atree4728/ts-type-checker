{-# LANGUAGE OverloadedStrings #-}

module Parser (parse, Parser, ParserError) where

import AST (Param (..), Term (..), Type (..))
import Control.Monad.Combinators.Expr
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

pIdent :: Parser Text
pIdent = lexeme $ do
  ident <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if ident `elem` keywords
    then fail $ "Keyword `" ++ T.unpack ident ++ "` cannot be used as an identifier."
    else return ident
  where
    keywords = ["const", "number", "boolean", "true", "false"]

{-
 - Block   ::= Term (`;` Block)?
             | `const` Ident `=` Term `;` Block
 - Term    ::= AddExpr (`?` Term `:` Term)?
 - AddExpr ::= AppExpr (`+` AppExpr)*
 - AppExpr ::= Primary `(` (Term `,`)* `)`
 - Primary ::= `,` (Param `,`)* `)` `=>` Term
             | `true`
             | `false`
             | [0-9]+
             | Ident
             | `(` Term `)`
-}

pBlock :: Parser Term
pBlock =
  pConst
    <|> ( do
            term <- pTerm
            rest <- optional (symbol ";" *> skipMany (symbol ";") *> optional pBlock)
            return $ case rest of
              Nothing -> term
              Just Nothing -> term
              Just (Just r) -> TmSeq term r
        )

pConst :: Parser Term
pConst = do
  _ <- symbol "const"
  name <- pIdent
  _ <- symbol "="
  body <- pTerm
  _ <- symbol ";"
  _ <- skipMany (symbol ";")
  rest <- optional pBlock
  return $ TmConst name body (fromMaybe (TmVar name) rest)

pTerm :: Parser Term
pTerm = do
  t1 <- pAddExpr
  ( do
      _ <- symbol "?"
      t2 <- pTerm
      _ <- symbol ":"
      TmIf t1 t2 <$> pTerm
    )
    <|> return t1

pParam :: Parser Param
pParam = do
  ident <- pIdent
  _ <- symbol ":"
  Param ident <$> pType

pArrowType :: Parser Type
pArrowType = try $ do
  ps <- parens (pParam `sepBy` symbol ",")
  _ <- symbol "=>"
  TyFunc ps <$> pType

pType :: Parser Type
pType =
  choice
    [ try pArrowType,
      TyBoolean <$ symbol "boolean",
      TyNumber <$ symbol "number",
      parens pType
    ]

pArrow :: Parser Term
pArrow = try $ do
  ps <- parens (pParam `sepBy` symbol ",")
  _ <- symbol "=>"
  TmArrow ps <$> pTerm

pAppExpr :: Parser Term
pAppExpr = do
  func <- pPrimary
  argss <- many $ parens (pTerm `sepBy` symbol ",")
  return $ foldl TmApp func argss

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
      TmTrue <$ symbol "true",
      TmFalse <$ symbol "false",
      TmNumber <$> lexeme L.decimal,
      TmVar <$> pIdent,
      parens pTerm
    ]

parse :: Text -> Either (ParseErrorBundle Text Void) Term
parse = MP.parse (sc *> pBlock <* eof) ""
