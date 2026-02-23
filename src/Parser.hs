{-# LANGUAGE OverloadedStrings #-}

module Parser (parseTerm, Parser, ParserError) where

import AST (Param (..), Term (..), Type (..))
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
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

pTerm :: Parser Term
pTerm = do
  t1 <- pBinOp
  ( do
      _ <- symbol "?"
      t2 <- pTerm
      _ <- symbol ":"
      TmIf t1 t2 <$> pTerm
    )
    <|> return t1

pIdent :: Parser Text
pIdent = do
  ident <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if ident `elem` keywords
    then fail $ "Keyword `" ++ T.unpack ident ++ "` cannot be used as an identifier."
    else return ident
  where
    keywords = ["const", "number", "boolean", "true", "false"]

pParam :: Parser Param
pParam = do
  ident <- pIdent
  _ <- symbol ":"
  Param ident <$> pType

pFuncType :: Parser Type
pFuncType = try $ do
  ps <- parens (pParam `sepBy` symbol ",")
  _ <- symbol "=>"
  TyFunc ps <$> pType

pType :: Parser Type
pType =
  choice
    [ try pFuncType,
      TyBoolean <$ symbol "boolean",
      TyNumber <$ symbol "number",
      parens pType
    ]

pFunc :: Parser Term
pFunc = try $ do
  ps <- parens (pParam `sepBy` symbol ",")
  _ <- symbol "=>"
  TmFunc ps <$> pTerm

-- pFunc :: Parser Term
-- pFunc = _

-- pCall :: Parser Term
-- pCall = _

-- pSeq :: Parser Term
-- pSeq = _

pBinOp :: Parser Term
pBinOp = makeExprParser pAtom operatorTable

operatorTable :: [[Operator Parser Term]]
operatorTable = [[InfixL (TmAdd <$ symbol "+")]]

pAtom :: Parser Term
pAtom =
  choice
    [ pFunc,
      TmTrue <$ symbol "true",
      TmFalse <$ symbol "false",
      TmNumber <$> lexeme L.decimal,
      TmVar <$> pIdent,
      parens pTerm
    ]

parseTerm :: Text -> Either (ParseErrorBundle Text Void) Term
parseTerm = parse (sc *> pTerm <* eof) ""
