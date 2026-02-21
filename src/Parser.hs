{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST (Term (..))
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type ParserError = Void

type Parser = Parsec ParserError Text

{-
<term>
    = `true`
    | `false`
    | <term> `?` <term>
-}

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
    t1 <- pAddExpr
    ( do
            _ <- symbol "?"
            t2 <- pTerm
            _ <- symbol ":"
            t3 <- pTerm
            return $ TmIf t1 t2 t3
        )
        <|> return t1

pAddExpr :: Parser Term
pAddExpr = makeExprParser pAtom operatorTable

operatorTable :: [[Operator Parser Term]]
operatorTable = [[InfixL (TmAdd <$ symbol "+")]]

pAtom :: Parser Term
pAtom =
    choice
        [ parens pTerm
        , TmTrue <$ symbol "true"
        , TmFalse <$ symbol "false"
        , TmNumber <$> lexeme L.decimal
        ]

parseTerm :: Text -> Either (ParseErrorBundle Text Void) Term
parseTerm = parse (sc *> pTerm <* eof) ""
