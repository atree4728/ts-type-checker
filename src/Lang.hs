module Lang where

import AST (Type)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Text (Text)
import Parser (ParserError, parseTerm)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import TypeChecker (TypeError, typecheck)

data LangError
  = EParse (ParseErrorBundle Text ParserError)
  | EType TypeError
  deriving (Eq)

instance Show LangError where
  show (EParse e) = errorBundlePretty e
  show (EType e) = show e

langcheck :: Text -> Either LangError Type
langcheck = first EParse . parseTerm >=> first EType . typecheck

-- >>> parseTerm (Data.Text.pack "(n: number) => 1")
-- Right (TmFunc {params = [Param {name = "n", type_ = TyNumber}], body = TmNumber {n = 1}})
