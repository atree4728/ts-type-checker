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

instance Show LangError where
  show (EParse e) = errorBundlePretty e
  show (EType e) = show e

langCheck :: Text -> Either LangError Type
langCheck = first EParse . parseTerm >=> first EType . typecheck