module Lang where

import AST (Type)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Text (Text)
import Parser (ParserError, parse)
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
langcheck = first EParse . parse >=> first EType . typecheck

-- >>> langcheck (Data.Text.pack "() => 0")
-- Right (TyArrow {params = [], retType = TyNumber})
