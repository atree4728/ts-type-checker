module AST where

import Data.Function (on)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

data Term
  = TmTrue
  | TmFalse
  | TmIf {cond :: Term, thn :: Term, els :: Term}
  | TmNumber Int
  | TmAdd {left :: Term, right :: Term}
  | TmVar {name :: Text}
  | TmArrow {params :: [Param], body :: Term}
  | TmApp {func :: Term, args :: [Term]}
  | TmSeq {body :: Term, rest :: Term}
  | TmConst {name :: Text, body :: Term, rest :: Term}
  | TmObjNew {props :: M.Map Text Term}
  | TmObjGet {obj :: Term, name :: Text}
  deriving (Show, Eq)

data Param
  = Param {name :: Text, type_ :: Type}

instance Show Param where
  show Param {..} = T.unpack name <> ": " <> show type_

instance Eq Param where
  (==) = (==) `on` (.type_)

data Type
  = TyBoolean
  | TyNumber
  | TyArrow {params :: [Param], tRet :: Type}
  | TyObject {props :: M.Map Text Type}
  deriving (Eq)

instance Show Type where
  show TyBoolean = "boolean"
  show TyNumber = "number"
  show TyArrow {..} = "(" <> intercalate ", " (map show params) <> ") => " <> show tRet
  show TyObject {..} = "{" <> intercalate ", " (map showProp (M.toList props)) <> "}"
    where
      showProp (k, v) = T.unpack k <> ": " <> show v