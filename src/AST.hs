{-# LANGUAGE DuplicateRecordFields #-}

module AST where

import Data.Function (on)
import Data.Text (Text)

data Term
  = TmTrue
  | TmFalse
  | TmIf {cond :: Term, thn :: Term, els :: Term}
  | TmNumber {n :: Int}
  | TmAdd {left :: Term, right :: Term}
  | TmVar {name :: Text}
  | TmArrow {params :: [Param], body :: Term}
  | TmApp {func :: Term, args :: [Term]}
  | TmSeq {body :: Term, rest :: Term}
  | TmConst {name :: Text, body :: Term, rest :: Term}
  | TmObjNew {props :: [PropTerm]}
  | TmObjGet {obj :: Term, name :: Text}
  deriving (Show, Eq)

data Param
  = Param {name :: Text, type_ :: Type}
  deriving (Show)

data PropTerm
  = PropTerm {name :: Text, term :: Term}
  deriving (Show, Eq)

instance Eq Param where
  (==) = (==) `on` type_

data Type
  = TyBoolean
  | TyNumber
  | TyArrow {params :: [Param], tRet :: Type}
  deriving (Show, Eq)
