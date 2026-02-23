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
  | TmFunc {params :: [Param], body :: Term}
  --   | TmSeq {body :: Term, rest :: Term}
  --   | TmConst {name :: Text, init :: Term, rest :: Term}
  deriving (Show, Eq)

data Param
  = Param {name :: Text, type_ :: Type}
  deriving (Show)

instance Eq Param where
  (==) = (==) `on` type_

data Type
  = TyBoolean
  | TyNumber
  | TyFunc {params :: [Param], retType :: Type}
  deriving (Show, Eq)
