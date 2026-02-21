module AST where

data Term
    = TmTrue
    | TmFalse
    | TmIf {cond :: Term, thn :: Term, els :: Term}
    | TmNumber {n :: Int}
    | TmAdd {left :: Term, right :: Term}
    deriving (Show, Eq)

data Type
    = TyBoolean
    | TyNumber
    deriving (Show, Eq)
