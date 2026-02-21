module TypeChecker where

import AST
import Control.Monad (when)

data TypeError
    = Unexpected Type Type -- actual, expected
    | Mismatched Type Type -- left, right
    deriving (Show, Eq)

typecheck :: Term -> Either TypeError Type
typecheck TmTrue = Right TyBoolean
typecheck TmFalse = Right TyBoolean
typecheck TmNumber{} = Right TyNumber
typecheck TmIf{cond, thn, els} = do
    tCond <- typecheck cond
    tThn <- typecheck thn
    tEls <- typecheck els
    when (tThn /= tEls) $ Left $ Mismatched tThn tEls
    when (tCond /= TyBoolean) $ Left $ Unexpected tCond TyBoolean
    pure tThn
typecheck TmAdd{left, right} = do
    tLeft <- typecheck left
    tRight <- typecheck right
    when (tLeft /= TyNumber) $ Left $ Unexpected tLeft TyNumber
    when (tRight /= TyNumber) $ Left $ Unexpected tRight TyNumber
    pure TyNumber
