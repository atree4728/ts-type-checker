{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import AST (Term (..), Type (..))
import Control.Monad (when)

data TypeError
  = Unexpected Type Type -- actual, expected
  | Mismatched Type Type -- left, right
  deriving (Show, Eq)

typecheck :: Term -> Either TypeError Type
typecheck = \case
  TmTrue -> Right TyBoolean
  TmFalse -> Right TyBoolean
  TmNumber {} -> Right TyNumber
  TmIf {cond, thn, els} -> do
    tCond <- typecheck cond
    tThn <- typecheck thn
    tEls <- typecheck els
    when (tThn /= tEls) $ Left $ Mismatched tThn tEls
    when (tCond /= TyBoolean) $ Left $ Unexpected tCond TyBoolean
    pure tThn
  TmAdd {left, right} -> do
    tLeft <- typecheck left
    tRight <- typecheck right
    when (tLeft /= TyNumber) $ Left $ Unexpected tLeft TyNumber
    when (tRight /= TyNumber) $ Left $ Unexpected tRight TyNumber
    pure TyNumber
