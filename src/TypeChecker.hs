module TypeChecker where

import AST (Term (..), Type (..))
import Control.Monad (when)
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Text (Text)

data TypeError
  = Unexpected Type Type -- actual, expected
  | Mismatched Type Type -- left, right
  deriving (Show, Eq)

type TypeEnv = Map.Map Text Type

type TypingM = ReaderT TypeEnv (Either TypeError)

err :: TypeError -> TypingM ()
err = lift . Left

tc :: Term -> TypingM Type
tc TmTrue = pure TyBoolean
tc TmFalse = pure TyBoolean
tc TmNumber {} = pure TyNumber
tc TmIf {cond, thn, els} = do
  tCond <- tc cond
  tThn <- tc thn
  tEls <- tc els
  when (tThn /= tEls) $ err $ Mismatched tThn tEls
  when (tCond /= TyBoolean) $ err $ Unexpected tCond TyBoolean
  pure tThn
tc TmAdd {left, right} = do
  tLeft <- tc left
  tRight <- tc right
  when (tLeft /= TyNumber) $ err $ Unexpected tLeft TyNumber
  when (tRight /= TyNumber) $ err $ Unexpected tRight TyNumber
  pure TyNumber

typecheck :: Term -> Either TypeError Type
typecheck term = runReaderT (tc term) Map.empty