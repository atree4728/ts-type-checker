{-# LANGUAGE RecordWildCards #-}

module TypeChecker where

import AST (Param (..), Term (..), Type (..))
import Control.Monad (when)
import Control.Monad.Reader
import Data.Map qualified as M
import Data.Text (Text)

data TypeError
  = Unexpected Type Type -- actual, expected
  | Mismatched Type Type -- left, right
  | Unbounded Text
  | NotAFunction Type
  | UnexpectedApp [Type] [Type] -- actual, expected
  deriving (Show, Eq)

type TypeEnv = M.Map Text Type

type TypingM = ReaderT TypeEnv (Either TypeError)

err :: TypeError -> TypingM a
err = lift . Left

lookupVar :: Text -> TypingM Type
lookupVar name = do
  env <- ask
  case M.lookup name env of
    Nothing -> err $ Unbounded name
    Just ty -> pure ty

tc :: Term -> TypingM Type
tc TmTrue = pure TyBoolean
tc TmFalse = pure TyBoolean
tc TmNumber {} = pure TyNumber
tc TmIf {..} = do
  tCond <- tc cond
  tThn <- tc thn
  tEls <- tc els
  when (tThn /= tEls) $ err $ Mismatched tThn tEls
  when (tCond /= TyBoolean) $ err $ Unexpected tCond TyBoolean
  pure tThn
tc TmAdd {..} = do
  tLeft <- tc left
  tRight <- tc right
  when (tLeft /= TyNumber) $ err $ Unexpected tLeft TyNumber
  when (tRight /= TyNumber) $ err $ Unexpected tRight TyNumber
  pure TyNumber
tc TmVar {..} = lookupVar name
tc TmArrow {..} = do
  TyArrow params <$> tc body
tc TmApp {..} = do
  tFunc <- tc func
  tArgs <- mapM tc args
  case tFunc of
    TyArrow {..} ->
      do
        let tPrms = type_ <$> params
        when (tPrms /= tArgs) $ err $ UnexpectedApp tArgs tPrms
        pure tRet
    _ -> err $ NotAFunction tFunc
tc TmSeq {..} = do pure TyNumber
tc TmConst {..} = do pure TyNumber

typecheck :: Term -> Either TypeError Type
typecheck term = runReaderT (tc term) M.empty