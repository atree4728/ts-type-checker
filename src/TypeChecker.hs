module TypeChecker where

import AST (Param (..), Term (..), Type (..))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Text (Text, unpack)

data TypeError
  = Unexpected Type Type -- actual, expected
  | Mismatched Type Type -- left, right
  | Unbounded Text
  | NotAFunction Type
  | UnexpectedApp [Type] [Type] -- actual, expected
  | NotAnObject Type
  | PropNotFound Text (M.Map Text Type)
  deriving (Eq)

instance Show TypeError where
  show (Unexpected actual expected) =
    "expected `" <> show expected <> "` but got `" <> show actual <> "`"
  show (Mismatched tyL tyR) =
    "type mismatch: `" <> show tyL <> "` and `" <> show tyR <> "`"
  show (Unbounded name) =
    "unbound variable: `" <> unpack name <> "`"
  show (NotAFunction ty) =
    "`" <> show ty <> "` is not a function"
  show (UnexpectedApp actual expected) =
    "argument type mismatch: expected ("
      <> intercalate ", " (map show expected)
      <> ") but got ("
      <> intercalate ", " (map show actual)
      <> ")"
  show (NotAnObject ty) =
    "`" <> show ty <> "` is not an object"
  show (PropNotFound k props) =
    "property `" <> unpack k <> "` not found in " <> show (TyObject props)

type TypeEnv = M.Map Text Type

type TypingM = ReaderT TypeEnv (Either TypeError)

lookupVar :: Text -> TypingM Type
lookupVar name =
  asks (M.lookup name)
    >>= maybe (throwError $ Unbounded name) pure

tc :: Term -> TypingM Type
tc TmTrue = pure TyBoolean
tc TmFalse = pure TyBoolean
tc TmNumber {} = pure TyNumber
tc TmIf {..} = do
  tCond <- tc cond
  tThn <- tc thn
  tEls <- tc els
  unless (tThn == tEls) $ throwError $ Mismatched tThn tEls
  unless (tCond == TyBoolean) $ throwError $ Unexpected tCond TyBoolean
  pure tThn
tc TmAdd {..} = do
  tLeft <- tc left
  tRight <- tc right
  unless (tLeft == TyNumber) $ throwError $ Unexpected tLeft TyNumber
  unless (tRight == TyNumber) $ throwError $ Unexpected tRight TyNumber
  pure TyNumber
tc TmVar {..} = lookupVar name
tc TmArrow {..} =
  TyArrow params
    <$> local
      (M.union $ M.fromList $ map ((.name) &&& (.type_)) params)
      (tc body)
tc TmApp {..} = do
  tFunc <- tc func
  tArgs <- traverse tc args
  case tFunc of
    TyArrow {..} -> do
      let tPrms = map (.type_) params
      unless (tPrms == tArgs) $ throwError $ UnexpectedApp tArgs tPrms
      pure tRet
    _ -> throwError $ NotAFunction tFunc
tc TmSeq {..} = tc body *> tc rest
tc TmConst {..} = do
  tBody <- tc body
  local (M.insert name tBody) $ tc rest
tc TmObjNew {..} = TyObject <$> traverse tc props
tc TmObjGet {..} = do
  tObj <- tc obj
  case tObj of
    TyObject {..} ->
      maybe (throwError $ PropNotFound name props) pure (M.lookup name props)
    _ -> throwError $ NotAnObject tObj

typecheck :: Term -> Either TypeError Type
typecheck term = runReaderT (tc term) M.empty
