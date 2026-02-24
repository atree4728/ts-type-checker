{-# LANGUAGE RecordWildCards #-}

module TypeChecker where

import AST (Param (..), Term (..), Type (..))
import Control.Monad (when)
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

showType :: Type -> String
showType TyBoolean = "boolean"
showType TyNumber = "number"
showType TyArrow {..} = "(" <> intercalate ", " (map showParam params) <> ") => " <> showType tRet
showType TyObject {..} = "{" <> intercalate ", " (map showProp (M.toList props)) <> "}"
  where
    showProp (k, v) = unpack k <> ": " <> showType v

showParam :: Param -> String
showParam Param {..} = unpack name <> ": " <> showType type_

instance Show TypeError where
  show (Unexpected actual expected) =
    "expected `" <> showType expected <> "` but got `" <> showType actual <> "`"
  show (Mismatched tyL tyR) =
    "type mismatch: `" <> showType tyL <> "` and `" <> showType tyR <> "`"
  show (Unbounded name) =
    "unbound variable: `" <> unpack name <> "`"
  show (NotAFunction ty) =
    "`" <> showType ty <> "` is not a function"
  show (UnexpectedApp actual expected) =
    "argument type mismatch: expected ("
      <> intercalate ", " (map showType expected)
      <> ") but got ("
      <> intercalate ", " (map showType actual)
      <> ")"
  show (NotAnObject ty) =
    "`" <> showType ty <> "` is not an object"
  show (PropNotFound k props) =
    "property `" <> unpack k <> "` not found in " <> showType (TyObject props)

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
tc TmSeq {..} = do
  _ <- tc body
  tc rest
tc TmConst {..} = do
  tBody <- tc body
  local (M.insert name tBody) $ tc rest
tc TmObjNew {..} = TyObject <$> mapM tc props
tc TmObjGet {..} = do
  tObj <- tc obj
  case tObj of
    TyObject {..} ->
      maybe (err $ PropNotFound name props) pure (M.lookup name props)
    _ -> err $ NotAnObject tObj

typecheck :: Term -> Either TypeError Type
typecheck term = runReaderT (tc term) M.empty
