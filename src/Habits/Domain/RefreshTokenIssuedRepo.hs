module Habits.Domain.RefreshTokenIssuedRepo where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import Habits.Domain.RefreshTokenIssuedNew
  ( RefreshTokenIssuedNew,
  )
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)
import Habits.Domain.AccountId (AccountId)

type Add m =
  RefreshTokenIssuedNew ->
  Excepts '[RepositoryError] m RefreshTokenIssuedId

type GetById m =
  RefreshTokenIssuedId ->
  Excepts '[RepositoryError] m (Maybe RefreshTokenIssued)

type GetByAccountId m =
  AccountId ->
  Excepts '[RepositoryError] m [RefreshTokenIssued]

type DeleteByAccountId m =
  AccountId ->
  Excepts '[RepositoryError] m ()

type DeleteById m =
  RefreshTokenIssuedId ->
  Excepts '[RepositoryError] m ()

data RefreshTokenIssuedRepo m = RefreshTokenIssuedRepo
  { _add :: Add m,
    _getById :: GetById m,
    _getByAccountId :: GetByAccountId m,
    _deleteByAccountId :: DeleteByAccountId m,
    _deleteById :: DeleteById m
  }

type instance ToSymbol (RefreshTokenIssuedRepo m) = "RefreshTokenIssuedRepo"

add :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => Add m
add r = do
  RefreshTokenIssuedRepo {_add = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r

getById :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => GetById m
getById r = do
  RefreshTokenIssuedRepo {_getById = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r

getByAccountId :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => GetByAccountId m
getByAccountId r = do
  RefreshTokenIssuedRepo {_getByAccountId = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r

deleteByAccountId :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => DeleteByAccountId m
deleteByAccountId r = do
  RefreshTokenIssuedRepo {_deleteByAccountId = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r

deleteById :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => DeleteById m
deleteById r = do
  RefreshTokenIssuedRepo {_deleteById = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r
