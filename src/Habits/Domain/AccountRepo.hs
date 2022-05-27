module Habits.Domain.AccountRepo where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader.Class (asks)
import Habits.Domain.Account (Account)
import Habits.Domain.AccountId (AccountId)
import Habits.Domain.AccountNew
  ( AccountNew,
  )
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.Email (Email)
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

type Add m =
  AccountNew ->
  Excepts '[RepositoryError] m AccountId

type GetByEmail m =
  Email ->
  Excepts '[RepositoryError] m (Maybe Account)

type GetById m =
  AccountId ->
  Excepts '[RepositoryError, AccountNotFoundError] m Account

data AccountRepo m = AccountRepo
  { _add :: Add m,
    _getById :: GetById m,
    _getByEmail :: GetByEmail m
  }

type instance ToSymbol (AccountRepo m) = "AccountRepo"

type AccountRepoR env = AccountRepo (ReaderT env IO)

add :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => Add m
add r = do
  f <- asks (Has.get @(AccountRepo m))
  f._add r

getById :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => GetById m
getById r = do
  f <- asks (Has.get @(AccountRepo m))
  f._getById r

getByEmail :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => GetByEmail m
getByEmail r = do
  f <- asks (Has.get @(AccountRepo m))
  f._getByEmail r
