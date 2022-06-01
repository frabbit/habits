module Habits.Domain.AccountRepo where

import Habits.Prelude
import Habits.Domain.Account (Account)
import Habits.Domain.AccountId (AccountId)
import Habits.Domain.AccountNew
  ( AccountNew,
  )
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.Email (Email)
import Habits.Domain.RepositoryError (RepositoryError)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

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

getAccountRepo :: (MonadReader r n, Has.Has (AccountRepo m) r) => n (AccountRepo m)
getAccountRepo = asks Has.get

add :: forall m. AccountRepo m -> Add m
add = (._add)

getById :: forall m. AccountRepo m -> GetById m
getById = (._getById)

getByEmail :: forall m. AccountRepo m -> GetByEmail m
getByEmail repo = repo._getByEmail

getByEmailOrFail :: (Monad m) => AccountRepo m -> Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail repo e = S.do
  acc <- getByEmail repo e
  case acc of
    Just a -> liftE $ S.pure a
    Nothing -> failureE AccountNotFoundError