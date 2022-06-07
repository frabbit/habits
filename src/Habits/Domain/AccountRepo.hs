module Habits.Domain.AccountRepo where

import Habits.Prelude
import Habits.Domain.Account (Account)
import Habits.Domain.AccountId (AccountId)
import Habits.Domain.AccountNew (AccountNew)
import Habits.Domain.AccountUpdate (AccountUpdate)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.Email (Email)
import Habits.Domain.RepositoryError (RepositoryError)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

type Add m =
  AccountNew ->
  Excepts '[RepositoryError] m AccountId

type Update m =
  AccountUpdate -> AccountId ->
  Excepts '[RepositoryError] m ()

type GetByEmail m =
  Email ->
  Excepts '[RepositoryError] m (Maybe Account)

type GetById m =
  AccountId ->
  Excepts '[RepositoryError, AccountNotFoundError] m Account

data AccountRepo m = AccountRepo
  { add :: Add m,
    update :: Update m,
    getById :: GetById m,
    getByEmail :: GetByEmail m
  }

type instance ToSymbol (AccountRepo m) = "AccountRepo"

getAccountRepo :: (MonadReader r n, Has.Has (AccountRepo m) r) => n (AccountRepo m)
getAccountRepo = asks Has.get

getByEmailOrFail :: (Monad m) => AccountRepo m -> Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail repo e = S.do
  acc <- getByEmail repo e
  maybe (failureE AccountNotFoundError) (liftE . S.pure) acc