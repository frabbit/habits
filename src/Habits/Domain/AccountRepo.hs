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
  { _add :: Add m,
    _update :: Update m,
    _getById :: GetById m,
    _getByEmail :: GetByEmail m
  }

type instance ToSymbol (AccountRepo m) = "AccountRepo"

getAccountRepo :: (MonadReader r n, Has.Has (AccountRepo m) r) => n (AccountRepo m)
getAccountRepo = asks Has.get


{- HLINT ignore "Redundant bracket" -}
add :: forall m. AccountRepo m -> Add m
add = (._add)

update :: forall m. AccountRepo m -> Update m
update = (._update)

{- HLINT ignore "Redundant bracket" -}
getById :: forall m. AccountRepo m -> GetById m
getById = (._getById)

getByEmail :: forall m. AccountRepo m -> GetByEmail m
getByEmail repo = repo._getByEmail

getByEmailOrFail :: (Monad m) => AccountRepo m -> Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail repo e = S.do
  acc <- getByEmail repo e
  maybe (failureE AccountNotFoundError) (liftE . S.pure) acc