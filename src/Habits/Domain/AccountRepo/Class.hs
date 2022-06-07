module Habits.Domain.AccountRepo.Class where

import Habits.Prelude
import Habits.Domain.Account (Account)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.AccountRepo
  ( Add,
    GetByEmail,
    GetById, Update,
  )
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.Email (Email)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Data.Has (Has)
import Habits.Utils (applyFirstM, applyFirst2M)

class AccountRepoM m where
  add :: Add m
  getById :: GetById m
  update :: Update m
  getByEmail :: GetByEmail m


instance (MonadReader env m, Has (AR.AccountRepo m) env) => AccountRepoM m where
  add = applyFirstM AR.add
  getById = applyFirstM AR.getById
  update = applyFirst2M AR.update
  getByEmail = applyFirstM AR.getByEmail


getByEmailOrFail :: (Monad m, AccountRepoM m) => Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail e = S.do
  acc <- getByEmail e
  case acc of
    Just a -> liftE $ S.pure a
    Nothing -> failureE AccountNotFoundError
