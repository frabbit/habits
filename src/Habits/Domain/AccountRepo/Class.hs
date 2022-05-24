module Habits.Domain.AccountRepo.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Habits.Domain.Account (Account)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.AccountRepo
  ( Add,
    GetByEmail,
    GetById,
  )
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.Email (Email)
import Haskus.Utils.Variant.Excepts (Excepts, failureE, liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Data.Has (Has)

class AccountRepo m where
  add :: Add m
  getById :: GetById m
  getByEmail :: GetByEmail m

instance (MonadReader env m, Has (AR.AccountRepo m) env) => AccountRepo m where
  add = AR.add
  getById = AR.getById
  getByEmail = AR.getByEmail

getByEmailOrFail :: (Monad m, AccountRepo m) => Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail e = S.do
  acc <- getByEmail e
  case acc of
    Just a -> liftE $ S.pure a
    Nothing -> failureE AccountNotFoundError
