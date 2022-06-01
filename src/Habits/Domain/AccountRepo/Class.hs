module Habits.Domain.AccountRepo.Class where

import Habits.Prelude
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
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Data.Has (Has)
import qualified Veins.Data.Has as Has

class AccountRepo m where
  add :: Add m
  getById :: GetById m
  getByEmail :: GetByEmail m

wrap :: (MonadReader r m, Has a r) => ((a -> b -> m c) -> b -> m c)
wrap f r = flip f r =<< asks Has.get

instance (MonadReader env m, Has (AR.AccountRepo m) env) => AccountRepo m where
  add = wrap AR.add
  getById = wrap AR.getById
  getByEmail = wrap AR.getByEmail

getByEmailOrFail :: (Monad m, AccountRepo m) => Email -> Excepts '[RepositoryError, AccountNotFoundError] m Account
getByEmailOrFail e = S.do
  acc <- getByEmail e
  case acc of
    Just a -> liftE $ S.pure a
    Nothing -> failureE AccountNotFoundError
