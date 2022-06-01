module Habits.Domain.RefreshTokenIssuedRepo.Class where

import Habits.Prelude

import Habits.Domain.RefreshTokenIssuedRepo
  ( Add,
    GetById, GetByAccountId, DeleteByAccountId, DeleteById,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo as R
import Veins.Data.Has (Has)
import qualified Veins.Data.Has as Has

class RefreshTokenIssuedRepo m where
  add :: Add m
  getById :: GetById m
  getByAccountId :: GetByAccountId m
  deleteByAccountId :: DeleteByAccountId m
  deleteById :: DeleteById m

wrap :: (MonadReader r m, Has a r) => ((a -> b -> m c) -> b -> m c)
wrap f r = flip f r =<< asks Has.get

instance (MonadReader env m, Has (R.RefreshTokenIssuedRepo m) env) => RefreshTokenIssuedRepo m where
  add = wrap R.add
  getById = wrap R.getById
  getByAccountId = wrap R.getByAccountId
  deleteByAccountId = wrap R.deleteByAccountId
  deleteById = wrap R.deleteById