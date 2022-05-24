module Habits.Domain.RefreshTokenIssuedRepo.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Habits.Domain.RefreshTokenIssuedRepo
  ( Add,
    GetById, GetByAccountId, DeleteByAccountId, DeleteById,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo as R
import Veins.Data.Has (Has)

class RefreshTokenIssuedRepo m where
  add :: Add m
  getById :: GetById m
  getByAccountId :: GetByAccountId m
  deleteByAccountId :: DeleteByAccountId m
  deleteById :: DeleteById m

instance (MonadReader env m, Has (R.RefreshTokenIssuedRepo m) env) => RefreshTokenIssuedRepo m where
  add = R.add
  getById = R.getById
  getByAccountId = R.getByAccountId
  deleteByAccountId = R.deleteByAccountId
  deleteById = R.deleteById