module Habits.Domain.RefreshTokenIssuedRepo.Class where

import Habits.Prelude

import Habits.Domain.RefreshTokenIssuedRepo
  ( Add,
    GetById, GetByAccountId, DeleteByAccountId, DeleteById,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo as R
import Veins.Data.Has (Has)
import Habits.Utils (applyFirstM)

class RefreshTokenIssuedRepoM m where
  add :: Add m
  getById :: GetById m
  getByAccountId :: GetByAccountId m
  deleteByAccountId :: DeleteByAccountId m
  deleteById :: DeleteById m

instance (MonadReader env m, Has (R.RefreshTokenIssuedRepo m) env) => RefreshTokenIssuedRepoM m where
  add = applyFirstM R.add
  getById = applyFirstM R.getById
  getByAccountId = applyFirstM R.getByAccountId
  deleteByAccountId = applyFirstM R.deleteByAccountId
  deleteById = applyFirstM R.deleteById