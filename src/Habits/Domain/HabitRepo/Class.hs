module Habits.Domain.HabitRepo.Class where

import Habits.Prelude

import Habits.Domain.HabitRepo
  ( Add,
    GetByAccountId,
    GetById
  )
import qualified Habits.Domain.HabitRepo as AR
import Veins.Data.Has (Has)
import Habits.Utils (applyFirstM)

class HabitRepoM m where
  add :: Add m
  getById :: GetById m
  getByAccountId :: GetByAccountId m

instance (MonadReader env m, Has (AR.HabitRepo m) env) => HabitRepoM m where
  add = applyFirstM AR.add
  getById = applyFirstM AR.getById
  getByAccountId = applyFirstM AR.getByAccountId

