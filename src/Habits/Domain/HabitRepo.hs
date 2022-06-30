module Habits.Domain.HabitRepo where

import Habits.Prelude
import Habits.Domain.RepositoryError (RepositoryError)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)
import Habits.Domain.AccountId (AccountId)
import Habits.Domain.HabitNew (HabitNew)
import Habits.Domain.HabitId (HabitId)
import Habits.Domain.Habit (Habit)

type Add m =
  HabitNew ->
  Excepts '[RepositoryError] m HabitId

type GetByAccountId m =
  AccountId ->
  Excepts '[RepositoryError] m [Habit]

type GetById m =
  HabitId ->
  Excepts '[RepositoryError] m (Maybe Habit)

data HabitRepo m = HabitRepo
  { add :: Add m,
    getById :: GetById m,
    getByAccountId :: GetByAccountId m
  }

type instance ToSymbol (HabitRepo m) = "HabitRepo"

getHabitRepo :: (MonadReader r n, Has.Has (HabitRepo m) r) => n (HabitRepo m)
getHabitRepo = asks Has.get
