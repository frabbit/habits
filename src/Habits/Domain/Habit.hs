{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.Habit where

import Habits.Prelude

import Habits.Domain.HabitId (HabitId)
import qualified Habits.Domain.HabitNew as AN
import Habits.Domain.AccountId (AccountId)

data Habit = Habit
  { habitId :: HabitId,
    name :: Text,
    accountId :: AccountId
  }
  deriving (Eq, Show, Ord)

fromHabitNew :: AN.HabitNew -> HabitId -> Habit
fromHabitNew AN.HabitNew {..} habitId = Habit {habitId, ..}

toHabitNew :: Habit -> AN.HabitNew
toHabitNew Habit {..} = AN.HabitNew {..}
