module Habits.UseCases.CreateHabit.CreateHabitResponse where

import Habits.Prelude
import Habits.Domain.HabitId (HabitId)


data CreateHabitResponse = CreateHabitResponse {
  habitId :: HabitId
} deriving (Show, Eq, Ord)
