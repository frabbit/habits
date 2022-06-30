{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.CreateHabit.CreateHabitResponse where

import Habits.Prelude
import Habits.Domain.HabitId (HabitId)


data CreateHabitResponse = CreateHabitResponse {
  habitId :: HabitId
} deriving (Show, Eq, Ord)

instance Arbitrary CreateHabitResponse where
  arbitrary = do
    habitId <- arbitrary
    pure $ CreateHabitResponse { .. }
