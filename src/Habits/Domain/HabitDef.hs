module Habits.Domain.HabitDef where

import Habits.Prelude

data HabitInterval
  = HabitIntervalPerWeek Int
  | HabitIntervalPerMonth Int

newtype HabitDef = HabitDef {interval :: HabitInterval}
