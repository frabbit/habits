module Habits.Domain.HabitDef where

import Prelude

data HabitInterval
  = HabitIntervalPerWeek Int
  | HabitIntervalPerMonth Int

newtype HabitDef = HabitDef {interval :: HabitInterval}
