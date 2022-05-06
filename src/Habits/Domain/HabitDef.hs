module Habits.Domain.HabitDef where

data HabitInterval
  = HabitIntervalPerWeek Int
  | HabitIntervalPerMonth Int

newtype HabitDef = HabitDef {interval :: HabitInterval}
