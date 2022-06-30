{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.CreateHabit.Class where

import Habits.Prelude
import Habits.Utils (applyFirstM)
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.CreateHabit as R

class CreateHabitM m where
  createHabit :: R.CreateHabitExec m

instance (MonadReader env m, Has (R.CreateHabit m) env) => CreateHabitM m where
  createHabit = applyFirstM R.createHabit
