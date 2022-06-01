module Habits.UseCases.Refresh.Class where

import Habits.Prelude
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Refresh as R

import Habits.Utils (applyFirstM)

class RefreshM m where
  refresh :: R.RefreshExec m

instance (MonadReader env m, Has (R.Refresh m) env) => RefreshM m where
  refresh = applyFirstM R.refresh
