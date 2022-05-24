module Habits.UseCases.Refresh.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Refresh as R

class Refresh m where
  refresh :: R.RefreshExec m

instance (MonadReader env m, Has (R.Refresh m) env) => Refresh m where
  refresh = R.refresh
