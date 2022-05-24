{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.TimeProvider.Class where

import qualified Habits.Domain.TimeProvider as TP
import qualified Veins.Data.Has as Has
import Control.Monad.Reader (MonadReader)

class TimeProvider m where
  getNow :: TP.GetNow m


instance (MonadReader env m, Has.Has (TP.TimeProvider m) env) => TimeProvider m where
  getNow = TP.getNow
