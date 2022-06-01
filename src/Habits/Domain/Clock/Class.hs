{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.Clock.Class where

import Habits.Prelude
import qualified Habits.Domain.Clock as Clock
import qualified Veins.Data.Has as Has

class Clock m where
  getNow :: Clock.GetNow m


instance (MonadReader env m, Has.Has (Clock.Clock m) env) => Clock m where
  getNow = Clock.getNow
