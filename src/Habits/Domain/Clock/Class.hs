{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.Clock.Class where

import qualified Habits.Domain.Clock as Clock
import qualified Veins.Data.Has as Has
import Control.Monad.Reader (MonadReader)

class Clock m where
  getNow :: Clock.GetNow m


instance (MonadReader env m, Has.Has (Clock.Clock m) env) => Clock m where
  getNow = Clock.getNow
