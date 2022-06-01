module Habits.UseCases.Utils where

import Habits.Prelude

import qualified Veins.Data.Has as Has
import Veins.Data.Has (Has)

applyFirstM :: (MonadReader r m, Has a r) => (a -> b -> m c) -> b -> m c
applyFirstM f r = flip f r =<< asks Has.get