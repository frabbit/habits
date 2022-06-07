module Habits.Utils where

import Habits.Prelude

import qualified Veins.Data.Has as Has
import Veins.Data.Has (Has)

applyFirst0M :: (MonadReader r m, Has a r) => (a -> m c) -> m c
applyFirst0M f = f =<< asks Has.get

applyFirstM :: (MonadReader r m, Has a r) => (a -> b -> m c) -> b -> m c
applyFirstM f r = flip f r =<< asks Has.get

applyFirst2M :: (MonadReader r m, Has a r) => (a -> b -> x -> m c) -> b -> x -> m c
applyFirst2M f a b = (\r -> f r a b) =<< asks Has.get