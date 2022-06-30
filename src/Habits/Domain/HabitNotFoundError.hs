module Habits.Domain.HabitNotFoundError
  ( HabitNotFoundError (..),
  )
where

import Habits.Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data HabitNotFoundError = HabitNotFoundError
  deriving (Show, Eq, Ord, Typeable)

instance Exception HabitNotFoundError