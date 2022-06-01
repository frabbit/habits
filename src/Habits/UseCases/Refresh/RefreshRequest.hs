module Habits.UseCases.Refresh.RefreshRequest where

import Prelude
import Habits.Domain.RefreshToken (RefreshToken)

data RefreshRequest = RefreshRequest RefreshToken deriving (Eq, Show)

