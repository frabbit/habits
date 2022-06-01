module Habits.UseCases.Refresh.RefreshRequest where
import Habits.Domain.RefreshToken (RefreshToken)

data RefreshRequest = RefreshRequest RefreshToken deriving (Eq, Show)

