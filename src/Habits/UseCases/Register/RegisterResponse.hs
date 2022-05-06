module Habits.UseCases.Register.RegisterResponse where

newtype RegisterResponse = RegisterResponse
  { success :: Bool
  }
  deriving (Show, Eq, Ord)
