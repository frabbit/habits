{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.CreateHabit.CreateHabitRequest where


import Habits.Prelude
import Habits.Domain.AccountId (AccountId(..))
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

data CreateHabitRequest = CreateHabitRequest
  {
    name :: Text,
    accountId :: AccountId
  }
  deriving (Eq, Show)

instance Arbitrary CreateHabitRequest where
  arbitrary = do
    name <- genValidUtf8WithoutNullByte
    accountId <- arbitrary
    pure $ CreateHabitRequest {..}