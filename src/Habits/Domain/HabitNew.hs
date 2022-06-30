{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.HabitNew where

import Habits.Prelude

import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Habits.Domain.AccountId (AccountId)

data HabitNew = HabitNew
  {
    name :: Text,
    accountId :: AccountId
  }
  deriving (Show, Eq, Ord)

instance Arbitrary HabitNew where
  arbitrary = do
    accountId <- arbitrary
    name <- genValidUtf8WithoutNullByte
    pure $ HabitNew {..}
