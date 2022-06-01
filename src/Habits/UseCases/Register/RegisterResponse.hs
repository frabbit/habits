{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Register.RegisterResponse where

import Prelude
import Habits.Domain.AccountId (AccountId)
import Test.QuickCheck (Arbitrary (arbitrary))

newtype RegisterResponse = RegisterResponse
  { accountId :: AccountId
  }
  deriving (Show, Eq, Ord)

instance Arbitrary RegisterResponse where
  arbitrary = do
    accountId <- arbitrary
    pure $ RegisterResponse {..}