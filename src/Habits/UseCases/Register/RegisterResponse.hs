{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Register.RegisterResponse where

import Habits.Prelude

import Habits.Domain.AccountId (AccountId)

newtype RegisterResponse = RegisterResponse
  { accountId :: AccountId
  }
  deriving (Show, Eq, Ord)

instance Arbitrary RegisterResponse where
  arbitrary = do
    accountId <- arbitrary
    pure $ RegisterResponse {..}