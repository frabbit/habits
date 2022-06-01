{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Register.RegisterRequest where

import Habits.Prelude

import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

data RegisterRequest = RegisterRequest
  { email :: Email,
    name :: Text,
    password :: Password
  } deriving (Show, Eq, Ord)

instance Arbitrary RegisterRequest where
  arbitrary = do
    email <- arbitrary
    name <- genValidUtf8WithoutNullByte
    password <- arbitrary
    pure $ RegisterRequest {..}
