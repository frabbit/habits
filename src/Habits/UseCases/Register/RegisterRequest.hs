{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Register.RegisterRequest where

import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Test.QuickCheck (Arbitrary (arbitrary))

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
