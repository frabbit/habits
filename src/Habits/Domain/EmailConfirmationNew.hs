{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.EmailConfirmationNew where

import Habits.Prelude

import Habits.Domain.Email (Email)

data EmailConfirmationNew = EmailConfirmationNew
  { email :: Email
  }
  deriving (Show, Eq, Ord)

instance Arbitrary EmailConfirmationNew where
  arbitrary = do
    email <- arbitrary
    pure $ EmailConfirmationNew {..}
