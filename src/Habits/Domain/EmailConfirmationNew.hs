{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.EmailConfirmationNew where

import Habits.Prelude

import Habits.Domain.Email (Email)
import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce)

data EmailConfirmationNew = EmailConfirmationNew
  { email :: Email,
    emailConfirmationNonce:: EmailConfirmationNonce
  }
  deriving (Show, Eq, Ord)

instance Arbitrary EmailConfirmationNew where
  arbitrary = do
    (email,emailConfirmationNonce) <- arbitrary
    pure $ EmailConfirmationNew {..}
