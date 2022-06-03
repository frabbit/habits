{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.EmailConfirmationNew where

import Habits.Prelude

import Habits.Domain.Email (Email)
import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce)
import Habits.Domain.AccountId (AccountId)

data EmailConfirmationNew = EmailConfirmationNew
  { email :: Email,
    emailConfirmationNonce:: EmailConfirmationNonce,
    accountId :: AccountId
  }
  deriving (Show, Eq, Ord)

instance Arbitrary EmailConfirmationNew where
  arbitrary = do
    (email, emailConfirmationNonce, accountId) <- arbitrary
    pure $ EmailConfirmationNew {..}
