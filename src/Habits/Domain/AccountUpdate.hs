{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.AccountUpdate where

import Habits.Prelude
import Habits.Domain.Email (Email)

data AccountUpdate = AccountUpdate
  {
    emailConfirmed :: Maybe Bool,
    email :: Maybe Email
  }
  deriving (Show, Eq, Ord)

instance Arbitrary AccountUpdate where
  arbitrary = do
    (email, emailConfirmed) <- arbitrary

    pure $ AccountUpdate {..}

