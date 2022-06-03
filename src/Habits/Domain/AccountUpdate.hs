{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.AccountUpdate where

import Habits.Prelude

data AccountUpdate = AccountUpdate
  {
    emailConfirmed :: Maybe Bool
  }
  deriving (Show, Eq, Ord)

instance Arbitrary AccountUpdate where
  arbitrary = do
    emailConfirmed <- arbitrary
    pure $ AccountUpdate {..}

