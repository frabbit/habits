{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.AccountNew where

import Habits.Prelude

import Habits.Domain.Email (Email)
import Habits.Domain.PasswordHash (PasswordHash)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

data AccountNew = AccountNew
  { email :: Email,
    emailConfirmed :: Bool,
    name :: Text,
    password :: PasswordHash
  }
  deriving (Show, Eq, Ord)

instance Arbitrary AccountNew where
  arbitrary = do
    (password, email, emailConfirmed) <- arbitrary
    name <- genValidUtf8WithoutNullByte
    pure $ AccountNew {..}
