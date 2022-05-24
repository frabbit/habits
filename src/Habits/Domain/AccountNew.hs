{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.AccountNew where

import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.PasswordHash (PasswordHash)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
  )
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

data AccountNew = AccountNew
  { email :: Email,
    name :: Text,
    password :: PasswordHash
  }
  deriving (Show, Eq, Ord)

instance Arbitrary AccountNew where
  arbitrary = do
    email <- arbitrary
    name <- genValidUtf8WithoutNullByte
    password <- arbitrary
    pure $ AccountNew {..}
