{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Habits.Domain.AccountNew where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
  )
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Habits.Domain.PasswordHash (PasswordHash)

data AccountNew = AccountNew
  { _email :: Email,
    _name :: Text,
    _password :: PasswordHash
  }
  deriving (Show, Eq, Ord)

makeLenses ''AccountNew

instance Arbitrary AccountNew where
  arbitrary = do
    _email <- arbitrary
    _name <- genValidUtf8WithoutNullByte
    _password <- arbitrary
    pure $ AccountNew {..}
