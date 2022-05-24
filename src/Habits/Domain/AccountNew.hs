{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Habits.Domain.AccountNew where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.PasswordHash (PasswordHash)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
  )
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

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
