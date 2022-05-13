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
import Test.QuickCheck.Utf8 (genValidUtf8)

data AccountNew = AccountNew
  { _email :: Email,
    _name :: Text,
    _password :: Password
  }
  deriving (Show, Eq, Ord)

makeLenses ''AccountNew

instance Arbitrary AccountNew where
  arbitrary = do
    _email <- arbitrary
    _name <- genValidUtf8
    _password <- arbitrary
    pure $ AccountNew {..}
