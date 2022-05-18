{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Register.RegisterRequest where

import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)
import Control.Lens (makeLenses)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Test.QuickCheck (Arbitrary (arbitrary))

data RegisterRequest = RegisterRequest
  { _email :: Email,
    _name :: Text,
    _password :: Password
  } deriving (Show, Eq, Ord)

makeLenses ''RegisterRequest

instance Arbitrary RegisterRequest where
  arbitrary = do
    _email <- arbitrary
    _name <- genValidUtf8WithoutNullByte
    _password <- arbitrary
    pure $ RegisterRequest {..}
