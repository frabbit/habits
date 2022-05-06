module Habits.Domain.Email where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()

newtype Email = Email {unEmail :: Text} deriving (Show, Eq, Ord)

instance Arbitrary Email where
  arbitrary = do
    Email <$> arbitrary
