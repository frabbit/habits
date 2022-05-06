module Habits.Domain.Password where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()

newtype Password = Password {unPassword :: Text} deriving (Show, Eq, Ord)

instance Arbitrary Password where
  arbitrary = do
    Password <$> arbitrary
