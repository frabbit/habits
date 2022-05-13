module Habits.Domain.Email where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Utf8 (genValidUtf8)

newtype Email = Email {unEmail :: Text} deriving (Show, Eq, Ord)

instance Arbitrary Email where
  arbitrary = do
    Email <$> genValidUtf8
