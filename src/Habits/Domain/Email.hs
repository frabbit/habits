module Habits.Domain.Email where

import Test.QuickCheck.Instances ()
import           Data.Text                      ( Text )
import           Test.QuickCheck                ( arbitrary, Arbitrary )

newtype Email = Email { unEmail :: Text } deriving (Show, Eq, Ord)

instance Arbitrary Email where
  arbitrary = do
    Email <$> arbitrary
