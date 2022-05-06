module Habits.Domain.Password where

import Test.QuickCheck.Instances ()
import           Data.Text                      ( Text )
import           Test.QuickCheck                ( arbitrary, Arbitrary )

newtype Password = Password { unPassword :: Text } deriving (Show, Eq, Ord)

instance Arbitrary Password where
  arbitrary = do
    Password <$> arbitrary
