module Habits.Domain.AccountId where

import Data.Text (Text)
import Data.UUID (toText)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances.UUID ()

newtype AccountId = AccountId { unAccountId :: Text } deriving (Eq, Show, Ord)

unwrap :: AccountId -> Text
unwrap (AccountId x) = x

instance Arbitrary AccountId where
  arbitrary = do
    AccountId . toText <$> arbitrary
