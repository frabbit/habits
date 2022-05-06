module Habits.Domain.AccountId where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary (arbitrary))
import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()

newtype AccountId = AccountId Text deriving (Eq, Show, Ord)

unwrap :: AccountId -> Text
unwrap (AccountId x) = x

instance Arbitrary AccountId where
  arbitrary = do
    AccountId . toText <$> arbitrary
