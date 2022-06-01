module Habits.Domain.AccountId where

import Habits.Prelude

import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()
import qualified Data.UUID as UUID
import Veins.Data.Functor.Utils ((<.>))

newtype AccountId = AccountId { unAccountId :: Text } deriving (Eq, Show, Ord)

parseAccountId :: Text -> Maybe AccountId
parseAccountId = AccountId <.> UUID.toText <.> UUID.fromText

unwrap :: AccountId -> Text
unwrap (AccountId x) = x

instance Arbitrary AccountId where
  arbitrary = do
    AccountId . toText <$> arbitrary
