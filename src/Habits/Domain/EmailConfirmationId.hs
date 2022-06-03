module Habits.Domain.EmailConfirmationId where

import Habits.Prelude

import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()
import qualified Data.UUID as UUID
import Veins.Data.Functor.Utils ((<.>))

newtype EmailConfirmationId = EmailConfirmationId { unEmailConfirmationId :: Text } deriving (Eq, Show, Ord)

parseEmailConfirmationId :: Text -> Maybe EmailConfirmationId
parseEmailConfirmationId = EmailConfirmationId <.> UUID.toText <.> UUID.fromText

unwrap :: EmailConfirmationId -> Text
unwrap (EmailConfirmationId x) = x

instance Arbitrary EmailConfirmationId where
  arbitrary = do
    EmailConfirmationId . toText <$> arbitrary
