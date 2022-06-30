module Habits.Domain.HabitId where

import Habits.Prelude

import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()
import qualified Data.UUID as UUID
import Veins.Data.Functor.Utils ((<.>))

newtype HabitId = HabitId { unHabitId :: Text } deriving (Eq, Show, Ord)

parseHabitId :: Text -> Maybe HabitId
parseHabitId = HabitId <.> UUID.toText <.> UUID.fromText

unwrap :: HabitId -> Text
unwrap (HabitId x) = x

instance Arbitrary HabitId where
  arbitrary = do
    HabitId . toText <$> arbitrary
