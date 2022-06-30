module Habits.Domain.AccountId where

import Habits.Prelude

import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()
import qualified Data.UUID as UUID
import Veins.Data.Functor.Utils ((<.>))
import Veins.Data.Codec (Codec(..))
import qualified Veins.Data.Codec as Codec

newtype AccountId = AccountId { unAccountId :: Text } deriving (Eq, Show, Ord)

accountIdFromText :: Codec Text AccountId
accountIdFromText = Codec.withContext (Codec.VECNamed "AccountIdFromText") (Codec.Codec encoder decoder)
  where
    encoder = Codec.encoderFromMaybe (Codec.convError "Text" "Email") parseAccountId
    decoder p = p.unAccountId

parseAccountId :: Text -> Maybe AccountId
parseAccountId = AccountId <.> UUID.toText <.> UUID.fromText

unwrap :: AccountId -> Text
unwrap (AccountId x) = x

instance Arbitrary AccountId where
  arbitrary = do
    AccountId . toText <$> arbitrary
