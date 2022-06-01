module Habits.Domain.Password where

import Habits.Prelude
import Test.QuickCheck (chooseInt)
import Test.QuickCheck.Instances ()
import Veins.Test.QuickCheck (genUtf8CharacterWithoutNullByte)
import qualified Veins.Data.Codec as Codec
import Veins.Data.Codec (Codec)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8')

newtype Password = Password {unPassword :: Text} deriving (Show, Eq, Ord)

instance Arbitrary Password where
  arbitrary = do
    size <- chooseInt (10, 50)
    bytes <- BS.concat <$> replicateM size genUtf8CharacterWithoutNullByte
    case decodeUtf8' bytes of
      Left _ -> arbitrary
      Right t -> pure $ Password t

parsePassword :: Text -> Maybe Password
parsePassword t
  | Text.length t >= 10 && Text.length t <= 50 = Just . Password $ t
  | otherwise = Nothing

passwordFromText :: Codec Text Password
passwordFromText = Codec.withContext (Codec.VECNamed "PasswordFromText") (Codec.Codec encoder decoder)
  where
    encoder = Codec.encoderFromMaybe (Codec.convError "Text" "Password") parsePassword
    decoder p = p.unPassword
