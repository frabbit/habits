module Habits.Domain.Email where

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import qualified Veins.Data.Codec as Codec
import Veins.Data.Codec (Codec)
import qualified Data.Text as Text
import Data.Text (Text)

newtype Email = Email {unEmail :: Text} deriving (Show, Eq, Ord)

parseEmail :: Text -> Maybe Email
parseEmail t
  | Text.elem '@' t = Just . Email $ t
  | otherwise = Nothing

emailFromText :: Codec Text Email
emailFromText = Codec.withContext (Codec.VECNamed "EmailFromText") (Codec.Codec encoder decoder)
  where
    encoder = Codec.encoderFromMaybe (Codec.convError "Text" "Email") parseEmail
    decoder p = p.unEmail

instance Arbitrary Email where
  arbitrary = do
    prefix <- genValidUtf8WithoutNullByte
    suffix <- genValidUtf8WithoutNullByte
    pure $ Email (prefix <> ("@"::Text) <> suffix)
