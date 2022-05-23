module Habits.Domain.RefreshTokenIssuedId where

import Data.Text (Text)
import Data.UUID (toText)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances.UUID ()

newtype RefreshTokenIssuedId = RefreshTokenIssuedId Text deriving (Eq, Show, Ord)

unwrap :: RefreshTokenIssuedId -> Text
unwrap (RefreshTokenIssuedId x) = x

instance Arbitrary RefreshTokenIssuedId where
  arbitrary = do
    RefreshTokenIssuedId . toText <$> arbitrary
