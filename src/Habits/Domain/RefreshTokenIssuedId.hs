module Habits.Domain.RefreshTokenIssuedId where

import Habits.Prelude
import Data.UUID (toText)
import Test.QuickCheck.Instances.UUID ()

newtype RefreshTokenIssuedId = RefreshTokenIssuedId Text deriving (Eq, Show, Ord)

unwrap :: RefreshTokenIssuedId -> Text
unwrap (RefreshTokenIssuedId x) = x

instance Arbitrary RefreshTokenIssuedId where
  arbitrary = do
    RefreshTokenIssuedId . toText <$> arbitrary
