{-# LANGUAGE RecordWildCards #-}

module Habits.UseCases.Refresh.RefreshResponse where

import Habits.Domain.AccessToken (AccessToken)
import Habits.Domain.RefreshToken (RefreshToken)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

data RefreshResponse = RefreshResponse
  { accessToken :: AccessToken,
    refreshToken :: RefreshToken
  }
  deriving (Show, Eq, Ord)

instance Arbitrary RefreshResponse where
  arbitrary = do
    accessToken <- arbitrary
    refreshToken <- arbitrary
    pure $ RefreshResponse {..}
