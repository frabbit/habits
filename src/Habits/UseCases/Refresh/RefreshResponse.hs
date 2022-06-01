{-# LANGUAGE RecordWildCards #-}

module Habits.UseCases.Refresh.RefreshResponse where

import Habits.Prelude

import Habits.Domain.AccessToken (AccessToken)
import Habits.Domain.RefreshToken (RefreshToken)

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
