{-# LANGUAGE RecordWildCards #-}
module Habits.UseCases.Login.LoginResponse where
import Habits.Domain.RefreshToken (RefreshToken)
import Habits.Domain.AccessToken (AccessToken)
import Test.QuickCheck (Arbitrary (arbitrary))

data LoginResponse = LoginResponse {
  accessToken :: AccessToken,
  refreshToken :: RefreshToken
} deriving (Show,Eq,Ord)

instance Arbitrary LoginResponse where
  arbitrary = do
    accessToken <- arbitrary
    refreshToken <- arbitrary
    pure $ LoginResponse { .. }
