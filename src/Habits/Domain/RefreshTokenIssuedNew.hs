{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.RefreshTokenIssuedNew where

import Data.Time (UTCTime)
import Habits.Domain.AccountId (AccountId)

import Habits.Domain.RefreshTokenHash (RefreshTokenHash)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))

data RefreshTokenIssuedNew = RefreshTokenIssuedNew {
  _expiration :: UTCTime,
  _refreshTokenHash :: RefreshTokenHash,
  _accountId :: AccountId
} deriving (Eq, Show, Ord)

instance Arbitrary RefreshTokenIssuedNew where
  arbitrary = do
    (_expiration, _refreshTokenHash, _accountId) <- arbitrary

    let token = RefreshTokenIssuedNew {..}
    pure token