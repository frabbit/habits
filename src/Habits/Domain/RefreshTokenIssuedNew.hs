{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.RefreshTokenIssuedNew where

import Prelude
import Data.Time (UTCTime)
import Habits.Domain.AccountId (AccountId)

import Habits.Domain.RefreshTokenHash (RefreshTokenHash)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))

data RefreshTokenIssuedNew = RefreshTokenIssuedNew {
  expiration :: UTCTime,
  refreshTokenHash :: RefreshTokenHash,
  accountId :: AccountId
} deriving (Eq, Show, Ord)

instance Arbitrary RefreshTokenIssuedNew where
  arbitrary = do
    (expiration, refreshTokenHash, accountId) <- arbitrary

    let token = RefreshTokenIssuedNew {..}
    pure token