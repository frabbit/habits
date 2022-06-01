{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.RefreshTokenIssued where

import Habits.Prelude

import Data.Time (UTCTime)
import Habits.Domain.AccountId (AccountId)

import Habits.Domain.RefreshTokenHash (RefreshTokenHash)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import qualified Habits.Domain.RefreshTokenIssuedNew as RN

data RefreshTokenIssued = RefreshTokenIssued {
  refreshTokenIssuedId :: RefreshTokenIssuedId,
  expiration :: UTCTime,
  refreshTokenHash :: RefreshTokenHash,
  accountId :: AccountId
} deriving (Show, Eq, Ord)

fromRefreshTokenIssuedNew :: RN.RefreshTokenIssuedNew -> RefreshTokenIssuedId -> RefreshTokenIssued
fromRefreshTokenIssuedNew RN.RefreshTokenIssuedNew {..} refreshTokenIssuedId = RefreshTokenIssued { refreshTokenIssuedId, ..}

toRefreshTokenIssuedNew :: RefreshTokenIssued -> RN.RefreshTokenIssuedNew
toRefreshTokenIssuedNew RefreshTokenIssued {..} = RN.RefreshTokenIssuedNew {..}