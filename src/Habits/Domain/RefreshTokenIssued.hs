{-# LANGUAGE RecordWildCards #-}

module Habits.Domain.RefreshTokenIssued where

import Data.Time (UTCTime)
import Habits.Domain.AccountId (AccountId)

import Habits.Domain.RefreshTokenHash (RefreshTokenHash)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import qualified Habits.Domain.RefreshTokenIssuedNew as RN
import Control.Lens (makeLenses)

data RefreshTokenIssued = RefreshTokenIssued {
  _refreshTokenIssuedId :: RefreshTokenIssuedId,
  _expiration :: UTCTime,
  _refreshTokenHash :: RefreshTokenHash,
  _accountId :: AccountId
} deriving (Show, Eq, Ord)

makeLenses ''RefreshTokenIssued

fromRefreshTokenIssuedNew :: RN.RefreshTokenIssuedNew -> RefreshTokenIssuedId -> RefreshTokenIssued
fromRefreshTokenIssuedNew RN.RefreshTokenIssuedNew {..} _refreshTokenIssuedId = RefreshTokenIssued {_refreshTokenIssuedId, ..}

toRefreshTokenIssuedNew :: RefreshTokenIssued -> RN.RefreshTokenIssuedNew
toRefreshTokenIssuedNew RefreshTokenIssued {..} = RN.RefreshTokenIssuedNew {..}