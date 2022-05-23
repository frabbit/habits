module Habits.Domain.RefreshTokenIssued where

import Data.Time (UTCTime)
import Habits.Domain.AccountId (AccountId)

data RefreshTokenIssued = RefreshTokenIssued {
  _expiration :: UTCTime,
  --_refreshToken :: RefreshToken,
  _accountId :: AccountId
}