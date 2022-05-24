module Habits.UseCases.Refresh.RefreshResponse where
import Habits.Domain.RefreshToken (RefreshToken)
import Habits.Domain.AccessToken (AccessToken)

data RefreshResponse = RefreshResponse {
  accessToken :: AccessToken,
  refreshToken :: RefreshToken
} deriving (Show,Eq,Ord)

