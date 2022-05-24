module Habits.UseCases.Login.LoginResponse where
import Habits.Domain.RefreshToken (RefreshToken)
import Habits.Domain.AccessToken (AccessToken)

data LoginResponse = LoginResponse {
  accessToken :: AccessToken,
  refreshToken :: RefreshToken
} deriving (Show,Eq,Ord)

