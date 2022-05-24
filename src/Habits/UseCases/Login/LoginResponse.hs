module Habits.UseCases.Login.LoginResponse where
import Habits.Domain.RefreshToken (RefreshToken)
import Habits.Domain.AccessToken (AccessToken)
import Control.Lens (makeLenses)

data LoginResponse = LoginResponse {
  _accessToken :: AccessToken,
  _refreshToken :: RefreshToken
} deriving (Show,Eq,Ord)

makeLenses ''LoginResponse

