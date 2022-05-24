module Habits.UseCases.Refresh.RefreshResponse where
import Habits.Domain.RefreshToken (RefreshToken)
import Habits.Domain.AccessToken (AccessToken)
import Control.Lens (makeLenses)

data RefreshResponse = RefreshResponse {
  _accessToken :: AccessToken,
  _refreshToken :: RefreshToken
} deriving (Show,Eq,Ord)

makeLenses ''RefreshResponse

