module Habits.Domain.AuthConfig.Class where

import Habits.Prelude
import Habits.Domain.AccessTokenSecret (AccessTokenSecret)
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret)
import Veins.Data.Has (Has)
import Habits.Utils (applyFirst0M)

class AuthConfigM m where
  getAccessTokenSecret :: m AccessTokenSecret
  getRefreshTokenSecret :: m RefreshTokenSecret

instance (MonadReader env m, Has (AC.AuthConfig m) env) => AuthConfigM m where
  getAccessTokenSecret = applyFirst0M AC.accessTokenSecret
  getRefreshTokenSecret = applyFirst0M AC.refreshTokenSecret