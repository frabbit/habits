module Habits.Domain.AuthConfig.Class where

import Control.Monad.Reader (MonadReader, asks)
import Habits.Domain.AccessTokenSecret (AccessTokenSecret)
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret)
import Veins.Data.Has (Has)
import qualified Veins.Data.Has as Has

class AuthConfig m where
  getAccessTokenSecret :: m AccessTokenSecret
  getRefreshTokenSecret :: m RefreshTokenSecret

instance (MonadReader env m, Has AC.AuthConfig env) => AuthConfig m where
  getAccessTokenSecret = asks (AC._accessTokenSecret . Has.get)
  getRefreshTokenSecret = asks (AC._refreshTokenSecret . Has.get)