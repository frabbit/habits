{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.AuthConfig.Class where
import Habits.Domain.AccessTokenSecret
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret(RefreshTokenSecret))
import qualified Habits.Domain.AuthConfig as AC

import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader (asks)
import qualified Veins.Data.Has as Has
import Veins.Data.Has (Has)

class AuthConfig m where
  getAccessTokenSecret :: m AccessTokenSecret
  getRefreshTokenSecret :: m RefreshTokenSecret

instance (MonadReader env m, Has AC.AuthConfig env) => AuthConfig m where
  getAccessTokenSecret = asks (AC._accessTokenSecret . Has.get)
  getRefreshTokenSecret = asks (AC._refreshTokenSecret . Has.get)