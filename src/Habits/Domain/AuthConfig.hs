module Habits.Domain.AuthConfig where

import Habits.Domain.AccessTokenSecret (AccessTokenSecret)
import Habits.Domain.RefreshTokenSecret ( RefreshTokenSecret )
import Veins.Data.ToSymbol (ToSymbol)
import Control.Monad.Reader (MonadReader, asks)
import qualified Veins.Data.Has as Has

data AuthConfig = AuthConfig
  { _accessTokenSecret :: AccessTokenSecret,
    _refreshTokenSecret :: RefreshTokenSecret
  }

getAccessTokenSecret :: forall m env. (Has.Has AuthConfig env, MonadReader env m) => m AccessTokenSecret
getAccessTokenSecret = asks (_accessTokenSecret . Has.get)

getRefreshTokenSecret :: forall m env. (Has.Has AuthConfig env, MonadReader env m) => m RefreshTokenSecret
getRefreshTokenSecret = asks (_refreshTokenSecret . Has.get)

mkGetAccessTokenSecret :: forall n m env. (Applicative n, Has.Has AuthConfig env, MonadReader env m) => m (n AccessTokenSecret)
mkGetAccessTokenSecret = asks (pure . _accessTokenSecret . Has.get)

mkGetRefreshTokenSecret :: forall n m env. (Applicative n, Has.Has AuthConfig env, MonadReader env m) => m (n RefreshTokenSecret)
mkGetRefreshTokenSecret = asks (pure . _refreshTokenSecret . Has.get)


type instance ToSymbol AuthConfig = "AuthConfig"

