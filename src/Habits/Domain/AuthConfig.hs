module Habits.Domain.AuthConfig where

import Habits.Prelude
import Habits.Domain.AccessTokenSecret (AccessTokenSecret)
import Habits.Domain.RefreshTokenSecret ( RefreshTokenSecret )
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import qualified Veins.Data.ComposableEnv as CE

data AuthConfig = AuthConfig
  { _accessTokenSecret :: AccessTokenSecret,
    _refreshTokenSecret :: RefreshTokenSecret
  }

getAuthConfig :: forall m env. (Has.Has AuthConfig env, MonadReader env m) => m AuthConfig
getAuthConfig = asks Has.get

getAccessTokenSecret :: forall m env. (Has.Has AuthConfig env, MonadReader env m) => m AccessTokenSecret
getAccessTokenSecret = asks $ _accessTokenSecret . Has.get

getRefreshTokenSecret :: forall m env. (Has.Has AuthConfig env, MonadReader env m) => m RefreshTokenSecret
getRefreshTokenSecret = asks $ _refreshTokenSecret . Has.get

mkGetAccessTokenSecret :: forall n m env. (Applicative n, Has.Has AuthConfig env, MonadReader env m) => m (n AccessTokenSecret)
mkGetAccessTokenSecret = asks $ pure . _accessTokenSecret . Has.get

mkGetRefreshTokenSecret :: forall n m env. (Applicative n, Has.Has AuthConfig env, MonadReader env m) => m (n RefreshTokenSecret)
mkGetRefreshTokenSecret = asks $ pure . _refreshTokenSecret . Has.get


type instance ToSymbol AuthConfig = "AuthConfig"

mkAuthConfigStatic :: forall n. (Monad n) => _ -> _ -> ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AuthConfig])
mkAuthConfigStatic atSecret rtSecret = pure $ CE.singleton AuthConfig {_accessTokenSecret = atSecret, _refreshTokenSecret = rtSecret}