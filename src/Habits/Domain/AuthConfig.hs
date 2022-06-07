module Habits.Domain.AuthConfig where

import Habits.Prelude
import Habits.Domain.AccessTokenSecret (AccessTokenSecret)
import Habits.Domain.RefreshTokenSecret ( RefreshTokenSecret )
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import qualified Veins.Data.ComposableEnv as CE

data AuthConfig m = AuthConfig
  { accessTokenSecret :: m AccessTokenSecret,
    refreshTokenSecret :: m RefreshTokenSecret
  }

getAuthConfig :: forall m n env. (Has.Has (AuthConfig m) env, MonadReader env n) => n (AuthConfig m)
getAuthConfig = asks Has.get

type instance ToSymbol (AuthConfig m) = "AuthConfig"

mkAuthConfigStatic :: forall n m . (Monad n, Applicative m) => AccessTokenSecret -> RefreshTokenSecret -> ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AuthConfig m])
mkAuthConfigStatic atSecret rtSecret = pure $ CE.singleton AuthConfig {accessTokenSecret = pure atSecret, refreshTokenSecret = pure rtSecret}