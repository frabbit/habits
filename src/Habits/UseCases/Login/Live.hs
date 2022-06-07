{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Habits.UseCases.Login.Live where

import Habits.Prelude
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.AccessToken (mkAccessToken)
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.PasswordHash (isValid)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError (PasswordIncorrectError))
import Habits.Domain.RefreshToken (mkRefreshToken)
import Habits.UseCases.Login
  ( LoginExec,
  )
import qualified Habits.UseCases.Login as L
import Habits.UseCases.Login.LoginRequest (LoginRequest (..))
import Habits.UseCases.Login.LoginResponse (LoginResponse (..))
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.Clock as Clock
import Veins.Data.Time.Utils (addHoursToUTCTime, addDaysToUTCTime)
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew(RefreshTokenIssuedNew, accountId, expiration, refreshTokenHash))
import Habits.Domain.RefreshTokenHash (mkFromRefreshToken)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT

type Deps m = CE.MkSorted '[Clock.Clock m, AC.AuthConfig m, AR.AccountRepo m, RT.RefreshTokenIssuedRepo m]

mkLogin :: forall n m. (Monad n, MonadIO m) => ReaderT (Deps m) n (LoginExec m)
mkLogin = do
  authConfig <- AC.getAuthConfig
  rtr <- RT.getRefreshTokenIssuedRepo
  ar <- AR.getAccountRepo
  clock <- Clock.getClock
  pure $ \(EmailPasswordLoginRequest email pw) -> liftE $ S.do
    acc <- AR.getByEmailOrFail ar email
    accessSecret <- S.lift authConfig.accessTokenSecret
    refreshSecret <- S.lift authConfig.refreshTokenSecret
    unless (isValid pw acc.password) (failureE PasswordIncorrectError)
    -- unless (acc.emailConfirmed) (failureE EmailNotConfirmedError)
    time <- S.lift clock._getNow
    let refreshTokenExpiration = addDaysToUTCTime 7 time
    let accessToken = mkAccessToken accessSecret (acc.accountId) (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let refreshToken = mkRefreshToken refreshSecret (acc.accountId) (utcTimeToPOSIXSeconds refreshTokenExpiration)
    hash <- S.coerce $ mkFromRefreshToken refreshToken
    RT.add rtr $ RefreshTokenIssuedNew { accountId = acc.accountId, expiration = refreshTokenExpiration, refreshTokenHash = hash}
    S.coerce . pure $ LoginResponse{ accessToken, refreshToken }

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (Deps m) n (CE.ComposableEnv '[L.Login m])
mkLive = CE.singleton . L.Login <$> mkLogin
