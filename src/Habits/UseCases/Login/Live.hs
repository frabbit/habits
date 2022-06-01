module Habits.UseCases.Login.Live where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Function ((&))
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
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.Clock as Clock
import Veins.Data.Time.Utils (addHoursToUTCTime, addDaysToUTCTime)
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew(RefreshTokenIssuedNew, accountId, expiration, refreshTokenHash))
import Habits.Domain.RefreshTokenHash (mkFromRefreshToken)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import Habits.Domain.AccountRepo (getAccountRepo)

mkLogin :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted '[Clock.Clock m, AC.AuthConfig, AR.AccountRepo m, RT.RefreshTokenIssuedRepo m]) n (LoginExec m)
mkLogin = do
  getAccessSecret <- AC.mkGetAccessTokenSecret
  getRefreshSecret <- AC.mkGetRefreshTokenSecret
  rtr <- RT.getRefreshTokenIssuedRepo
  ar <- getAccountRepo
  getNow <- Clock.mkGetNow
  pure $ \(EmailPasswordLoginRequest email pw) -> liftE $ S.do
    acc <- AR.getByEmailOrFail ar email
    unless (isValid pw acc.password) (failureE PasswordIncorrectError)
    time <- S.lift getNow
    accessSecret <- S.coerce getAccessSecret
    refreshSecret <- S.coerce getRefreshSecret
    let refreshTokenExpiration = addDaysToUTCTime 7 time
    let accessToken = mkAccessToken accessSecret (acc.accountId) (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let refreshToken = mkRefreshToken refreshSecret (acc.accountId) (utcTimeToPOSIXSeconds refreshTokenExpiration)
    hash <- S.coerce $ mkFromRefreshToken refreshToken
    RT.add rtr $ RefreshTokenIssuedNew { accountId = acc.accountId, expiration = refreshTokenExpiration, refreshTokenHash = hash}
    S.coerce . pure $ LoginResponse{ accessToken, refreshToken }

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted '[AC.AuthConfig, Clock.Clock m, RT.RefreshTokenIssuedRepo m, AR.AccountRepo m]) n (CE.ComposableEnv '[L.Login m])
mkLive = CE.do
  f <- mkLogin
  CE.pure $ CE.empty & CE.insert (L.Login f)
