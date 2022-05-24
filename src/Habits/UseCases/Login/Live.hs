module Habits.UseCases.Login.Live where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Function ((&))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.AccessToken (mkAccessToken)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    getByEmailOrFail,
  )
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.PasswordHash (isValid)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError (PasswordIncorrectError))
import Habits.Domain.RefreshToken (mkRefreshToken)
import Habits.UseCases.Login
  ( Execute,
  )
import qualified Habits.UseCases.Login as L
import Habits.UseCases.Login.LoginRequest (LoginRequest (..))
import Habits.UseCases.Login.LoginResponse (LoginResponse (..))
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.TimeProvider as TP
import Veins.Data.Time.Utils (addHoursToUTCTime, addDaysToUTCTime)
import Habits.Domain.RefreshTokenIssuedRepo.Class (RefreshTokenIssuedRepo)
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew(RefreshTokenIssuedNew, _accountId, _expiration, _refreshTokenHash))
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RefreshTokenIssuedRepo
import Habits.Domain.RefreshTokenHash (mkFromRefreshToken)

mkExecute :: forall n m. (Monad n, MonadIO m, AccountRepo m, RefreshTokenIssuedRepo m) => ReaderT (CE.MkSorted '[TP.TimeProvider m, AC.AuthConfig]) n (Execute m)
mkExecute = do
  getAccessSecret <- AC.mkGetAccessTokenSecret
  getRefreshSecret <- AC.mkGetRefreshTokenSecret
  getNow <- TP.mkGetNow
  pure $ \(EmailPasswordLoginRequest email pw) -> liftE $ S.do
    acc <- getByEmailOrFail email
    unless (isValid pw acc.password) (failureE PasswordIncorrectError)
    time <- S.lift getNow
    accessSecret <- S.coerce getAccessSecret
    refreshSecret <- S.coerce getRefreshSecret
    let refreshTokenExpiration = addDaysToUTCTime 7 time
    let accessToken = mkAccessToken accessSecret (acc.accountId) (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let refreshToken = mkRefreshToken refreshSecret (acc.accountId) (utcTimeToPOSIXSeconds refreshTokenExpiration)
    hash <- S.coerce $ mkFromRefreshToken refreshToken
    RefreshTokenIssuedRepo.add $ RefreshTokenIssuedNew { _accountId = acc.accountId, _expiration = refreshTokenExpiration, _refreshTokenHash = hash}
    S.coerce . pure $ LoginResponse{ accessToken, refreshToken }

mkLive :: forall n m. (Monad n, MonadIO m, AccountRepo m, RefreshTokenIssuedRepo m) => ReaderT (CE.ComposableEnv '[AC.AuthConfig, TP.TimeProvider m]) n (CE.ComposableEnv '[L.Login m])
mkLive = CE.do
  execute <- mkExecute
  CE.pure $ CE.empty & CE.insert L.Login {L._execute = execute}
