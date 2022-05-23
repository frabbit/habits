module Habits.UseCases.Login.Live where

import Control.Lens ((^.))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.AccessToken (mkAccessToken)
import Habits.Domain.AccessTokenSecret (mkAccessTokenSecret)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    getByEmail,
    getByEmailOrFail,
  )
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.AuthConfig.Class (AuthConfig (getAccessTokenSecret, getRefreshTokenSecret))
import Habits.Domain.PasswordHash (isValid)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError (PasswordIncorrectError))
import Habits.Domain.RefreshToken (mkRefreshToken)
import Habits.Domain.RefreshTokenSecret (mkRefreshTokenSecret)
import Habits.UseCases.Login
  ( Execute,
  )
import qualified Habits.UseCases.Login as L
import Habits.UseCases.Login.LoginRequest (LoginRequest (..))
import Habits.UseCases.Login.LoginResponse (LoginResponse (LoginResponse), _accessToken, _refreshToken)
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.TimeProvider as TP
import Veins.Data.Time.Utils (addHoursToUTCTime, addDaysToUTCTime)

mkExecute :: forall n m. (Monad n, MonadIO m, AccountRepo m) => ReaderT (CE.MkSorted '[TP.TimeProvider m, AC.AuthConfig]) n (Execute m)
mkExecute = do
  getAccessSecret <- AC.mkGetAccessTokenSecret
  getRefreshSecret <- AC.mkGetRefreshTokenSecret
  getNow <- TP.mkGetNow
  pure $ \(EmailPasswordLoginRequest email pw) -> liftE $ S.do
    acc <- getByEmailOrFail email
    unless (isValid pw (acc ^. A.password)) (failureE PasswordIncorrectError)
    time <- S.lift getNow
    accessSecret <- S.coerce getAccessSecret
    refreshSecret <- S.coerce getRefreshSecret
    let _accessToken = mkAccessToken accessSecret (acc ^. A.accountId) (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let _refreshToken = mkRefreshToken refreshSecret (acc ^. A.accountId) (utcTimeToPOSIXSeconds (addDaysToUTCTime 7 time))
    S.coerce . pure $ LoginResponse {_accessToken, _refreshToken}

mkLive :: forall n m. (Monad n, MonadIO m, AccountRepo m) => ReaderT (CE.ComposableEnv '[AC.AuthConfig, TP.TimeProvider m]) n (CE.ComposableEnv '[L.Login m])
mkLive = CE.do
  execute <- mkExecute
  CE.pure $ CE.empty & CE.insert L.Login {L._execute = execute}
