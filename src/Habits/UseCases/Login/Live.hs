module Habits.UseCases.Login.Live where

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    getByEmail, getByEmailOrFail
  )
import Habits.UseCases.Login
  ( Execute
  )
import qualified Habits.UseCases.Login as L
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Data.Function ((&))
import qualified Veins.Data.ComposableEnv as CE
import Control.Monad.Reader (ReaderT)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (MonadIO)
import Habits.UseCases.Login.LoginResponse (LoginResponse(LoginResponse), _refreshToken, _accessToken)
import Habits.UseCases.Login.LoginRequest (LoginRequest(..))
import Habits.Domain.AccountNotFoundError (AccountNotFoundError(AccountNotFoundError))
import Habits.Domain.PasswordHash (isValid)
import qualified Habits.Domain.Account as A
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError(PasswordIncorrectError))
import Control.Lens ((^.))
import Control.Monad (unless)
import Habits.Domain.AccessToken (mkAccessToken)
import Habits.Domain.AccessTokenSecret (mkAccessTokenSecret)
import Habits.Domain.RefreshTokenSecret (mkRefreshTokenSecret)
import Data.Time (getCurrentTime)
import Habits.Domain.RefreshToken (mkRefreshToken)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)



execute :: (Monad m, MonadIO m, AccountRepo m) => Execute m
execute (EmailPasswordLoginRequest email pw) = liftE $ S.do
  acc <- getByEmailOrFail email
  unless (isValid pw (acc ^. A.password)) (failureE PasswordIncorrectError)
  time <- S.liftIO getCurrentTime
  let _accessToken = mkAccessToken   (mkAccessTokenSecret "abc") (acc ^. A.accountId) (utcTimeToPOSIXSeconds time)
  let _refreshToken = mkRefreshToken (mkRefreshTokenSecret "abc") (acc ^. A.accountId) (utcTimeToPOSIXSeconds time)
  S.coerce . pure $ LoginResponse { _accessToken, _refreshToken }


mkLive :: forall n m. (Monad n, MonadIO m, AccountRepo m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[L.Login m])
mkLive = pure $ CE.empty & CE.insert L.Login {L._execute = execute}
