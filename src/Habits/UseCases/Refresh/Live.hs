module Habits.UseCases.Refresh.Live where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Function ((&))
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
  )
import qualified Habits.Domain.AuthConfig as AC
import Habits.UseCases.Refresh
  ( RefreshExec,
  )
import qualified Habits.UseCases.Refresh as R
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest (..))
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.TimeProvider as TP
import Habits.Domain.RefreshTokenIssuedRepo.Class (RefreshTokenIssuedRepo (getByAccountId, deleteById, add, deleteByAccountId))
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError(RefreshTokenIssuedNotFoundError))
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Habits.Domain.RefreshToken (getAccountId, RefreshToken (RefreshToken), mkRefreshToken, isExpired)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Data.Foldable (find)
import Habits.Domain.RefreshTokenHash (isValid, mkFromRefreshToken)
import Veins.Data.Time.Utils (addDaysToUTCTime, addHoursToUTCTime)
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse(..))
import Habits.Domain.AccessToken (AccessToken(..), mkAccessToken)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError(RefreshTokenInvalidError))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.RefreshTokenIssued (refreshTokenIssuedId)
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RefreshTokenIssuedRepo
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew(..))
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError(RefreshTokenExpiredError))
import Control.Monad (when)

mkExecute :: forall n m. (Monad n, MonadIO m, AccountRepo m, RefreshTokenIssuedRepo m) => ReaderT (CE.MkSorted '[TP.TimeProvider m, AC.AuthConfig]) n (RefreshExec m)
mkExecute = do
  getAccessSecret <- AC.mkGetAccessTokenSecret
  getRefreshSecret <- AC.mkGetRefreshTokenSecret
  getNow <- TP.mkGetNow
  pure $ \(RefreshRequest token) -> liftE $ S.do
    atSecret <- S.coerce getAccessSecret
    rtSecret <- S.coerce getRefreshSecret
    time <- S.lift getNow
    let expired = isExpired rtSecret token (utcTimeToPOSIXSeconds time)
    when expired (failureE RefreshTokenExpiredError)
    accountId <- case getAccountId rtSecret token of
      Just x -> liftE $ S.pure x
      Nothing -> failureE RefreshTokenInvalidError
    issuedTokens <- getByAccountId accountId
    let found = find (\rti -> isValid token rti._refreshTokenHash) issuedTokens
    rti <- case found of
      Nothing -> S.do
        deleteByAccountId accountId
        failureE RefreshTokenIssuedNotFoundError
      Just rti -> liftE $ S.do
        S.pure rti

    deleteById rti._refreshTokenIssuedId
    let refreshTokenExpiration = addDaysToUTCTime 7 time
    let _accessToken = mkAccessToken atSecret accountId (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let _refreshToken = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds refreshTokenExpiration)
    hash <- S.coerce $ mkFromRefreshToken _refreshToken
    RefreshTokenIssuedRepo.add $ RefreshTokenIssuedNew { _accountId = accountId, _expiration = refreshTokenExpiration, _refreshTokenHash = hash }
    S.pure $ RefreshResponse { _accessToken, _refreshToken}

mkLive :: forall n m. (Monad n, MonadIO m, AccountRepo m, RefreshTokenIssuedRepo m) => ReaderT (CE.ComposableEnv '[AC.AuthConfig, TP.TimeProvider m]) n (CE.ComposableEnv '[R.Refresh m])
mkLive = CE.do
  execute <- mkExecute
  CE.pure $ CE.empty & CE.insert (R.Refresh execute)
