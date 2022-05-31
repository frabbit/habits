{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Habits.UseCases.Refresh.Live where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Function ((&))
import qualified Habits.Domain.AuthConfig as AC
import Habits.UseCases.Refresh
  ( RefreshExec,
  )
import qualified Habits.UseCases.Refresh as R
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest (..))
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.Clock as Clock
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError(RefreshTokenIssuedNotFoundError))
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Habits.Domain.RefreshToken (getAccountId, mkRefreshToken, isExpired)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Data.Foldable (find)
import Habits.Domain.RefreshTokenHash (isValid, mkFromRefreshToken)
import Veins.Data.Time.Utils (addDaysToUTCTime, addHoursToUTCTime)
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse(..))
import Habits.Domain.AccessToken (mkAccessToken)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError(RefreshTokenInvalidError))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew(..))
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError(RefreshTokenExpiredError))
import Control.Monad (when)
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT

mkExecute :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted '[Clock.Clock m, AC.AuthConfig, RT.RefreshTokenIssuedRepo m]) n (RefreshExec m)
mkExecute = do
  getAccessSecret <- AC.mkGetAccessTokenSecret
  getRefreshSecret <- AC.mkGetRefreshTokenSecret
  rtr <- RT.getRefreshTokenIssuedRepo
  getNow <- Clock.mkGetNow
  pure $ \(RefreshRequest token) -> liftE $ S.do
    atSecret <- S.coerce getAccessSecret
    rtSecret <- S.coerce getRefreshSecret
    time <- S.lift getNow
    let expired = isExpired rtSecret token (utcTimeToPOSIXSeconds time)

    when expired (failureE RefreshTokenExpiredError)
    accountId <- case getAccountId rtSecret token of
      Just x -> liftE $ S.pure x
      Nothing -> failureE RefreshTokenInvalidError
    issuedTokens <- RT.getByAccountId rtr accountId

    let found = find (\rti -> isValid token rti.refreshTokenHash) issuedTokens
    rti <- case found of
      Nothing -> S.do
        RT.deleteByAccountId rtr accountId
        failureE RefreshTokenIssuedNotFoundError
      Just rti -> liftE $ S.do
        S.pure rti

    RT.deleteById rtr rti.refreshTokenIssuedId
    let refreshTokenExpiration = addDaysToUTCTime 7 time
    let accessToken = mkAccessToken atSecret accountId (utcTimeToPOSIXSeconds (addHoursToUTCTime 3 time))
    let refreshToken = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds refreshTokenExpiration)
    hash <- S.coerce $ mkFromRefreshToken refreshToken
    RT.add rtr $ RefreshTokenIssuedNew { accountId, expiration = refreshTokenExpiration, refreshTokenHash = hash }
    S.pure $ RefreshResponse { accessToken, refreshToken }

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.ComposableEnv '[AC.AuthConfig, Clock.Clock m, RT.RefreshTokenIssuedRepo m]) n (CE.ComposableEnv '[R.Refresh m])
mkLive = CE.do
  execute <- mkExecute
  CE.pure $ CE.empty & CE.insert (R.Refresh execute)
