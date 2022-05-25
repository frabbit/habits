{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.UseCases.RefreshSpec (spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.AccessToken (AccessToken (..))
import qualified Habits.Domain.AccessToken as AccessToken
import qualified Habits.Domain.AccessTokenSecret as ATS
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.AuthConfig.Class as ACC
import Habits.Domain.RefreshToken (RefreshToken (..), mkRefreshToken)
import qualified Habits.Domain.RefreshToken as RefreshToken
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError)
import Habits.Domain.RefreshTokenHash (mkFromRefreshToken)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError)
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError))
import Habits.Domain.RefreshTokenIssuedRepo (getByAccountId)
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RTC
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.TimeProvider as TP
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import qualified Habits.UseCases.Refresh as Refresh
import Habits.UseCases.Refresh.Class (Refresh (refresh))
import qualified Habits.UseCases.Refresh.Live as RefreshLive
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest (RefreshRequest))
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse (..))
import Haskus.Utils.Variant.Excepts (catchLiftRight, evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe,
  )
import Utils (catchAllToFail, expectError, sampleIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.HSpec.TH (shouldMatchPattern)
import Prelude hiding (id)

type Env m = CE.MkSorted '[AR.AccountRepo m, Refresh.Refresh m, RT.RefreshTokenIssuedRepo m, AC.AuthConfig]

atSecret :: _
atSecret = ATS.mkAccessTokenSecret "abcd"

rtSecret :: _
rtSecret = RTS.mkRefreshTokenSecret "abcde"

timeNow :: Time.UTCTime
timeNow = Time.UTCTime (Time.fromGregorian 2022 1 2) (Time.secondsToDiffTime 0)

timeExpired :: Time.UTCTime
timeExpired = Time.UTCTime (Time.fromGregorian 2021 1 2) (Time.secondsToDiffTime 0)

ac :: forall n. (Monad n) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AC.AuthConfig])
ac = pure $ CE.empty & CE.insert AC.AuthConfig {_accessTokenSecret = atSecret, _refreshTokenSecret = rtSecret}

tp :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[TP.TimeProvider m])
tp = pure $ CE.empty & CE.insert TP.TimeProvider {_getNow = pure timeNow}

envLayer :: forall m n. (MonadIO n, RTC.RefreshTokenIssuedRepo m, ARC.AccountRepo m, MonadIO m, ACC.AuthConfig m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer =
  ARM.mkAccountRepoMemory
    `CE.provideAndChainLayerFlipped` RTL.mkRefreshTokenIssuedRepoMemory
    `CE.provideAndChainLayerFlipped` RefreshLive.mkLive
    `CE.provideAndChainLayerFlipped` ac
    `CE.provideLayerFlipped` tp

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ -> _ b -> IO b
runWithEnv layer app = do
  env <- runReaderT layer CE.empty
  runApp env app

addValidExpiredToken :: _ => _
addValidExpiredToken = addToken timeExpired

addToken :: _ => _
addToken expirationTime = S.do
  (rtiNew, accountId) <- S.coerce sampleIO
  let token = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds expirationTime)
  hash <- S.coerce $ mkFromRefreshToken token
  RTC.add (rtiNew{refreshTokenHash = hash, accountId = accountId})
  S.pure (token, hash, accountId)

addValidToken :: _ => _
addValidToken = addToken timeNow

embed :: _ => _
embed = runWithEnv (envLayer :: _) . evalE . catchAllToFail

spec :: Spec
spec = describe "refresh should" $ do
  it "fail with RefreshTokenIssuedNotFoundError when refreshToken does not exist" . embed $
    S.do
      accountId <- S.coerce sampleIO
      let token = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds timeNow)
      refresh (RefreshRequest token)
      & expectError @RefreshTokenIssuedNotFoundError
  it "fail with RefreshTokenInvalidError when refreshToken was signed with a different secret" . embed $
    S.do
      token <- S.coerce sampleIO
      refresh (RefreshRequest token)
      & expectError @RefreshTokenInvalidError
  it "succeed with new AccessToken and RefreshToken when refreshToken is found" . embed $ S.do
    (token, _, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    S.coerce $('resp `shouldMatchPattern` [p|RefreshResponse (AccessToken _) (RefreshToken _)|])
  it "return a valid access token" . embed $ S.do
    (token, _, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    let verifyResult = AccessToken.verifyAccessToken atSecret resp.accessToken
    S.coerce $ verifyResult `shouldBe` True
  it "return a valid refresh token" . embed $ S.do
    (token, _, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    let verifyResult = RefreshToken.verifyRefreshToken rtSecret resp.refreshToken
    S.coerce $ verifyResult `shouldBe` True
  it "fail with RefreshTokenExpiredError when token is expired" . embed $
    S.do
      (token, _, _) <- addValidExpiredToken
      refresh (RefreshRequest token)
      & expectError @RefreshTokenExpiredError
  it "return a refreshToken which can be used to refresh successfully again" . embed $ S.do
    (token, _, _) <- addValidToken
    resp1 <- refresh (RefreshRequest token)
    resp2 <- refresh (RefreshRequest resp1.refreshToken)
    S.coerce $('resp2 `shouldMatchPattern` [p|RefreshResponse (AccessToken _) (RefreshToken _)|])
  it "invalidate the used refreshToken which can not be used again afterwards" . embed $
    S.do
      (token, _, _) <- addValidToken
      refresh (RefreshRequest token)
      refresh (RefreshRequest token)
      & expectError @RefreshTokenIssuedNotFoundError
  it "invalidate all refreshTokens of a user when a valid, but non-existing refreshToken is used (token theft)" . embed $ S.do
    (token, _, accountId) <- addValidToken
    refresh (RefreshRequest token)
    refresh (RefreshRequest token) & void & catchLiftRight (\RefreshTokenIssuedNotFoundError -> S.pure ())
    tokens <- getByAccountId accountId
    S.coerce $ tokens `shouldBe` []
