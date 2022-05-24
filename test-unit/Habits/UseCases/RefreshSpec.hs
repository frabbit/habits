{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.RefreshSpec where


import Prelude hiding (id)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import qualified Habits.Domain.AccessTokenSecret as ATS
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.AuthConfig.Class as ACC
import qualified Habits.Domain.TimeProvider as TP
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Refresh as Refresh
import qualified Habits.UseCases.Refresh.Live as RefreshLive
import Haskus.Utils.Variant.Excepts (evalE, catchE, catchLiftLeft, catchLiftBoth, catchLiftRight)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( Spec,
    describe,
    it, fdescribe,
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe
  )
import Utils (catchAllToFail, sampleIO, expectError)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

import qualified Data.Time as Time
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RTC
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import Habits.UseCases.Refresh.Class (Refresh(refresh))
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest(RefreshRequest))
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError(RefreshTokenIssuedNotFoundError))
import Habits.Domain.RefreshTokenHash (mkFromRefreshToken)
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse(..))
import Habits.Domain.AccessToken (AccessToken(..))
import Habits.Domain.RefreshToken (RefreshToken(..), mkRefreshToken)
import Veins.Test.HSpec.TH (shouldMatchPattern)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError(RefreshTokenInvalidError))
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError(RefreshTokenExpiredError))
import qualified Habits.Domain.AccessToken as AccessToken
import qualified Habits.Domain.RefreshToken as RefreshToken
import Control.Monad (void)
import Habits.Domain.RefreshTokenIssuedRepo (getByAccountId)

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
ac = pure $ CE.empty & CE.insert AC.AuthConfig{_accessTokenSecret = atSecret, _refreshTokenSecret = rtSecret}

tp :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[TP.TimeProvider m])
tp = pure $ CE.empty & CE.insert TP.TimeProvider {_getNow = pure timeNow }

envLayer :: forall m n. (MonadIO n, RTC.RefreshTokenIssuedRepo m, ARC.AccountRepo m, MonadIO m, ACC.AuthConfig m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer = ARM.mkAccountRepoMemory
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
addValidExpiredToken = S.do
  (rtiNew, accountId) <- S.coerce sampleIO
  let token = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds timeExpired)
  hash <- S.coerce $ mkFromRefreshToken token
  RTC.add (rtiNew{ _refreshTokenHash = hash, _accountId = accountId })
  S.pure (token, hash, accountId)

addValidToken :: _ => _
addValidToken = S.do
  (rtiNew, accountId) <- S.coerce sampleIO
  let token = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds timeNow)
  hash <- S.coerce $ mkFromRefreshToken token
  RTC.add (rtiNew{ _refreshTokenHash = hash, _accountId = accountId })
  S.pure (token, hash, accountId)

spec :: Spec
spec = describe "refresh should" $ do
  let runEval = runWithEnv (envLayer :: _) . evalE
  it "fail with RefreshTokenIssuedNotFoundError when refreshToken does not exist" . runEval . catchAllToFail $ S.do
    accountId <- S.coerce sampleIO
    let token = mkRefreshToken rtSecret accountId (utcTimeToPOSIXSeconds timeNow)
    refresh (RefreshRequest token)
    & expectError @RefreshTokenIssuedNotFoundError
  it "fail with RefreshTokenInvalidError when refreshToken was signed with a different secret" . runEval . catchAllToFail $ S.do
    token <- S.coerce sampleIO
    refresh (RefreshRequest token)
    & expectError @RefreshTokenInvalidError
  it "succeed with new AccessToken and RefreshToken when refreshToken is found" . runEval . catchAllToFail $ S.do
    (token, _, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    S.coerce $('resp `shouldMatchPattern` [p|RefreshResponse (AccessToken _) (RefreshToken _)|])
  it "return a valid access token" . runEval . catchAllToFail $ S.do
    (token, _, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    let verifyResult = AccessToken.verifyAccessToken atSecret (resp._accessToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return a valid refresh token" . runEval . catchAllToFail $ S.do
    (token,_, _) <- addValidToken
    resp <- refresh (RefreshRequest token)
    let verifyResult = RefreshToken.verifyRefreshToken rtSecret (resp._refreshToken)
    S.coerce $ verifyResult `shouldBe` True
  it "fail with RefreshTokenExpiredError when token is expired" . runEval . catchAllToFail $ S.do
    (token,_, _) <- addValidExpiredToken
    refresh (RefreshRequest token)
    & expectError @RefreshTokenExpiredError
  it "return a refreshToken which can be used to refresh successfully again" . runEval . catchAllToFail $ S.do
    (token, _, _) <- addValidToken
    resp1 <- refresh (RefreshRequest token)
    resp2 <- refresh (RefreshRequest resp1._refreshToken)
    S.coerce $('resp2 `shouldMatchPattern` [p|RefreshResponse (AccessToken _) (RefreshToken _)|])
  it "invalidate the used refreshToken which can not be used again afterwards" . runEval . catchAllToFail $ S.do
    (token, _, _) <- addValidToken
    refresh (RefreshRequest token)
    refresh (RefreshRequest token)
    & expectError @RefreshTokenIssuedNotFoundError
  it "invalidate all refreshTokens of a user when a valid, but non-existing refreshToken is used (token theft)" . runEval . catchAllToFail $ S.do
    (token, _, accountId) <- addValidToken
    refresh (RefreshRequest token)
    refresh (RefreshRequest token) & void & catchLiftRight (\RefreshTokenIssuedNotFoundError -> S.pure ())
    tokens <- getByAccountId accountId
    S.coerce $ tokens `shouldBe` []

