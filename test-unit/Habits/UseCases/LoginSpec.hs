{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.UseCases.LoginSpec (spec) where

import Prelude hiding (id)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Functor ((<&>))
import Habits.Domain.AccessToken (AccessToken (AccessToken))
import qualified Habits.Domain.AccessToken as AccessToken
import qualified Habits.Domain.AccessTokenSecret as ATS
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.AuthConfig.Class as ACC
import Habits.Domain.Password (Password (..))
import Habits.Domain.PasswordHash (mkFromPassword)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)
import Habits.Domain.RefreshToken (RefreshToken (RefreshToken))
import qualified Habits.Domain.RefreshToken as RefreshToken
import qualified Habits.Domain.TimeProvider as TP
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Login as Login
import qualified Habits.UseCases.Login.Class as LC
import qualified Habits.UseCases.Login.Live as LoginLive
import Habits.UseCases.Login.LoginRequest (LoginRequest (EmailPasswordLoginRequest))
import Habits.UseCases.Login.LoginResponse (LoginResponse (..))
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL
import Haskus.Utils.Variant.Excepts (evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Lifted (shouldBe)
import Utils (catchAllToFail, expectError, sampleIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.HSpec.TH (shouldMatchPattern)

import System.TimeIt (timeIt)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Veins.Data.Time.Utils (addHoursToUTCTime, addMillisecondsToUTCTime, addDaysToUTCTime)
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RTC
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RefreshTokenIssuedRepo
import qualified Habits.Domain.RefreshTokenHash as RefreshTokenHash

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m, Login.Login m, RT.RefreshTokenIssuedRepo m, AC.AuthConfig]


atSecret :: _
atSecret = ATS.mkAccessTokenSecret "abcd"

rtSecret :: _
rtSecret = RTS.mkRefreshTokenSecret "abcde"

timeNow :: Time.UTCTime
timeNow = Time.UTCTime (Time.fromGregorian 2022 1 2) (Time.secondsToDiffTime 0)

ac :: forall n. (Monad n) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AC.AuthConfig])
ac = pure $ CE.empty & CE.insert AC.AuthConfig {AC._accessTokenSecret = atSecret, AC._refreshTokenSecret = rtSecret}

tp :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[TP.TimeProvider m])
tp = pure $ CE.empty & CE.insert TP.TimeProvider {TP._getNow = pure timeNow }

envLayer :: forall m n. (MonadIO n, RTC.RefreshTokenIssuedRepo m, ARC.AccountRepo m, MonadIO m, ACC.AuthConfig m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer = ARM.mkAccountRepoMemory
  `CE.provideAndChainLayerFlipped` RTL.mkRefreshTokenIssuedRepoMemory
  `CE.provideAndChainLayerFlipped` RL.mkLive
  `CE.provideAndChainLayerFlipped` LoginLive.mkLive
  `CE.provideAndChainLayerFlipped` ac
  `CE.provideLayerFlipped` tp

AppTH.mkBoilerplate "runApp" ''Env

addUserWithPassword :: _ => _
addUserWithPassword = S.do
  pw <- S.coerce sampleIO
  pwHash <- S.coerce $ mkFromPassword pw
  acc <- S.coerce $ sampleIO <&> \a -> a{ password = pwHash }
  id <- AR.add acc
  S.pure (acc, pw, id)

runWithEnv :: _ -> _ b -> IO b
runWithEnv layer app = do
  env <- runReaderT layer CE.empty
  runApp env app

spec :: Spec
spec = describe "Login.execute should" $ do
  let runEval = runWithEnv envLayer . evalE
  it "be successfull when account with same password and email exists" . runEval . catchAllToFail $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest acc.email pw
    S.coerce $ $('resp `shouldMatchPattern` [p|LoginResponse (AccessToken _) (RefreshToken _)|])
  it "return a valid access token" . runEval . catchAllToFail $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- timeIt $ LC.execute $ EmailPasswordLoginRequest acc.email pw
    let verifyResult = AccessToken.verifyAccessToken atSecret (resp.accessToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return a valid refresh token" . runEval . catchAllToFail $ S.do
    (acc, pw,_) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest acc.email pw
    let verifyResult = RefreshToken.verifyRefreshToken rtSecret (resp.refreshToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return an access token which is 3 hours valid" . runEval . catchAllToFail $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest acc.email pw
    let now = timeNow
    let almost = addHoursToUTCTime 3 timeNow
    let expired = addMillisecondsToUTCTime 1 almost
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds now) `shouldBe` False
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds almost) `shouldBe` False
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds expired) `shouldBe` True
  it "return a refresh token which is 7 days valid" . runEval . catchAllToFail $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest acc.email pw
    let now = timeNow
    let almost = addDaysToUTCTime 7 timeNow
    let expired = addMillisecondsToUTCTime 1 almost
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds now) `shouldBe` False
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds almost) `shouldBe` False
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds expired) `shouldBe` True
  it "store the hash of the generated refresh token" . runEval . catchAllToFail $ S.do
    (acc, pw, id) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest acc.email pw
    [info] <- RefreshTokenIssuedRepo.getByAccountId id
    S.coerce $ RefreshTokenHash.isValid resp.refreshToken info.refreshTokenHash `shouldBe` True

  it "fail with AccountNotFoundError when account with given email does not exist" . runEval . catchAllToFail $
    S.do
      pw <- S.coerce sampleIO
      email <- S.coerce sampleIO
      LC.execute $ EmailPasswordLoginRequest email pw
      & expectError @AccountNotFoundError
  it "fail with PasswordIncorrectError when account with email exists but password is wrong" . runEval . catchAllToFail $
    S.do
      let pw = Password "InvalidPassword"
      acc <- S.coerce sampleIO
      AR.add acc
      LC.execute $ EmailPasswordLoginRequest acc.email pw
      & expectError @PasswordIncorrectError
