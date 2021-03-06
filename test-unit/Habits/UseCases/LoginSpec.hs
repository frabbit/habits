{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.UseCases.LoginSpec (spec) where

import Habits.Test.Prelude

import Habits.Domain.AccessToken (AccessToken (AccessToken))
import qualified Habits.Domain.AccessToken as AccessToken
import qualified Habits.Domain.AccessTokenSecret as ATS
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Domain.AuthConfig as AC
import Habits.Domain.Password (Password (..))
import Habits.Domain.PasswordHash (mkFromPassword)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)
import Habits.Domain.RefreshToken (RefreshToken (RefreshToken))
import qualified Habits.Domain.RefreshToken as RefreshToken
import qualified Habits.Domain.Clock as Clock
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Login as Login
import qualified Habits.UseCases.Login.Class as LC
import qualified Habits.UseCases.Login.Live as LoginLive
import Habits.UseCases.Login.LoginRequest (LoginRequest (EmailPasswordLoginRequest))
import Habits.UseCases.Login.LoginResponse (LoginResponse (..))
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec.Expectations.Lifted (shouldBe)
import Utils (catchAllToFail, expectError, sampleIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

import System.TimeIt (timeIt)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Veins.Data.Time.Utils (addHoursToUTCTime, addMillisecondsToUTCTime, addDaysToUTCTime)
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RefreshTokenIssuedRepo
import qualified Habits.Domain.RefreshTokenHash as RefreshTokenHash
import Habits.Domain.EmailNotConfirmedError (EmailNotConfirmedError)

type Env m = CE.MkSorted '[AR.AccountRepo m, Login.Login m, RT.RefreshTokenIssuedRepo m]

atSecret :: _
atSecret = ATS.mkAccessTokenSecret "abcd"

rtSecret :: _
rtSecret = RTS.mkRefreshTokenSecret "abcde"

timeNow :: Time.UTCTime
timeNow = Time.UTCTime (Time.fromGregorian 2022 1 2) (Time.secondsToDiffTime 0)

envLayer :: forall m n. (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = LoginLive.mkLive
  <<-&& ARM.mkAccountRepoMemory
  <<-&& RTL.mkRefreshTokenIssuedRepoMemory
  <<- AC.mkAuthConfigStatic atSecret rtSecret
  <<- Clock.mkClockStatic timeNow

AppTH.mkBoilerplate "runApp" ''Env

addUserWithPassword :: _ => _
addUserWithPassword = S.do
  pw <- S.coerce sampleIO
  pwHash <- S.coerce $ mkFromPassword pw
  acc <- S.coerce $ sampleIO <&> \a -> a{password = pwHash, emailConfirmed = True}
  id <- ARC.add acc
  S.pure (acc, pw, id)

addUserWithUnconfirmedEmail :: _ => _
addUserWithUnconfirmedEmail = S.do
  pw <- S.coerce sampleIO
  pwHash <- S.coerce $ mkFromPassword pw
  acc <- S.coerce $ sampleIO <&> \a -> a{password = pwHash, emailConfirmed = False}
  id <- ARC.add acc
  S.pure (acc, pw, id)

runWithEnv :: _ -> _ b -> IO b
runWithEnv layer app = do
  env <- runReaderT layer CE.empty
  runApp env app

embed :: _ => _
embed = runWithEnv (envLayer :: _) . evalE . catchAllToFail

spec :: Spec
spec = describe "Login.execute should" $ do
  it "be successfull when account with same password and email exists" . embed $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.login $ EmailPasswordLoginRequest acc.email pw
    S.coerce $ $('resp `shouldMatchPattern` [p|LoginResponse (AccessToken _) (RefreshToken _)|])
  it "return a valid access token" . embed $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- timeIt $ LC.login $ EmailPasswordLoginRequest acc.email pw
    let verifyResult = AccessToken.verifyAccessToken atSecret (resp.accessToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return a valid refresh token" . embed $ S.do
    (acc, pw,_) <- addUserWithPassword
    resp <- LC.login $ EmailPasswordLoginRequest acc.email pw
    let verifyResult = RefreshToken.verifyRefreshToken rtSecret (resp.refreshToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return an access token which is 3 hours valid" . embed $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.login $ EmailPasswordLoginRequest acc.email pw
    let now = timeNow
    let almost = addHoursToUTCTime 3 timeNow
    let expired = addMillisecondsToUTCTime 1 almost
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds now) `shouldBe` False
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds almost) `shouldBe` False
    S.coerce $ AccessToken.isExpired atSecret resp.accessToken (utcTimeToPOSIXSeconds expired) `shouldBe` True
  it "return a refresh token which is 7 days valid" . embed $ S.do
    (acc, pw, _) <- addUserWithPassword
    resp <- LC.login $ EmailPasswordLoginRequest acc.email pw
    let now = timeNow
    let almost = addDaysToUTCTime 7 timeNow
    let expired = addMillisecondsToUTCTime 1 almost
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds now) `shouldBe` False
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds almost) `shouldBe` False
    S.coerce $ RefreshToken.isExpired rtSecret resp.refreshToken (utcTimeToPOSIXSeconds expired) `shouldBe` True
  it "store the hash of the generated refresh token" . embed $ S.do
    (acc, pw, id) <- addUserWithPassword
    resp <- LC.login $ EmailPasswordLoginRequest acc.email pw
    [info] <- RefreshTokenIssuedRepo.getByAccountId id
    S.coerce $ RefreshTokenHash.isValid resp.refreshToken info.refreshTokenHash `shouldBe` True

  it "fail with AccountNotFoundError when account with given email does not exist" . embed $
    S.do
      pw <- S.coerce sampleIO
      email <- S.coerce sampleIO
      LC.login $ EmailPasswordLoginRequest email pw
      & expectError @AccountNotFoundError
  xit "fail with EmailNotConfirmedError when account has unconfirmed email" . embed $
    S.do
      (acc, pw, _) <- addUserWithUnconfirmedEmail
      LC.login $ EmailPasswordLoginRequest acc.email pw
      & expectError @EmailNotConfirmedError
  it "fail with PasswordIncorrectError when account with email exists but password is wrong" . embed $
    S.do
      let pw = Password "InvalidPassword"
      acc <- S.coerce sampleIO
      ARC.add acc
      LC.login $ EmailPasswordLoginRequest acc.email pw
      & expectError @PasswordIncorrectError
