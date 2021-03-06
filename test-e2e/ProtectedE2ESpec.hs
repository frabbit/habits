module ProtectedE2ESpec (spec) where

import Prelude
import E2EUtils (runLogin, runRegister, withApp, runProtected, testConfig)
import Habits.Web.Routes.LoginRoute (LoginRequestDto (EmailPasswordLoginRequestDto), email, password)
import Test.Hspec (Spec, describe, it, shouldBe)
import Veins.Test.QuickCheck (propertyRuns)
import Servant.Client (ClientError(..), ResponseF (responseStatusCode))
import Network.HTTP.Types (Status(statusCode))
import Habits.Domain.AccessToken (mkAccessToken)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Habits.Domain.AccountId (AccountId(AccountId))

timePast :: POSIXTime
timePast = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2016 1 1) (secondsToDiffTime 0)

spec :: Spec
spec = describe "ProtectedE2E" $ do
  -- run only 5 tests. Logins are quite expensive because of the password check.
  it "should accept a valid token" . propertyRuns 1 $ \req -> withApp $ \port -> do
    Right rr <- runRegister port req
    Right r1 <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    r <- runProtected port r1.accessToken
    (.accountId) <$> r `shouldBe` Right rr.accountId
  it "should fail for invalid token" . propertyRuns 1 $ \req -> withApp $ \port -> do
    Right _ <- runRegister port req
    Right _ <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    Left (FailureResponse _ resp) <- runProtected port "abc"
    (statusCode . responseStatusCode $ resp) `shouldBe` 401
  it "should fail for expired tokens" . propertyRuns 1 $ \req -> withApp $ \port -> do
    Right rr <- runRegister port req
    let outdatedToken = mkAccessToken testConfig.accessTokenSecret (AccountId rr.accountId) timePast
    Right _ <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    Left (FailureResponse _ resp) <- runProtected port outdatedToken.unAccessToken
    (statusCode . responseStatusCode $ resp) `shouldBe` 401


