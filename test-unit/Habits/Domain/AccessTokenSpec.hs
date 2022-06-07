module Habits.Domain.AccessTokenSpec (spec) where

import Habits.Test.Prelude
import Habits.Domain.AccessToken (mkAccessToken, verifyAccessToken, isExpired)
import Veins.Test.QuickCheck (sampleIO)
import Data.Time (UTCTime(UTCTime), fromGregorian, secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Habits.Domain.AccessTokenSecret (AccessTokenSecret(AccessTokenSecret))

timeNow :: POSIXTime
timeNow = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2022 1 1) (secondsToDiffTime 0)

timePast :: POSIXTime
timePast = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2021 12 31) (secondsToDiffTime 0)
timeFuture :: POSIXTime
timeFuture = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2022 1 2) (secondsToDiffTime 0)

coerceIO :: IO a -> IO a
coerceIO = identity

spec :: Spec
spec = describe "AccessToken" $ do
  describe "verifyAccessToken should" $ do
    it "return true if token was signed with the given secret" . property $ \(secret, accountId) -> coerceIO $ do
      let token = mkAccessToken secret accountId timeNow
      verifyAccessToken secret token `shouldBe` True
    it "return false if token was not signed with the given secret" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let token = mkAccessToken secret accountId timeNow
      verifyAccessToken (AccessTokenSecret "foo") token `shouldBe` False

  describe "isExpired should" $ do
    it "return False when the token expiration time is  based on the given current time" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let expiresAt = timeNow
      let token = mkAccessToken secret accountId expiresAt

      isExpired secret token timeFuture `shouldBe` True
      isExpired secret token timePast `shouldBe` False
    it "return False when the token is expired based on the given current time" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let expiresAt = timeNow
      let token = mkAccessToken secret accountId expiresAt

      isExpired secret token timeFuture `shouldBe` True
      isExpired secret token timePast `shouldBe` False
