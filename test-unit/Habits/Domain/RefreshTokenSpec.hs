module Habits.Domain.RefreshTokenSpec where
import Test.Hspec (Spec, it, describe)
import Habits.Domain.RefreshToken (mkRefreshToken, verifyRefreshToken, isExpired, getAccountId)
import Veins.Test.QuickCheck (sampleIO)
import Data.Time (UTCTime(UTCTime), fromGregorian, secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret(RefreshTokenSecret))



timeNow :: POSIXTime
timeNow = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2022 1 1) (secondsToDiffTime 0)

timePast :: POSIXTime
timePast = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2021 12 31) (secondsToDiffTime 0)
timeFuture :: POSIXTime
timeFuture = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2022 1 2) (secondsToDiffTime 0)

coerceIO :: IO a -> IO a
coerceIO = id

spec :: Spec
spec = describe "RefreshToken" $ do
  describe "verifyRefreshToken" $ do
    it "should return true if token was signed with the given secret" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let token = mkRefreshToken secret accountId timeNow
      verifyRefreshToken secret token `shouldBe` True
    it "should return false if token was not signed with the given secret" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let token = mkRefreshToken secret accountId timeNow
      verifyRefreshToken (RefreshTokenSecret "foo") token `shouldBe` False

  describe "isExpired should" $ do
    it "return False when the token expiration time is  based on the given current time" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let expiresAt = timeNow
      let token = mkRefreshToken secret accountId expiresAt

      isExpired secret token timeFuture `shouldBe` True
      isExpired secret token timePast `shouldBe` False
    it "return False when the token is expired based on the given current time" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let expiresAt = timeNow
      let token = mkRefreshToken secret accountId expiresAt

      isExpired secret token timeFuture `shouldBe` True
      isExpired secret token timePast `shouldBe` False
  describe "getAccountId should" $ do
    it "return the accountId which was used at creation time" . coerceIO $ do
      (secret, accountId) <- sampleIO
      let expiresAt = timeNow
      let token = mkRefreshToken secret accountId expiresAt

      getAccountId  secret token  `shouldBe` Just accountId
