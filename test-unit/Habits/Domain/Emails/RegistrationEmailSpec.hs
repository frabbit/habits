module Habits.Domain.Emails.RegistrationEmailSpec where

import Habits.Test.Prelude
import Habits.Domain.Emails.RegistrationEmail (mkRegistrationEmail, getNonceFromRegistrationEmail)
import Test.Hspec (shouldBe)
import Habits.Domain.EmailMessage (EmailSender(..), EmailReceiver (EmailReceiver))
import Habits.Domain.Email (Email(..))

spec :: Spec
spec = describe "RegistrationEmail" $ do
  describe "getNonceFromRegistrationEmail" $ do
    it "should extract the nonce successfully after creation with mkRegistrationEmail" . property $ \(email, nonce) -> do
      let msg = mkRegistrationEmail email nonce
      getNonceFromRegistrationEmail msg.body `shouldBe` Just nonce
  describe "mkRegistrationEmail" $ do
    it "should use the given email as receiver" . property $ \(email, nonce) -> do
      let msg = mkRegistrationEmail email nonce
      msg.receiver `shouldBe` EmailReceiver email
    it "should use the right email as sender" . property $ \(email, nonce) -> do
      let msg = mkRegistrationEmail email nonce
      msg.sender `shouldBe` (EmailSender . Email $ "contact@habits.de")