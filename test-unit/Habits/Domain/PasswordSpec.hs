module Habits.Domain.PasswordSpec where

import Habits.Test.Prelude
import Test.Hspec.Expectations.Lifted (shouldSatisfy)
import Habits.Domain.Password (Password(unPassword), parsePassword, passwordFromText)
import qualified Veins.Data.Codec as Codec
import Data.Validation (toEither)

coerceIO :: IO a -> IO a
coerceIO = identity

spec :: Spec
spec = describe "PasswordSpec" $ do
  describe "every generated arbitrary" $ do
    it "should pass the parsing test" . property $ \pw -> coerceIO $ do
      (parsePassword . unPassword $ pw) `shouldSatisfy` isJust
    it "should pass the codec test" . property $ \pw -> coerceIO $ do
      (toEither . Codec.encode passwordFromText . unPassword $ pw) `shouldSatisfy` isRight