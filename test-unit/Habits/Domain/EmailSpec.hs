module Habits.Domain.EmailSpec where

import Habits.Test.Prelude
import Habits.Domain.Email (parseEmail, unEmail, emailFromText)
import Test.Hspec.Expectations.Lifted (shouldSatisfy)
import qualified Veins.Data.Codec as Codec
import Data.Validation (toEither)

coerceIO :: IO a -> IO a
coerceIO = identity

spec :: Spec
spec = describe "EmailSpec" $ do
  describe "every generated arbitrary" $ do
    it "should pass the parsing test" . property $ \email -> coerceIO $ do
      (parseEmail . unEmail $ email) `shouldSatisfy` isJust
    it "should pass the codec test" . property $ \email -> coerceIO $ do
      (toEither . Codec.encode emailFromText . unEmail $ email) `shouldSatisfy` isRight