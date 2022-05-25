module Habits.Domain.EmailSpec where

import Prelude
import Test.Hspec (Spec, describe, it, fit)
import Test.QuickCheck (Testable(property))
import Habits.Domain.Email (parseEmail, unEmail, emailFromText)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import qualified Veins.Data.Codec as Codec
import Data.Validation (toEither)
import Data.Either (isRight)

coerceIO :: IO a -> IO a
coerceIO = id

spec :: Spec
spec = describe "EmailSpec" $ do
  describe "every generated arbitrary" $ do
    it "should pass the parsing test" . property $ \email -> coerceIO $ do
      (parseEmail . unEmail $ email) `shouldSatisfy` isJust
    it "should pass the codec test" . property $ \email -> coerceIO $ do
      (toEither . Codec.encode emailFromText . unEmail $ email) `shouldSatisfy` isRight