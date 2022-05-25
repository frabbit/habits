module Habits.Domain.PasswordSpec where

import Prelude
import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Testable(property))
import Test.Hspec.Expectations.Lifted (shouldSatisfy)
import Data.Maybe (isJust)
import Habits.Domain.Password (Password(unPassword), parsePassword, passwordFromText)
import qualified Veins.Data.Codec as Codec
import Data.Either (isRight)
import Data.Validation (toEither)

coerceIO :: IO a -> IO a
coerceIO = id

spec :: Spec
spec = describe "PasswordSpec" $ do
  describe "every generated arbitrary" $ do
    it "should pass the parsing test" . property $ \pw -> coerceIO $ do
      (parsePassword . unPassword $ pw) `shouldSatisfy` isJust
    it "should pass the codec test" . property $ \pw -> coerceIO $ do
      (toEither . Codec.encode passwordFromText . unPassword $ pw) `shouldSatisfy` isRight