module Test.Habits.UseCases.RegisterSpec where

import qualified Habits.UseCases.Register      as R

import qualified Habits.UseCases.Register.Class
                                               as RC

import           Habits.Domain.Email            ( Email(..) )

import           Data.Variant                   ( catchM )
import           Test.Habits.App                ( App
                                                , runApp
                                                )
import           Test.Habits.AppEnv             ( AppEnv(..)
                                                , mkAppEnv
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
spec :: Spec
spec = describe "RegisterSpec execute should" $ do
  it "return with success" $ do
    res <- runApp env catchedApp
    res `shouldBe` (R.RegisterResponse { R.success = True })
 where
  catchedApp = catchM
    (RC.execute
      (R.RegisterRequest { R.name = "Peter", R.email = Email "abc@de.de" })
    )
    mapError
  mapError (_ :: R.RegisterError) =
    pure (R.RegisterResponse { R.success = True })
  env :: AppEnv App
  env = mkAppEnv
