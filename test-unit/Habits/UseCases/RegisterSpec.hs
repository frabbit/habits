module Habits.UseCases.RegisterSpec where

import qualified Habits.UseCases.Register      as R

import qualified Habits.UseCases.Register.Class
                                               as RC

import           Habits.Domain.Email            ( Email(..) )

import           Data.Variant                   ( catchM )
import           Habits.App                ( App
                                                , runAppE
                                                )
import           Habits.AppEnv             ( AppEnv(..)
                                                , mkAppEnv
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import Habits.Domain.Password (Password(Password))


spec :: Spec
spec = describe "RegisterSpec execute should" $ do
  it "return with success" $ do
    env <- mkAppEnv
    res <- runAppE env catchedApp
    res `shouldBe` (R.RegisterResponse { R.success = True })
 where
  catchedApp = catchM
    (RC.execute
      (R.RegisterRequest { R.name = "Peter", R.email = Email "abc@de.de", R.password = Password "abc" })
    )
    mapError
  mapError (_ :: R.RegisterError) =
    pure (R.RegisterResponse { R.success = True })
