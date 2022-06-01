module RegisterE2ESpec (spec) where

import Prelude
import Habits.Domain.AccountId (parseAccountId)
import Habits.Web.Routes.RegisterRoute (RegisterResponseDto (RegisterResponseDto))
import Test.Hspec (Spec, describe, it)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import E2EUtils (withApp, runRegister)
import Test.QuickCheck (property)

spec :: Spec
spec = describe "RegisterE2E" $ do
  it "should provide a register route" . property $ \req -> withApp $ \port -> do
    result <- runRegister port req
    $('result `shouldMatchPattern` [p|Right (RegisterResponseDto (parseAccountId -> Just _))|])
