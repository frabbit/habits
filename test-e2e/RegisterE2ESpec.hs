module RegisterE2ESpec (spec) where

import Habits.Domain.AccountId (parseAccountId)
import Habits.Web.Routes.RegisterRoute (RegisterResponseDto (RegisterResponseDto))
import Test.Hspec (Spec, describe, it)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import Veins.Test.QuickCheck (sampleIO)
import E2EUtils (withApp, runRegister)

spec :: Spec
spec = describe "RegisterE2E" $ do
  it "should provide a register route" . withApp $ \port -> do
    req <- sampleIO
    result <- runRegister port req
    $('result `shouldMatchPattern` [p|Right (RegisterResponseDto (parseAccountId -> Just _))|])
