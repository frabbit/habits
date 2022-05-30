module LoginE2ESpec (spec) where

import E2EUtils (runLogin, runRegister, withApp)
import Habits.Web.Routes.LoginRoute (LoginRequestDto (EmailPasswordLoginRequestDto), LoginResponseDto (LoginResponseDto), email, password)
import Test.Hspec (Spec, describe, it)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import Veins.Test.QuickCheck (propertyRuns)

spec :: Spec
spec = describe "LoginE2E" $ do
  it "should provide a login route" . propertyRuns 5 $ \req -> withApp $ \port -> do
    runRegister port req
    result <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    $('result `shouldMatchPattern` [p|Right (LoginResponseDto {})|])
