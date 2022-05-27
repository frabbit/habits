module LoginE2ESpec (spec) where

import Test.Hspec (Spec, describe, it)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import Veins.Test.QuickCheck (sampleIO)
import E2EUtils (withApp, runRegister, runLogin)
import Habits.Web.Routes.LoginRoute (LoginRequestDto(EmailPasswordLoginRequestDto), email, password, LoginResponseDto (LoginResponseDto))

spec :: Spec
spec = describe "LoginE2E" $ do
  it "should provide a login route" . withApp $ \port -> do
    req <- sampleIO
    runRegister port req
    result <- runLogin port $ EmailPasswordLoginRequestDto { email=req.email, password=req.password}
    $('result `shouldMatchPattern` [p|Right (LoginResponseDto {})|])
