module RefreshE2ESpec (spec) where

import E2EUtils (runLogin, runRegister, withApp, runRefresh)
import Habits.Web.Routes.LoginRoute (LoginRequestDto (EmailPasswordLoginRequestDto), LoginResponseDto (LoginResponseDto), email, password, refreshToken)
import Test.Hspec (Spec, it, fdescribe)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import Veins.Test.QuickCheck (propertyOne)
import Habits.Web.Routes.RefreshRoute (RefreshRequestDto(RefreshRequestDto, refreshToken), RefreshResponseDto (RefreshResponseDto))


spec :: Spec
spec = fdescribe "RefreshE2E" $ do
  it "should provide a refresh route" . propertyOne $ \req -> withApp $ \port -> do
    runRegister port req
    Right (LoginResponseDto { refreshToken }) <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    result <- runRefresh port $ RefreshRequestDto { refreshToken }
    $('result `shouldMatchPattern` [p|Right (RefreshResponseDto {})|])
