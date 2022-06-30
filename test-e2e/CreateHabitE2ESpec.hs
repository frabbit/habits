module CreateHabitE2ESpec (spec) where

import Habits.Test.Prelude
import E2EUtils (runLogin, runRegister, withApp, runCreateHabit)
import Habits.Web.Routes.LoginRoute (LoginRequestDto (EmailPasswordLoginRequestDto), email, password)
import Veins.Test.QuickCheck (propertyRuns)
import Habits.Web.Routes.CreateHabitRoute (CreateHabitResponseDto (CreateHabitResponseDto))
import Habits.Domain.HabitId (parseHabitId)

spec :: Spec
spec = describe "CreateHabitE2E" $ do
  -- run only 5 tests. Logins are quite expensive because of the password check.
  fit "should return a successful response" . propertyRuns 2 $ \(req, ch) -> withApp $ \port -> do
    Right rr <- runRegister port req
    Right r1 <- runLogin port $ EmailPasswordLoginRequestDto {email = req.email, password = req.password}
    result <- runCreateHabit port r1.accessToken ch{accountId = rr.accountId}
    $('result `shouldMatchPattern` [p|Right (CreateHabitResponseDto (parseHabitId -> Just _))|])




