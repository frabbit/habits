module RegisterE2ESpec (spec) where

import Test.Hspec (Spec, describe, it)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import Habits.Web.Server (app, ServerConfig (ServerConfig))
import Habits.Web.Routes.RegisterRoute (RegisterApi, RegisterResponseDto (RegisterResponseDto))
import Data.Proxy (Proxy(Proxy))
import Servant.Client (client, baseUrlPort, parseBaseUrl, mkClientEnv, runClientM)
import Veins.Test.HSpec.TH (ShouldMatchPattern(shouldMatchPattern))
import Veins.Test.QuickCheck (sampleIO)
import Debug.Trace (traceShowM)
import qualified Data.Text as Text

testConfig :: ServerConfig
testConfig = ServerConfig


-- testWithApplication makes sure the action is executed after the server has
-- started and is being properly shutdown.
withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication (pure $ app testConfig)


spec :: Spec
spec = describe "RegisterE2E" $ do
  it "should provide a register route" $ do
    withApp $ \port -> do
      baseUrl <- parseBaseUrl "http://localhost"
      manager <- newManager defaultManagerSettings
      let clientEnv p = mkClientEnv manager (baseUrl { baseUrlPort = p })
      let register = client (Proxy :: Proxy RegisterApi)
      x <- sampleIO
      result <- runClientM (register x) (clientEnv port)
      traceShowM result
      $('result `shouldMatchPattern` [p|Right (RegisterResponseDto (not . Text.null -> True))|])

