module RegisterE2ESpec (spec) where

import Data.Proxy (Proxy (Proxy))
import Debug.Trace (traceShowM)
import Habits.Domain.AccountId (parseAccountId)
import Habits.Web.Routes.RegisterRoute (RegisterApi, RegisterRequestDto, RegisterResponseDto (RegisterResponseDto))
import Habits.Web.Server (ServerConfig (ServerConfig), app)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (ClientEnv, ClientError, ClientM, baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Test.Hspec (Spec, describe, it)
import Veins.Test.HSpec.TH (ShouldMatchPattern (shouldMatchPattern))
import Veins.Test.QuickCheck (sampleIO)

testConfig :: ServerConfig
testConfig = ServerConfig

-- testWithApplication makes sure the action is executed after the server has
-- started and is being properly shutdown.
withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication (pure $ app testConfig)

getClientTestEnv :: Int -> IO ClientEnv
getClientTestEnv port = do
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager (baseUrl {baseUrlPort = port})

register :: RegisterRequestDto -> ClientM RegisterResponseDto
register = client (Proxy :: Proxy RegisterApi)

runRegister :: Int -> RegisterRequestDto -> IO (Either ClientError RegisterResponseDto)
runRegister port req = do
  env <- getClientTestEnv port
  runClientM (register req) env

spec :: Spec
spec = describe "RegisterE2E" $ do
  it "should provide a register route" . withApp $ \port -> do
    req <- sampleIO
    result <- runRegister port req
    $('result `shouldMatchPattern` [p|Right (RegisterResponseDto (parseAccountId -> Just _))|])
