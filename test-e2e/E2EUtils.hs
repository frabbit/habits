module E2EUtils where

import Data.Proxy (Proxy (Proxy))
import Habits.Web.Routes.RegisterRoute (RegisterApi, RegisterRequestDto, RegisterResponseDto)
import Habits.Web.Server (ServerConfig (ServerConfig), mkApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (ClientEnv, ClientError, ClientM, baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Habits.Web.Routes.LoginRoute (LoginRequestDto, LoginResponseDto, LoginApi)

testConfig :: ServerConfig
testConfig = ServerConfig

-- testWithApplication makes sure the action is executed after the server has
-- started and is being properly shutdown.
withApp :: (Warp.Port -> IO ()) -> IO ()
withApp f = do
  app <- mkApp testConfig
  Warp.testWithApplication (pure app) f

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

login :: LoginRequestDto -> ClientM LoginResponseDto
login = client (Proxy :: Proxy LoginApi)

runLogin :: Int -> LoginRequestDto -> IO (Either ClientError LoginResponseDto)
runLogin port req = do
  env <- getClientTestEnv port
  runClientM (login req) env