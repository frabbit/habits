module E2EUtils where

import Prelude
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Habits.Web.Routes.LoginRoute (LoginApi, LoginRequestDto, LoginResponseDto)
import Habits.Web.Routes.ProtectedRoute (ProtectedApi, ProtectedResponseDto)
import Habits.Web.Routes.RegisterRoute (RegisterApi, RegisterRequestDto, RegisterResponseDto)
import Habits.Web.Server (ServerConfig (ServerConfig), mkApp, refreshTokenSecret, accessTokenSecret, EmailServiceConfig (ESCMemory), emailServiceConfig)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (AuthProtect)
import Servant.Client (ClientEnv, ClientError, ClientM, baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, mkAuthenticatedRequest, addHeader, Request)
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.AccessTokenSecret as ATS
import Habits.Web.Routes.RefreshRoute (RefreshRequestDto, RefreshResponseDto, RefreshApi)

type instance AuthClientData (AuthProtect "JWT") = Text


testConfig :: ServerConfig
testConfig = ServerConfig { refreshTokenSecret = RTS.mkRefreshTokenSecret "abcde",accessTokenSecret = ATS.mkAccessTokenSecret "abc", emailServiceConfig = ESCMemory }


authenticateRequest :: Text -> Request -> Request
authenticateRequest token = addHeader "Authorization" ("Bearer " <> token)

authenticated :: Text -> (AuthenticatedRequest (AuthProtect "JWT") -> a) -> a
authenticated token f = f (mkAuthenticatedRequest token authenticateRequest)

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

refresh :: RefreshRequestDto -> ClientM RefreshResponseDto
refresh = client (Proxy :: Proxy RefreshApi)

runRefresh :: Int -> RefreshRequestDto -> IO (Either ClientError RefreshResponseDto)
runRefresh port req = do
  env <- getClientTestEnv port
  runClientM (refresh req) env

protected :: AuthenticatedRequest (AuthProtect "JWT") -> ClientM ProtectedResponseDto
protected = client (Proxy :: Proxy ProtectedApi)

runProtected :: Int -> Text -> IO (Either ClientError ProtectedResponseDto)
runProtected port token = do
  env <- getClientTestEnv port
  runClientM (authenticated token protected) env