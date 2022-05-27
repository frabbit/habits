module Habits.Web.Server where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Time as Time
import qualified Habits.Domain.AccessTokenSecret as ATS
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.TimeProvider as TP
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Login as L
import qualified Habits.UseCases.Login.Live as LL
import qualified Habits.UseCases.Register as R
import Habits.UseCases.Register.Class (Register)
import qualified Habits.UseCases.Register.Live as RL
import Habits.Web.Routes.RegisterRoute (RegisterApi, registerRoute)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Handler (Handler), HasServer (ServerT), ServerError, hoistServer, serve, type (:<|>) (..))
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import Habits.Web.Routes.LoginRoute (LoginApi, loginRoute)
import qualified Habits.UseCases.Login.Class as LC
import Control.Monad.Cont (MonadIO(liftIO))

type Env m = CE.MkSorted '[R.Register m, L.Login m, AR.AccountRepo m, RT.RefreshTokenIssuedRepo m]

atSecret :: _
atSecret = ATS.mkAccessTokenSecret "abcd"

rtSecret :: _
rtSecret = RTS.mkRefreshTokenSecret "abcde"

timeNow :: Time.UTCTime
timeNow = Time.UTCTime (Time.fromGregorian 2022 1 2) (Time.secondsToDiffTime 0)

ac :: forall n. (Monad n) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AC.AuthConfig])
ac = pure $ CE.empty & CE.insert AC.AuthConfig {AC._accessTokenSecret = atSecret, AC._refreshTokenSecret = rtSecret}

tp :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[TP.TimeProvider m])
tp = pure $ CE.empty & CE.insert TP.TimeProvider {TP._getNow = pure timeNow}

envLayer :: forall m n. (MonadIO n, MonadIO m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer =
  RTL.mkRefreshTokenIssuedRepoMemory
    `CE.provideAndChainLayerFlipped` ARM.mkAccountRepoMemory
    `CE.provideAndChainLayerFlipped` LL.mkLive
    `CE.provideAndChainLayerFlipped` RL.mkLive
    `CE.provideLayerFlipped` ac
    `CE.provideLayerFlipped` tp

AppTH.mkBoilerplate "runApp" ''Env

data ServerConfig = ServerConfig

server :: (Register m, LC.Login m, MonadIO m) => ServerT ServerApi (ExceptT ServerError m)
server = loginRoute :<|> registerRoute

type ServerApi = LoginApi :<|> RegisterApi

serverApi :: Proxy ServerApi
serverApi = Proxy

ntToHandler :: Env _ -> ExceptT ServerError _ a -> Handler a
ntToHandler env = Handler . hoist (runApp env)

mkApp :: (Monad n, MonadIO n) => ServerConfig -> n Application
mkApp _ = do
  env <- runReaderT envLayer CE.empty
  pure $ serve serverApi $ hoistServer serverApi (ntToHandler env) server

runServer :: ServerConfig -> IO ()
runServer cfg = do
  app <- liftIO $ mkApp cfg
  run 8081 app
