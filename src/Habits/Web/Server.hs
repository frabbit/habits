module Habits.Web.Server where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve, hoistServer, ServerError, Handler (Handler), HasServer (ServerT))
import Data.Proxy (Proxy (Proxy))
import Habits.Web.Routes.RegisterRoute (RegisterApi, registerRoute)
import Control.Monad.Except (ExceptT)
import Habits.UseCases.Register.Class (Register)
import Control.Monad.IO.Class (MonadIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.UseCases.Register as R
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Habits.UseCases.Register.Live as RL
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Veins.Test.AppTH as AppTH
import Control.Monad.Morph (hoist)

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall m n. (MonadIO n, ARC.AccountRepo m, MonadIO m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer = ARM.mkAccountRepoMemory `CE.provideAndChainLayerFlipped` RL.mkLive

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ b -> IO b
runWithEnv app' = do
  env <- runReaderT envLayer CE.empty
  runApp env app'

data ServerConfig = ServerConfig

server :: (Register m, MonadIO m) => ServerT RegisterApi (ExceptT ServerError m)
server = registerRoute

registerApi :: Proxy RegisterApi
registerApi = Proxy

ntToHandler :: ExceptT ServerError _ a -> Handler a
ntToHandler = Handler . hoist runWithEnv

app :: Application
app = serve registerApi $ hoistServer registerApi ntToHandler server

runServer :: ServerConfig -> IO ()
runServer _ = run 8081 app
