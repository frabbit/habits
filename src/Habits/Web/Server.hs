{-# LANGUAGE MultiWayIf #-}

module Habits.Web.Server where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Time as Time
import qualified Habits.Domain.AccessTokenSecret as ATS
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.Clock as Clock
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTL
import qualified Habits.UseCases.Login as L
import qualified Habits.UseCases.Login.Class as LC
import qualified Habits.UseCases.Login.Live as LL
import qualified Habits.UseCases.Register as R
import Habits.UseCases.Register.Class (Register)
import qualified Habits.UseCases.Register.Live as RL
import Habits.Web.Auth (accountId, parseAuthenticatedAccount, JWTAuthHandler)
import Habits.Web.Routes.LoginRoute (LoginApi, loginRoute)
import Habits.Web.Routes.ProtectedRoute (ProtectedApi, protectedRoute)
import Habits.Web.Routes.RegisterRoute (RegisterApi, registerRoute)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Context (EmptyContext, (:.)), Handler (Handler), HasServer (ServerT, hoistServerWithContext), ServerError, err401, errBody, serveWithContext, type (:<|>) (..))
import qualified Servant.Server.Experimental.Auth as ServantAuth
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[R.Register m, L.Login m, AR.AccountRepo m, RT.RefreshTokenIssuedRepo m]

atSecret :: _
atSecret = ATS.mkAccessTokenSecret "abcd"

rtSecret :: _
rtSecret = RTS.mkRefreshTokenSecret "abcde"

timeNow :: Time.UTCTime
timeNow = Time.UTCTime (Time.fromGregorian 2022 1 2) (Time.secondsToDiffTime 0)

ac :: forall n. (Monad n) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AC.AuthConfig])
ac = pure $ CE.empty & CE.insert AC.AuthConfig {AC._accessTokenSecret = atSecret, AC._refreshTokenSecret = rtSecret}

tp :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[Clock.Clock m])
tp = pure $ CE.empty & CE.insert Clock.Clock {Clock._getNow = pure timeNow}

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
server = protectedRoute :<|> loginRoute :<|> registerRoute

type ServerApi = ProtectedApi :<|> LoginApi :<|> RegisterApi

serverApi :: Proxy ServerApi
serverApi = Proxy

ntToHandler :: Env _ -> ExceptT ServerError _ a -> Handler a
ntToHandler env = Handler . hoist (runApp env)

mkAuthHandler :: ServerConfig -> JWTAuthHandler
mkAuthHandler _ = ServantAuth.mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = Handler $ do
      case parseAuthenticatedAccount timeNow atSecret req of
        Left _ -> throw401 "Token invalid"
        Right a -> ExceptT $ pure $ Right a

mkApp :: (Monad n, MonadIO n, _) => ServerConfig -> n Application
mkApp cfg = do
  env <- runReaderT envLayer CE.empty
  let
    context :: Context '[JWTAuthHandler]
    context = mkAuthHandler cfg :. EmptyContext
    contextProxy :: Proxy '[JWTAuthHandler]
    contextProxy = Proxy
    api = hoistServerWithContext serverApi contextProxy (ntToHandler env) server
    app :: Application
    app = serveWithContext serverApi context api
  pure app

runServer :: ServerConfig -> IO ()
runServer cfg = do
  app <- liftIO $ mkApp cfg
  run 8081 app
