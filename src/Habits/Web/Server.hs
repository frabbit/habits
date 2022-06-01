{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.Web.Server where

import Habits.Prelude
import Control.Monad.Morph (hoist)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AuthConfig as AC
import qualified Habits.Domain.Clock as Clock
import qualified Habits.Domain.RefreshTokenIssuedRepo as RT
import qualified Habits.UseCases.Login as L
import qualified Habits.UseCases.Login.Live as LL
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL
import Habits.Web.Auth (JWTAuthHandler, parseAuthenticatedAccount)
import Habits.Web.Routes.LoginRoute (LoginApi, loginRoute)
import Habits.Web.Routes.ProtectedRoute (ProtectedApi, protectedRoute)
import Habits.Web.Routes.RegisterRoute (RegisterApi, registerRoute)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Context (EmptyContext, (:.)), Handler (Handler), HasServer (ServerT, hoistServerWithContext), ServerError, err401, errBody, serveWithContext, type (:<|>) (..))
import qualified Servant.Server.Experimental.Auth as ServantAuth
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.Clock (mkClockLive)
import Habits.Domain.RefreshTokenSecret
import Habits.Domain.AccessTokenSecret
import qualified Habits.UseCases.Refresh as Refresh
import qualified Habits.UseCases.Refresh.Live as RefreshLive
import Habits.Web.Routes.RefreshRoute (RefreshApi, refreshRoute)
import Habits.UseCases.Register.Class (RegisterM)
import Habits.UseCases.Login.Class (LoginM)
import Habits.UseCases.Refresh.Class (RefreshM)
import Habits.Infra.Memory.RefreshTokenIssuedRepoMemory (mkRefreshTokenIssuedRepoMemory)
import Habits.Infra.Memory.AccountRepoMemory (mkAccountRepoMemory)
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[Refresh.Refresh m, R.Register m, L.Login m, AR.AccountRepo m, RT.RefreshTokenIssuedRepo m, Clock.Clock m, AC.AuthConfig]

data ServerConfig = ServerConfig {refreshTokenSecret :: RefreshTokenSecret, accessTokenSecret :: AccessTokenSecret }

{- HLINT ignore envLayer "Redundant bracket" -}
envLayer :: forall m n. (MonadIO n, MonadIO m) => ServerConfig -> ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer cfg =
  LL.mkLive
    CE.<<-&& RL.mkLive
    CE.<<-&& RefreshLive.mkLive
    CE.<<-&& mkRefreshTokenIssuedRepoMemory
    CE.<<-&& mkAccountRepoMemory
    CE.<<-&& AC.mkAuthConfigStatic cfg.accessTokenSecret cfg.refreshTokenSecret
    CE.<<-&& mkClockLive



server :: (RefreshM m, RegisterM m, LoginM m, MonadIO m) => ServerT ServerApi (ExceptT ServerError m)
server = refreshRoute :<|> protectedRoute :<|> loginRoute :<|> registerRoute

type ServerApi = RefreshApi :<|> ProtectedApi :<|> LoginApi :<|> RegisterApi

serverApi :: Proxy ServerApi
serverApi = Proxy

AppTH.mkBoilerplateForName "App" ''Env

ntToHandler :: Env App -> ExceptT ServerError App a -> Handler a
ntToHandler env = Handler . hoist (runApp env)

mkAuthHandler :: Env App -> JWTAuthHandler
mkAuthHandler env = ServantAuth.mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = Handler $ do
      now <- liftIO $ runApp env Clock.getNow
      atSecret <- liftIO $ runApp env AC.getAccessTokenSecret
      case parseAuthenticatedAccount now atSecret req of
        Left _ -> throw401 "Token invalid"
        Right a -> ExceptT $ pure $ Right a

mkApp :: (Monad n, MonadIO n) => ServerConfig -> n Application
mkApp cfg = do
  env <- runReaderT (envLayer cfg) CE.empty
  let context :: Context '[JWTAuthHandler]
      context = mkAuthHandler env :. EmptyContext
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
