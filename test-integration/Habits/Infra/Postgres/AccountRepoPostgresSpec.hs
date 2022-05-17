{-# LANGUAGE UndecidableInstances #-}
module Habits.Infra.Postgres.AccountRepoPostgresSpec where

import qualified Control.Lens as L
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Function ((&))
import Data.Text (Text)
import Habits.UseCases.Register.Live as RL

import qualified Data.Pool as P'

import Veins.Data.ComposableEnv as CE
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy
  ( isInfixOf,
  )
import qualified Database.Persist.Postgresql as P

import Habits.Domain.AccountRepositoryContract
  ( mkSpec,
  )
import qualified Habits.Infra.Postgres.AccountRepoPostgres as ARP
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.UseCases.Register as R
import qualified Veins.Test.AppTH as AppTH

import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Infra.Postgres.Schema as S
import Test.Hspec
  ( Spec,
  )
import qualified TestContainers.Docker as TD
import TestContainers.Hspec
  ( MonadDocker,
    Pipe (Stdout),
    containerIp,
    containerPort,
    containerRequest,
    run,
    setEnv,
    setExpose,
    setWaitingFor,
    waitForLogLine,
    waitUntilMappedPortReachable,
    waitUntilTimeout,
    withContainers,
  )
import TestContainers.WaitStrategies
  ( waitDelay,
    waitPar,
    waitSeq,
  )

postgres :: TD.ToImage
postgres = TD.fromTag "postgres:14-alpine"

data PostgresConfig = PostgresConfig
  { _user :: Text,
    _pw :: Text,
    _port :: Int,
    _ip :: Text,
    _db :: Text
  }
  deriving (Show, Eq, Ord)

containers1 :: MonadDocker m => m PostgresConfig
containers1 = do
  let user = "test"
  let pw = "test"
  let db = "test"
  let waitForPort = waitUntilMappedPortReachable 5432
  let waitForLine = waitForLogLine Stdout ("database system is ready to accept" `isInfixOf`)

  let waitPolicy =
        waitUntilTimeout 20 $
          waitSeq (waitPar waitForLine waitForPort) (waitDelay 600)

  let setupPostgres =
        containerRequest postgres
          & setExpose [5432]
          & setWaitingFor waitPolicy
          & setEnv
            [ ("POSTGRES_USER", user),
              ("POSTGRES_PASSWORD", pw),
              ("POSTGRES_DB", db)
            ]

  container <- run setupPostgres

  let ip = containerIp container
  let port = containerPort container 5432
  let cfg =
        PostgresConfig
          { _user = user,
            _pw = pw,
            _ip = ip,
            _port = port,
            _db = db
          }
  pure cfg

connStr :: Text -> Int -> P.ConnectionString
connStr ip port =
  Text.encodeUtf8 $
    "host="
      <> ip
      <> " dbname=test user=test password=test port="
      <> Text.pack (show port)



type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall n m . (MonadIO n, ARC.AccountRepo m, MonadIO m) => P'.Pool P.SqlBackend -> CE.ReaderCE '[] n (Env m)
envLayer pool = ARP.mkAccountRepoPostgres pool `CE.provideAndChainLayer` RL.mkLive

AppTH.mkBoilerplate "runApp" ''Env


spec :: Spec
spec = mkSpec $ \x -> do
  withContainers containers1 $ \cfg -> do
    runStderrLoggingT $
      P.withPostgresqlPool (connStr "localhost" (_port cfg)) 10 $
        \pool -> liftIO $ do
          S.migrateAll pool
          env <- runReaderT (envLayer pool) CE.empty
          runApp env x
