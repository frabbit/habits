module Veins.TestContainers.Postgres where

import Control.Monad.IO.Class ( liftIO )
import Data.Function ((&))
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy
  ( isInfixOf,
  )
import qualified Database.Persist.Postgresql as P


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
    waitUntilTimeout, withContainers,
  )
import TestContainers.WaitStrategies
  ( waitDelay,
    waitPar,
    waitSeq,
  )
import Test.QuickCheck (Gen, choose, generate)
import Control.Monad (replicateM)
import qualified Data.Pool as P'
import Control.Monad.Logger (runStderrLoggingT)

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

genAlphaText :: Int -> Int -> Gen Text
genAlphaText min' max' = do
  nameLength <- choose (min', max')
  name <- replicateM nameLength (choose ('a', 'z'))

  pure . Text.pack $ name

genPostgresUser :: Gen Text
genPostgresUser = genAlphaText 5 10

genPostgresPassword :: Gen Text
genPostgresPassword = genAlphaText 20 40

genPostgresDbName :: Gen Text
genPostgresDbName = genAlphaText 10 20

postgresContainer :: MonadDocker m => m PostgresConfig
postgresContainer = do
  user <- liftIO $ generate genPostgresUser
  pw <- liftIO $ generate genPostgresPassword
  db <- liftIO $ generate genPostgresDbName
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

mkPostgresConnectionString :: Text -> PostgresConfig -> P.ConnectionString
mkPostgresConnectionString host cfg =
  Text.encodeUtf8 $
      ""
      <> "host=" <> host
      <> " dbname=" <> _db cfg
      <> " user=" <> _user cfg
      <> " password=" <> _pw cfg
      <> " port=" <> Text.pack (show . _port $ cfg)


withPostgresPool :: (P'.Pool P.SqlBackend -> IO ()) -> IO ()
withPostgresPool f = withContainers postgresContainer $ \cfg -> do
  runStderrLoggingT $
    P.withPostgresqlPool (mkPostgresConnectionString "localhost" cfg) 10 (liftIO . f)