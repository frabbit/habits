{-# LANGUAGE UndecidableInstances #-}

module Habits.Infra.Postgres.AccountRepoPostgresSpec where

import Habits.Prelude
import Data.Pool (Pool)
import qualified Database.Persist.Postgresql as P
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountRepositoryContract
  ( mkSpec,
  )
import qualified Habits.Infra.Postgres.AccountRepoPostgres as ARP
import qualified Habits.Infra.Postgres.Schema as S
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL
import Test.Hspec
  ( Spec,
  )
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.TestContainers.Postgres (withPostgresPool)
import Veins.Data.ComposableEnv ((<<-&&))

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall n m. (MonadIO n, MonadIO m) => Pool P.SqlBackend -> CE.ReaderCE '[] n (Env m)
envLayer pool = RL.mkLive <<-&& ARP.mkAccountRepoPostgres pool

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec $ \x -> do
  withPostgresPool $
    \pool -> do
      S.migrateAll pool
      env <- runReaderT (envLayer pool) CE.empty
      runApp env x
