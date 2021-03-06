{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Habits.Infra.Postgres.AccountRepoPostgresSpec where

import Habits.Prelude
import Data.Pool (Pool)
import qualified Database.Persist.Postgresql as P
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountRepoContract
  ( mkSpec,
  )
import qualified Habits.Infra.Postgres.AccountRepoPostgres as ARP
import qualified Habits.Infra.Postgres.Schema as S
import Test.Hspec
  ( Spec,
  )
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.TestContainers.Postgres (withPostgresPool)

type Env m = CE.MkSorted '[AR.AccountRepo m]

envLayer :: forall n m. (MonadIO n, MonadIO m) => Pool P.SqlBackend -> CE.ReaderCE '[] n (Env m)
envLayer = ARP.mkAccountRepoPostgres

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec $ \x -> do
  withPostgresPool $
    \pool -> do
      S.migrateAll pool
      env <- runReaderT (envLayer pool) CE.empty
      runApp env x
