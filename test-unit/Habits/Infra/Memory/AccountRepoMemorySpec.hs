{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.AccountRepoMemorySpec (spec) where

import Prelude
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountRepositoryContract
  ( mkSpec,
  )
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL
import Test.Hspec
  ( Spec,
  )
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Data.ComposableEnv ((<<-&&))

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall n m . (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = RL.mkLive <<-&& ARM.mkAccountRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec \x -> do
  env <- runReaderT envLayer CE.empty
  runApp env x