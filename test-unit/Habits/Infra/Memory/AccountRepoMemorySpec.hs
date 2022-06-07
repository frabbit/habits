{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.AccountRepoMemorySpec (spec) where

import Habits.Test.Prelude

import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountRepoContract
  ( mkSpec,
  )
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH


type Env m = CE.MkSorted '[AR.AccountRepo m]

envLayer :: forall n m . (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = ARM.mkAccountRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec \x -> do
  env <- runReaderT envLayer CE.empty
  runApp env x