{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.HabitRepoMemorySpec (spec) where

import Habits.Test.Prelude

import qualified Habits.Domain.HabitRepo as AR
import Habits.Domain.HabitRepoContract
  ( mkSpec,
  )
import qualified Habits.Infra.Memory.HabitRepoMemory as ARM
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH


type Env m = CE.MkSorted '[AR.HabitRepo m]

envLayer :: forall n m . (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = ARM.mkHabitRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec \x -> do
  env <- runReaderT envLayer CE.empty
  runApp env x