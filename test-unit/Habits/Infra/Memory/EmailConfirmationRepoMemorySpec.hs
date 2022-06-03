{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.EmailConfirmationRepoMemorySpec (spec) where

import Habits.Test.Prelude

import Habits.Domain.EmailConfirmationRepoContract
  ( mkSpec,
  )
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo)
import Habits.Infra.Memory.EmailConfirmationRepoMemory (mkEmailConfirmationRepoMemory)


type Env m = CE.MkSorted '[EmailConfirmationRepo m]

envLayer :: forall n m . (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = mkEmailConfirmationRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

spec :: Spec
spec = mkSpec \x -> do
  env <- runReaderT envLayer CE.empty
  runApp env x