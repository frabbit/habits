{-# LANGUAGE BlockArguments #-}

module Habits.Infra.Memory.AccountRepoMemorySpec where

import Habits.App (runApp)
import Habits.AppEnv (mkAppEnv)
import Habits.Domain.AccountRepositoryContract
  ( mkSpec,
  )
import Test.Hspec
  ( Spec,
  )

spec :: Spec
spec = mkSpec \x -> do
  env <- mkAppEnv
  runApp env x
