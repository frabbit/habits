{-# LANGUAGE BlockArguments #-}
module Habits.Infra.Memory.AccountRepoMemorySpec where

import           Habits.App                ( runApp )
import           Habits.AppEnv             ( mkAppEnv )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Control.Monad.IO.Class         ( liftIO )
import           Habits.Domain.AccountRepositoryContract
                                                ( mkSpec )

spec :: Spec
spec = mkSpec \x -> do
  env <- mkAppEnv
  runApp env x
