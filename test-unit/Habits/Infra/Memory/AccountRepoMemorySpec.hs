{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.AccountRepoMemorySpec where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Function ((&))
import Habits.AppT (AppT, runAppT', unAppT)
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.Domain.AccountRepositoryContract
  ( mkSpec,
  )
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL
import Test.Hspec
  ( Spec,
  )
import Veins.Data.ComposableEnv (ComposableEnv (..))
import qualified Veins.Data.ComposableEnv as CE
import Veins.Data.Has (Has (get))
import qualified Veins.Data.Has as Has
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

mkAppEnv :: forall m n. (MonadIO n, MonadIO m, ARC.AccountRepo m) => n (Env m)
mkAppEnv = do
  accountRepo <- ARM.mkAccountRepoMemory
  pure $ CE.empty & CE.insert RL.mkLive & CE.insert accountRepo

-- START BOILERPLATE

AppTH.mkAppEnv

AppTH.mkApp

AppTH.mkRunApp

AppTH.mkHasInstance

-- END BOILERPLATE

spec :: Spec
spec = mkSpec \x -> do
  env <- mkAppEnv
  runApp env x
