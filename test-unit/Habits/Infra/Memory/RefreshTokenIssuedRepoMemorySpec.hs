{-# LANGUAGE BlockArguments #-}

module Habits.Infra.Memory.RefreshTokenIssuedRepoMemorySpec where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Habits.Domain.RefreshTokenIssuedRepo as RTIR
import qualified Habits.Infra.Memory.RefreshTokenIssuedRepoMemory as RTIM

import Habits.Domain.RefreshTokenIssuedRepoContract
  ( mkSpec,
  )
import Test.Hspec
  ( Spec,
  )
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[RTIR.RefreshTokenIssuedRepo m]

envLayer :: forall n m . (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = RTIM.mkRefreshTokenIssuedRepoMemory

AppTH.mkBoilerplate "runApp" ''Env


spec :: Spec
spec = mkSpec \x -> do
  env <- runReaderT envLayer CE.empty
  runApp env x