{-# LANGUAGE TemplateHaskell #-}

module Habits.AppEnv where

import           Control.Lens                   ( makeLenses )
import           Data.Has                       ( Has
                                                , hasLens
                                                )

import qualified Habits.Domain.AccountRepo.Class
                                               as ARC


import qualified Habits.Domain.AccountRepo     as AR
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register      as R
import qualified Habits.UseCases.Register.Live as RL
import Habits.Domain.Account (Account)
import UnliftIO (TVar, newTVar, atomically, newTVarIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)

data AppEnv m = AppEnv
  { _accountRepo :: AR.AccountRepo m
  , _register    :: R.Register m
  , _accountsDb :: TVar [Account]
  }

makeLenses ''AppEnv

instance Has (TVar [Account]) (AppEnv m) where
  hasLens = accountsDb

instance Has (AR.AccountRepo m) (AppEnv m) where
  hasLens = accountRepo

instance Has (R.Register m) (AppEnv m) where
  hasLens = register

mkAppEnv :: (MonadIO n) => (MonadReader (AppEnv m) m, MonadIO m, ARC.AccountRepo m) => n (AppEnv m)
mkAppEnv = do
  _accountsDb <- newTVarIO []
  _accountRepo <- ARM.mkAccountRepoMemory
  pure $ AppEnv { _accountRepo, _register = RL.mkLive, _accountsDb }
