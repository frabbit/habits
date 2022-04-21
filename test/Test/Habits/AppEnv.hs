{-# LANGUAGE TemplateHaskell #-}

module Test.Habits.AppEnv where

import           Control.Lens                   ( makeLenses )
import           Data.Has                       ( Has
                                                , hasLens
                                                )
import qualified Habits.Domain.AccountRepo     as AR
import qualified Habits.UseCases.Register      as R

import qualified Habits.Domain.AccountRepo.Class
                                               as ARC


import qualified Habits.UseCases.Register.Live as RegisterLive

import           Habits.Domain.AccountId        ( AccountId(..) )
data AppEnv m = AppEnv
  { _accountRepo :: AR.AccountRepo m
  , _register    :: R.Register m
  }

makeLenses ''AppEnv

instance Has (AR.AccountRepo m) (AppEnv m) where
  hasLens = accountRepo

instance Has (R.Register m) (AppEnv m) where
  hasLens = register

mkAppEnv :: (Monad m, ARC.AccountRepo m) => AppEnv m
mkAppEnv = AppEnv
  { _accountRepo = AR.AccountRepo { AR.add = \_ -> pure (AccountId "abc") }
  , _register    = R.Register { R.execute = RegisterLive.register }
  }
