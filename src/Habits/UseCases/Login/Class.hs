{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.Login.Class where

import Habits.Prelude

import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Login as L
import Habits.Utils (applyFirstM)

class LoginM m where
  login :: L.LoginExec m

instance (MonadReader env m, Has (L.Login m) env) => LoginM m where
  login = applyFirstM L.login
