{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.Login.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Login as L

class Login m where
  login :: L.LoginExec m

instance (MonadReader env m, Has (L.Login m) env) => Login m where
  login = L.login
