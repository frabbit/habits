{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.Register.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Register as R

class Register m where
  register :: R.RegisterExec m

instance (MonadReader env m, Has (R.Register m) env) => Register m where
  register = R.register
