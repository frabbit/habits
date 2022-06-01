{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.Register.Class where

import Habits.Prelude
import Habits.Utils (applyFirstM)
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.Register as R

class RegisterM m where
  register :: R.RegisterExec m

instance (MonadReader env m, Has (R.Register m) env) => RegisterM m where
  register = applyFirstM R.register
