module Habits.UseCases.ConfirmEmail.Class where

import Habits.Prelude
import Veins.Data.Has
  ( Has,
  )
import qualified Habits.UseCases.ConfirmEmail as R

import Habits.Utils (applyFirstM)

class ConfirmEmailM m where
  confirmEmail :: R.ConfirmEmailExec m

instance (MonadReader env m, Has (R.ConfirmEmail m) env) => ConfirmEmailM m where
  confirmEmail = applyFirstM R.confirmEmail
