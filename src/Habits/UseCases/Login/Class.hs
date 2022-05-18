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
  execute :: L.Execute m

instance (MonadReader env m, Has (L.Login m) env) => Login m where
  execute = L.execute
