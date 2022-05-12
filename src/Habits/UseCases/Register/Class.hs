{-# LANGUAGE UndecidableInstances #-}

module Habits.UseCases.Register.Class where

import Control.Lens ((^.))
import Control.Monad.RWS
  ( MonadReader,
    MonadTrans (lift),
    asks,
  )
import Veins.Data.Has
  ( Has,
    get,
  )
import qualified Habits.UseCases.Register as R

class Register m where
  execute :: R.Execute m

instance (Monad m, MonadReader env m, Has (R.Register m) env) => Register m where
  execute = R.execute
