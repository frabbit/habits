{-# LANGUAGE UndecidableInstances #-}
module Habits.UseCases.Register.Class where

import           Control.Monad.RWS              ( MonadReader
                                                , MonadTrans(lift)
                                                , asks
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                )
import qualified Habits.UseCases.Register      as R

class Register m where
  execute :: R.RegisterExec m

instance (Monad m, MonadReader env m, Has (R.Register m) env) => Register m where
  execute x = do
    y <- lift $ asks getter
    let R.Register { R.execute = exec } = y
    exec x
