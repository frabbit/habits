{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Habits.UseCases.Register.Class where

import qualified Habits.Domain.AccountRepo     as AR

import           Control.Monad.Exception        ( Throws )
import           Control.Monad.RWS              ( MonadReader
                                                , asks, MonadTrans (lift)
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                )
import           Habits.Domain.AccountRepo      ( Add
                                                , AddError
                                                )
import qualified Habits.UseCases.Register      as R

class Register m where
        execute :: R.RegisterExec m

instance (Monad m, MonadReader env m, Has (R.Register m) env) => Register m where
  execute x = do
    y <- lift $ asks getter
    let R.Register { R.execute = execute } = y
    execute x
