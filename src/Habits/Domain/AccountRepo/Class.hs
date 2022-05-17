{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Habits.Domain.AccountRepo.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Habits.Domain.AccountRepo
  ( Add,
    GetById, GetByEmail,
  )
import qualified Habits.Domain.AccountRepo as AR
import Veins.Data.Has (Has)

class AccountRepo m where
  add :: Add m
  getById :: GetById m
  getByEmail :: GetByEmail m

instance (MonadReader env m, Has (AR.AccountRepo m) env) => AccountRepo m where
  add = AR.add
  getById = AR.getById
  getByEmail = AR.getByEmail
