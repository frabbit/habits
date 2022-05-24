{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Habits.Domain.RefreshTokenIssuedRepo.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Habits.Domain.RefreshTokenIssuedRepo
  ( Add,
    GetById, GetByAccountId, DeleteByAccountId, DeleteById,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo as RTIR
import Veins.Data.Has (Has)

class RefreshTokenIssuedRepo m where
  add :: Add m
  getById :: GetById m
  getByAccountId :: GetByAccountId m
  deleteByAccountId :: DeleteByAccountId m
  deleteById :: DeleteById m

instance (MonadReader env m, Has (RTIR.RefreshTokenIssuedRepo m) env) => RefreshTokenIssuedRepo m where
  add = RTIR.add
  getById = RTIR.getById
  getByAccountId = RTIR.getByAccountId
  deleteByAccountId = RTIR.deleteByAccountId
  deleteById = RTIR.deleteById