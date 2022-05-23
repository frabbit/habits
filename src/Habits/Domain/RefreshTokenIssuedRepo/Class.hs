{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Habits.Domain.RefreshTokenIssuedRepo.Class where

import Control.Monad.RWS
  ( MonadReader,
  )
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.RefreshTokenIssuedRepo
  ( Add,
    GetById,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo as RTIR
import Habits.Domain.Email (Email)
import Haskus.Utils.Variant.Excepts (Excepts, failureE, liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Data.Has (Has)

class RefreshTokenIssuedRepo m where
  add :: Add m
  getById :: GetById m

instance (MonadReader env m, Has (RTIR.RefreshTokenIssuedRepo m) env) => RefreshTokenIssuedRepo m where
  add = RTIR.add
  getById = RTIR.getById