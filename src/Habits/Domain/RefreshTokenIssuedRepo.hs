{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Habits.Domain.RefreshTokenIssuedRepo where

import Habits.Prelude
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import Habits.Domain.RefreshTokenIssuedNew
  ( RefreshTokenIssuedNew,
  )
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import Habits.Domain.AccountId (AccountId)
import qualified Veins.Data.Has as Has

type Add m =
  RefreshTokenIssuedNew ->
  Excepts '[RepositoryError] m RefreshTokenIssuedId

type GetById m =
  RefreshTokenIssuedId ->
  Excepts '[RepositoryError] m (Maybe RefreshTokenIssued)

type GetByAccountId m =
  AccountId ->
  Excepts '[RepositoryError] m [RefreshTokenIssued]

type DeleteByAccountId m =
  AccountId ->
  Excepts '[RepositoryError] m ()

type DeleteById m =
  RefreshTokenIssuedId ->
  Excepts '[RepositoryError] m ()

data RefreshTokenIssuedRepo m = RefreshTokenIssuedRepo
  { add :: Add m,
    getById :: GetById m,
    getByAccountId :: GetByAccountId m,
    deleteByAccountId :: DeleteByAccountId m,
    deleteById :: DeleteById m
  }

type instance ToSymbol (RefreshTokenIssuedRepo m) = "RefreshTokenIssuedRepo"

getRefreshTokenIssuedRepo :: (MonadReader r n, Has.Has (RefreshTokenIssuedRepo m) r) => n (RefreshTokenIssuedRepo m)
getRefreshTokenIssuedRepo = asks Has.get
