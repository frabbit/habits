{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Habits.Domain.RefreshTokenIssuedRepo where

import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import Habits.Domain.RefreshTokenIssuedNew
  ( RefreshTokenIssuedNew,
  )
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts)
import Veins.Data.ToSymbol (ToSymbol)
import Habits.Domain.AccountId (AccountId)
import qualified Veins.Data.Has as Has
import Control.Monad.Reader (asks, MonadReader)

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
  { _add :: Add m,
    _getById :: GetById m,
    _getByAccountId :: GetByAccountId m,
    _deleteByAccountId :: DeleteByAccountId m,
    _deleteById :: DeleteById m
  }

type instance ToSymbol (RefreshTokenIssuedRepo m) = "RefreshTokenIssuedRepo"

getRefreshTokenIssuedRepo :: (MonadReader r n, Has.Has (RefreshTokenIssuedRepo m) r) => n (RefreshTokenIssuedRepo m)
getRefreshTokenIssuedRepo = asks Has.get

add :: forall m. RefreshTokenIssuedRepo m -> Add m
add = (._add)

getById :: forall m. RefreshTokenIssuedRepo m -> GetById m
getById= (._getById)

getByAccountId :: forall m. RefreshTokenIssuedRepo m -> GetByAccountId m
getByAccountId = (._getByAccountId)

deleteByAccountId :: forall m. RefreshTokenIssuedRepo m -> DeleteByAccountId m
deleteByAccountId = (._deleteByAccountId)

deleteById :: forall m. RefreshTokenIssuedRepo m -> DeleteById m
deleteById = (._deleteById)
