module Habits.Domain.RefreshTokenIssuedRepo where

import Control.Lens
  ( Lens',
    lens,
  )
import qualified Control.Lens as L
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader.Class (asks)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.Email (Email)
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import Habits.Domain.RefreshTokenIssuedNew
  ( RefreshTokenIssuedNew (RefreshTokenIssuedNew),
  )
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

type Add m =
  RefreshTokenIssuedNew ->
  Excepts '[RepositoryError] m RefreshTokenIssuedId

type GetById m =
  RefreshTokenIssuedId ->
  Excepts '[RepositoryError] m (Maybe RefreshTokenIssued)

data RefreshTokenIssuedRepo m = RefreshTokenIssuedRepo
  { _add :: Add m,
    _getById :: GetById m
  }

type instance ToSymbol (RefreshTokenIssuedRepo m) = "RefreshTokenIssuedRepo"

add :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => Add m
add r = do
  RefreshTokenIssuedRepo {_add = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r

getById :: forall m env. (Has.Has (RefreshTokenIssuedRepo m) env, MonadReader env m) => GetById m
getById r = do
  RefreshTokenIssuedRepo {_getById = f} <- asks (Has.get @(RefreshTokenIssuedRepo m))
  f r
