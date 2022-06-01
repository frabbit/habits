{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Infra.Memory.RefreshTokenIssuedRepoMemory where

import Habits.Prelude

import Data.Foldable (find)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Conc
  ( newTVarIO,
    readTVarIO,
  )
import qualified Habits.Domain.RefreshTokenIssued as RTI
import UnliftIO
  ( TVar,
    atomically,
  )
import UnliftIO.STM (modifyTVar)
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.RefreshTokenIssuedRepo (RefreshTokenIssuedRepo (RefreshTokenIssuedRepo, _getById, _add, _deleteByAccountId), Add, GetById, GetByAccountId, _getByAccountId, DeleteByAccountId, DeleteById, _deleteById)
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId(..))

mkAdd ::
  forall n m. (Applicative n, MonadIO m) => TVar [RefreshTokenIssued] -> n (Add m)
mkAdd accountsVar = pure f
  where
    f :: Add m
    f an = do
      uuid <- liftIO nextRandom
      let id' = RefreshTokenIssuedId (toText uuid)
      let newAccount = RTI.fromRefreshTokenIssuedNew an id'
      atomically $ modifyTVar accountsVar $ \a -> reverse (newAccount : reverse a)
      pure id'

mkGetById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [RefreshTokenIssued] ->
  n (GetById m)
mkGetById accountsVar = pure f
  where
    f :: GetById m
    f accountId = do
      accounts :: [RefreshTokenIssued] <- liftIO $ readTVarIO accountsVar
      pure $ find (\a -> a.refreshTokenIssuedId == accountId) accounts

mkGetByAccountId ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [RefreshTokenIssued] ->
  n (GetByAccountId m)
mkGetByAccountId accountsVar = pure f
  where
    f :: GetByAccountId m
    f accountId = do
      accounts :: [RefreshTokenIssued] <- liftIO $ readTVarIO accountsVar
      pure $ filter (\a -> a.accountId == accountId) accounts



mkDeleteByAccountId ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [RefreshTokenIssued] ->
  n (DeleteByAccountId m)
mkDeleteByAccountId accountsVar = pure f
  where
    f :: DeleteByAccountId m
    f accountId = do
      liftIO . atomically $ modifyTVar accountsVar $ filter (\a -> a.accountId /= accountId)
      pure ()

mkDeleteById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [RefreshTokenIssued] ->
  n (DeleteById m)
mkDeleteById accountsVar = pure f
  where
    f :: DeleteById m
    f entityId = do
      liftIO . atomically $ modifyTVar accountsVar $ filter (\a -> a.refreshTokenIssuedId /= entityId)
      pure ()


mkRefreshTokenIssuedRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[RefreshTokenIssuedRepo m])
mkRefreshTokenIssuedRepoMemory = do
  entitiesVar <- liftIO $ newTVarIO []
  _getById <- mkGetById entitiesVar
  _add <- mkAdd entitiesVar
  _getByAccountId <- mkGetByAccountId entitiesVar
  _deleteByAccountId <- mkDeleteByAccountId entitiesVar
  _deleteById <- mkDeleteById entitiesVar
  pure $ CE.singleton RefreshTokenIssuedRepo {_add, _getById, _getByAccountId, _deleteByAccountId, _deleteById }
