{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Infra.Memory.EmailConfirmationRepoMemory where

import Habits.Prelude
import Data.Foldable (find)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Conc
  ( newTVarIO,
    readTVarIO,
  )

import UnliftIO
  ( TVar,
    atomically,
  )
import UnliftIO.STM (modifyTVar)
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo(..), GetById, Add, GetByNonce)
import Habits.Domain.EmailConfirmationId (EmailConfirmationId(..))
import Habits.Domain.EmailConfirmation (EmailConfirmation (), fromEmailConfirmationNew)

mkAdd ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [EmailConfirmation] ->
  n (Add m)
mkAdd var = pure f
  where
    f :: Add m
    f new = do
      uuid <- liftIO nextRandom
      let id' = EmailConfirmationId (toText uuid)
      let newAccount = fromEmailConfirmationNew new id'
      atomically $ modifyTVar var $ \a -> reverse (newAccount : reverse a)
      pure id'


mkGetById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [EmailConfirmation] ->
  n (GetById m)
mkGetById var = pure f
  where
    f :: GetById m
    f id = do
      entities <- liftIO $ readTVarIO var
      pure $ find (\a -> a.emailConfirmationId == id) entities

mkGetByNonce ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [EmailConfirmation] ->
  n (GetByNonce m)
mkGetByNonce var = pure f
  where
    f :: GetByNonce m
    f nonce = do
      entities <- liftIO $ readTVarIO var
      pure $ find (\a -> a.emailConfirmationNonce == nonce) entities

mkEmailConfirmationRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[EmailConfirmationRepo m])
mkEmailConfirmationRepoMemory = do
  var <- liftIO $ newTVarIO []
  _getById <- mkGetById var
  _getByNonce <- mkGetByNonce var
  _add <- mkAdd var
  pure $ CE.singleton EmailConfirmationRepo {_getById, _add, _getByNonce }
