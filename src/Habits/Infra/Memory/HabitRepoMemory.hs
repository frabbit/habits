{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.Infra.Memory.HabitRepoMemory where

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
import Habits.Domain.HabitRepo (GetByAccountId, HabitRepo (HabitRepo), getByAccountId, add, getById, GetById, Add)
import Habits.Domain.Habit (Habit)
import qualified Habits.Domain.Habit as H
import Habits.Domain.HabitId (HabitId(..))

mkAdd ::
  forall n m. (Applicative n, MonadIO m) => TVar [Habit] -> n (Add m)
mkAdd entitiesVar = pure f
  where
    f :: Add m
    f an = do
      uuid <- liftIO nextRandom
      let id' = HabitId (toText uuid)
      let newAccount = H.fromHabitNew an id'
      atomically $ modifyTVar entitiesVar $ \a -> reverse (newAccount : reverse a)
      pure id'

mkGetById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [Habit] ->
  n (GetById m)
mkGetById entitiesVar = pure f
  where
    f :: GetById m
    f accountId = do
      entities :: [Habit] <- liftIO $ readTVarIO entitiesVar
      pure $ find (\a -> a.habitId == accountId) entities


mkGetByAccountId ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [Habit] ->
  n (GetByAccountId m)
mkGetByAccountId accountsVar = pure f
  where
    f :: GetByAccountId m
    f accId = do
      entities :: [Habit] <- liftIO $ readTVarIO accountsVar
      pure $ filter (\a -> a.accountId == accId) entities

mkHabitRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[HabitRepo m])
mkHabitRepoMemory = do
  accountsVar <- liftIO $ newTVarIO []
  getById <- mkGetById accountsVar
  getByAccountId <- mkGetByAccountId accountsVar
  add <- mkAdd accountsVar
  pure $ CE.singleton HabitRepo {add, getById, getByAccountId}
