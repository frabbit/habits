module Habits.Infra.VarStorage.Live where

import Veins.Prelude
import qualified Veins.Data.ComposableEnv as CE
import UnliftIO.STM (TVar, newTVarIO, readTVarIO)
import Habits.Infra.VarStorage (VarStorage (VarStorage), GetVar, getVar, ReadVar, readVar)

mkGetVar :: forall n m t . (Monad m, MonadIO n) => TVar t -> n (GetVar t m)
mkGetVar var = do
  pure . pure $ var

mkReadVar :: forall n m t . ( MonadIO n, MonadIO m) =>TVar t -> n (ReadVar t m)
mkReadVar var = do
  pure $ readTVarIO var

mkVarStorageLive :: forall t n m . (MonadIO n, MonadIO m) => t -> ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[VarStorage t m])
mkVarStorageLive t = do
  var <- liftIO $ newTVarIO t
  getVar <- mkGetVar var
  readVar <- mkReadVar var
  pure $ CE.singleton VarStorage { getVar, readVar }
