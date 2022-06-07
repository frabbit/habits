module Habits.Infra.Memory.EmailServiceMemory where

import Habits.Prelude
import Habits.Domain.EmailService (EmailService (..), SendMessage)
import Veins.Data.ComposableEnv (LayerCE)
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.EmailMessage (EmailMessage)
import UnliftIO.STM (TVar, newTVarIO)
import UnliftIO (modifyTVar, atomically)
import Data.List.Extra (snoc)


mkSendMessage :: (MonadIO m, Applicative n) => TVar [EmailMessage] -> n (SendMessage m)
mkSendMessage var = do
  pure $ \msg -> liftIO $ atomically $ modifyTVar var (`snoc` msg)

mkEmailServiceMemory :: (MonadIO n, MonadIO m) => LayerCE '[] n '[EmailService m]
mkEmailServiceMemory = do
  var <- liftIO $ newTVarIO []
  mkEmailServiceMemoryUsingVar var

mkEmailServiceMemoryUsingVar :: (MonadIO n, MonadIO m) => TVar [EmailMessage] -> LayerCE '[] n '[EmailService m]
mkEmailServiceMemoryUsingVar var = do
  sendMessage <- mkSendMessage var
  pure $ CE.singleton EmailService {sendMessage}