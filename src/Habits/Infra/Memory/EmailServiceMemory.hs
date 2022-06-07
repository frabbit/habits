module Habits.Infra.Memory.EmailServiceMemory where

import Veins.Prelude
import Habits.Domain.EmailService (EmailService (..), SendMessage)
import Veins.Data.ComposableEnv (LayerCE, ReaderCE)
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.EmailMessage (EmailMessage)
import UnliftIO (modifyTVar, atomically)
import Data.List.Extra (snoc)
import Habits.Infra.VarStorage (VarStorage, getVar)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Data.ToSymbol (ToSymbol)


mkSendMessage :: (MonadIO m, Monad n) => ReaderCE '[VarStorage [EmailMessage] m] n (SendMessage m)
mkSendMessage = CE.do
  provider <- CE.ask
  CE.pure $ \msg -> S.do
    var <- S.lift $ getVar provider
    S.liftIO $ atomically $ modifyTVar var (`snoc` msg)

mkEmailServiceMemory :: forall n m . (MonadIO n, MonadIO m) => LayerCE '[VarStorage [EmailMessage] m] n '[EmailService m]
mkEmailServiceMemory = do
  sendMessage <- mkSendMessage
  pure $ CE.singleton EmailService {sendMessage}

type instance ToSymbol EmailMessage = "EmailMessage"


