module Habits.Domain.EmailService where

import Habits.Domain.EmailMessage (EmailMessage)
import Habits.Prelude
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

type SendMessage m = EmailMessage -> Excepts '[] m ()

data EmailService m = EmailService
  { sendMessage :: SendMessage m
  }

getEmailService :: forall m n env. (Has.Has (EmailService m) env, MonadReader env n) => n (EmailService m)
getEmailService = asks Has.get

type instance ToSymbol (EmailService m) = "EmailService"
