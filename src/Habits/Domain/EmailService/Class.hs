module Habits.Domain.EmailService.Class where

import Habits.Prelude

import Habits.Domain.EmailService (SendMessage, EmailService)
import Veins.Data.Has (Has)
import Habits.Utils (applyFirstM)
import qualified Habits.Domain.EmailService as EmailService

class EmailServiceM m where
  sendMessage :: SendMessage m

instance (MonadReader env m, Has (EmailService m) env) => EmailServiceM m where
  sendMessage = applyFirstM EmailService.sendMessage