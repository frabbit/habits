module Habits.UseCases.ConfirmEmail.ConfirmEmailRequest where

import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce)
import Habits.Prelude

data ConfirmEmailRequest = ConfirmEmailRequest
  { nonce :: EmailConfirmationNonce
  }
  deriving (Eq, Show)
