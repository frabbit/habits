{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.EmailConfirmation where

import Habits.Prelude

import Habits.Domain.EmailConfirmationId (EmailConfirmationId)
import qualified Habits.Domain.EmailConfirmationNew as AN
import Habits.Domain.Email (Email)
import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce)

data EmailConfirmation = EmailConfirmation
  { emailConfirmationId :: EmailConfirmationId,
    emailConfirmationNonce:: EmailConfirmationNonce,
    email :: Email
  }
  deriving (Eq, Show, Ord)

fromEmailConfirmationNew :: AN.EmailConfirmationNew -> EmailConfirmationId -> EmailConfirmation
fromEmailConfirmationNew AN.EmailConfirmationNew {..} emailConfirmationId = EmailConfirmation {..}

toEmailConfirmationNew :: EmailConfirmation -> AN.EmailConfirmationNew
toEmailConfirmationNew EmailConfirmation {..} = AN.EmailConfirmationNew {..}
