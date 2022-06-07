module Habits.Domain.Emails.RegistrationEmail where

import Habits.Prelude

import Habits.Domain.EmailMessage
import Habits.Domain.Email (Email(Email))
import Text.Regex.TDFA ((=~))

import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce(..))
import Veins.Data.List.Utils (safeHead)

getNonceFromRegistrationEmail :: EmailBody -> Maybe EmailConfirmationNonce
getNonceFromRegistrationEmail (EmailBody txt) = EmailConfirmationNonce <$> match
  where
    match = (txt =~ nonceRegex :: (Text, Text, Text,[Text])) & fourth & safeHead
    nonceRegex = "Your activation code: ([a-zA-Z0-9_-]+)"::Text
    fourth (_,_,_,x) = x


mkRegistrationEmail :: Email -> EmailConfirmationNonce -> EmailMessage
mkRegistrationEmail receiver' nonce = EmailMessage {
    body,
    receiver,
    sender
  }
  where
    body = EmailBody $ "Welcome to Habits,\n\nYour activation code: " <> nonce.unEmailConfirmationNonce <> "\n\nBest regards, The Habits Team"
    receiver = EmailReceiver receiver'
    sender = EmailSender $ Email "contact@habits.de"