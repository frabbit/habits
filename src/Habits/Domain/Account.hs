{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.Account where

import Habits.Prelude

import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.Email (Email)
import Habits.Domain.PasswordHash (PasswordHash)
import Habits.Domain.AccountUpdate (AccountUpdate)

data Account = Account
  { accountId :: AccountId,
    name :: Text,
    email :: Email,
    emailConfirmed :: Bool,
    password :: PasswordHash
  }
  deriving (Eq, Show, Ord)

fromAccountNew :: AN.AccountNew -> AccountId -> Account
fromAccountNew AN.AccountNew {..} accountId = Account {accountId, ..}

toAccountNew :: Account -> AN.AccountNew
toAccountNew Account {..} = AN.AccountNew {..}

updateAccount :: AccountUpdate -> Account -> Account
updateAccount up a =
  a{emailConfirmed = maybe a.emailConfirmed identity up.emailConfirmed, email = maybe a.email identity up.email}