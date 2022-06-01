{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.Account where

import Habits.Prelude

import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.Email (Email)
import Habits.Domain.PasswordHash (PasswordHash)

data Account = Account
  { accountId :: AccountId,
    name :: Text,
    email :: Email,
    password :: PasswordHash
  }
  deriving (Eq, Show, Ord)

fromAccountNew :: AN.AccountNew -> AccountId -> Account
fromAccountNew AN.AccountNew {..} accountId = Account {accountId, ..}

toAccountNew :: Account -> AN.AccountNew
toAccountNew Account {..} = AN.AccountNew {..}
