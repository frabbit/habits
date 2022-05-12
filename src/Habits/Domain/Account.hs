{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Habits.Domain.Account where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)

data Account = Account
  { _accountId :: AccountId,
    _name :: Text,
    _email :: Email,
    _password :: Password
  }
  deriving (Eq, Show, Ord)

makeLenses ''Account

fromAccountNew :: AN.AccountNew -> AccountId -> Account
fromAccountNew AN.AccountNew {..} _accountId = Account {_accountId, ..}

toAccountNew :: Account -> AN.AccountNew
toAccountNew Account {..} = AN.AccountNew {..}
