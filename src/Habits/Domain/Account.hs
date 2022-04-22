{-# LANGUAGE TemplateHaskell #-}
module Habits.Domain.Account where
import           Control.Lens                   ( makeLenses )
import           Data.Text                      ( Text )
import           Habits.Domain.AccountId        ( AccountId )
import           Habits.Domain.Email            ( Email )

data Account = Account
  { _accountId :: AccountId
  , _name      :: Text
  , _email     :: Email
  }
  deriving (Eq, Show, Ord)

makeLenses ''Account
