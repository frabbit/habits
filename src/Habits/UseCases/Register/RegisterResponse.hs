{-# LANGUAGE TemplateHaskell #-}
module Habits.UseCases.Register.RegisterResponse where
import Habits.Domain.AccountId (AccountId)
import Control.Lens (makeLenses)

newtype RegisterResponse = RegisterResponse
  { _accountId :: AccountId
  }
  deriving (Show, Eq, Ord)


makeLenses ''RegisterResponse