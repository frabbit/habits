module Habits.UseCases.Register.RegisterResponse where
import Habits.Domain.AccountId (AccountId)

newtype RegisterResponse = RegisterResponse
  { accountId :: AccountId
  }
  deriving (Show, Eq, Ord)
