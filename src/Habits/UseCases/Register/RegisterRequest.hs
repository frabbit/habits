module Habits.UseCases.Register.RegisterRequest where
import           Data.Text                      ( Text )
import           Habits.Domain.Email            ( Email )
import           Habits.Domain.Password         ( Password )

data RegisterRequest = RegisterRequest
  { email    :: Email
  , name     :: Text
  , password :: Password
  }
