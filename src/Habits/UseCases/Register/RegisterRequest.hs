module Habits.UseCases.Register.RegisterRequest where
import           Data.Text                      ( Text )
import           Habits.Domain.Email            ( Email )

data RegisterRequest = RegisterRequest
  { email :: Email
  , name  :: Text
  }
