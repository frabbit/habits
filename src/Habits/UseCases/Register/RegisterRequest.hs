module Habits.UseCases.Register.RegisterRequest where

import Data.Text (Text)
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)
import Control.Lens (makeLenses)

data RegisterRequest = RegisterRequest
  { _email :: Email,
    _name :: Text,
    _password :: Password
  }

makeLenses ''RegisterRequest
