module Habits.UseCases.Login.LoginRequest where

import Habits.Prelude

import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)

data LoginRequest = EmailPasswordLoginRequest Email Password deriving (Show,Eq,Ord)

