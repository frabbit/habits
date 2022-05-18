module Habits.UseCases.Login.LoginResponse where
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)

data LoginResponse = LoginResponse deriving (Show,Eq,Ord)

