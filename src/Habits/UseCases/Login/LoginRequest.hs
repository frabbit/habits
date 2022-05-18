module Habits.UseCases.Login.LoginRequest where
import Habits.Domain.Email (Email)
import Habits.Domain.Password (Password)

data LoginRequest = EmailPasswordLoginRequest Email Password

