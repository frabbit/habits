module Habits.UseCases.Login where

import Habits.Prelude
import qualified Veins.Data.Has as Has
import Habits.UseCases.Login.LoginRequest (LoginRequest)
import Habits.UseCases.Login.LoginResponse (LoginResponse)
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)


type LoginExec m =
  LoginRequest ->
  Excepts '[RepositoryError, AccountNotFoundError, PasswordIncorrectError] m LoginResponse

newtype Login m = Login
  { unLogin :: LoginExec m
  }

type instance ToSymbol (Login m) = "Login"

makeLensesWithoutUnderscoreAndWithSuffixL ''Login

askLogin :: (MonadReader r n, Has.Has (Login m) r) => n (Login m)
askLogin = asks Has.get

login :: forall m. Login m -> LoginExec m
login = (.unLogin)

