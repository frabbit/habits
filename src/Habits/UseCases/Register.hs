module Habits.UseCases.Register
  ( RegisterResponse (..),
    RegisterRequest (..),
    Register (..),
    RegisterExec,
    register,
    unRegisterL,
  )
where

import Habits.Prelude
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError)
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.UseCases.Register.RegisterRequest
  ( RegisterRequest (..),
  )
import Habits.UseCases.Register.RegisterResponse
  ( RegisterResponse (..),
  )
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)

type RegisterExec m =
  RegisterRequest ->
  Excepts '[EmailAlreadyUsedError, RepositoryError] m RegisterResponse

newtype Register m = Register
  { unRegister :: RegisterExec m
  }

type instance ToSymbol (Register m) = "Register"

makeLensesWithoutUnderscoreAndWithSuffixL ''Register

register :: forall m env. (Has.Has (Register m) env, MonadReader env m) => RegisterExec m
register r = do
  Register {unRegister = f} <- asks (Has.get @(Register m))
  f r
