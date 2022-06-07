module Habits.UseCases.Register
  ( RegisterResponse (..),
    RegisterRequest (..),
    Register (..),
    RegisterExec,
    askRegister,
    registerL,
  )
where

import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError)
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Prelude
import Habits.UseCases.Register.RegisterRequest
  ( RegisterRequest (..),
  )
import Habits.UseCases.Register.RegisterResponse
  ( RegisterResponse (..),
  )
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

type RegisterExec m =
  RegisterRequest ->
  Excepts '[EmailAlreadyUsedError, RepositoryError] m RegisterResponse

newtype Register m = Register
  { register :: RegisterExec m
  }

type instance ToSymbol (Register m) = "Register"

makeLensesWithoutUnderscoreAndWithSuffixL ''Register

askRegister :: (MonadReader r n, Has.Has (Register m) r) => n (Register m)
askRegister = asks Has.get

