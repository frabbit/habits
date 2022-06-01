module Habits.UseCases.Register
  ( RegisterResponse (..),
    RegisterRequest (..),
    Register (..),
    RegisterExec,
    register,
    unRegisterL,
    askRegister)
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

askRegister :: (MonadReader r n, Has.Has (Register m) r) => n (Register m)
askRegister = asks Has.get

{- HLINT ignore "Redundant bracket" -}
register :: forall m. Register m -> RegisterExec m
register = (.unRegister)

