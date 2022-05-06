module Habits.UseCases.Register.Live where

import Data.Variant
  ( catchM,
    throwM,
  )
import Habits.Domain.AccountNew (AccountNew (AccountNew))
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.AccountRepo (AddError)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    add,
  )
import Habits.Domain.Email (Email (Email))
import Habits.Domain.Password (Password (Password))
import Habits.UseCases.Register
  ( Execute,
    RegisterError (RegisterError),
    RegisterResponse
      ( RegisterResponse
      ),
  )
import qualified Habits.UseCases.Register as R

execute :: (Monad m, AccountRepo m) => Execute m
execute _ = do
  catchM
    ( add
        ( AccountNew
            { AN._email = Email "what@do.de",
              AN._name = "aha",
              AN._password = Password "pw1"
            }
        )
    )
    (\(_ :: AddError) -> throwM RegisterError)
  pure $ RegisterResponse True

mkLive :: forall m. (Monad m, AccountRepo m) => R.Register m
mkLive = R.Register {R._execute = R.WrapExecute execute}
