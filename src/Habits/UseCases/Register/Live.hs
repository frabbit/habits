module Habits.UseCases.Register.Live where

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
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
      ( RegisterResponse, _accountId
      ),
  )
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.RegisterRequest as RR
import Control.Lens ((^.))
import Haskus.Utils.Variant.Excepts (catchLiftLeft, catchE, catchLiftBoth, throwE)
import Data.Function ((&))



execute :: (Monad m, AccountRepo m) => Execute m
execute req = (S.do
  accountId <- add
    ( AccountNew
        { AN._email = req ^. RR.email,
          AN._name = req ^. RR.name,
          AN._password = req ^. RR.password
        }
    )
  S.pure $ RegisterResponse { _accountId = accountId })
  & catchLiftLeft (\(_ :: AddError) -> throwE RegisterError)


mkLive :: forall m. (Monad m, AccountRepo m) => R.Register m
mkLive = R.Register {R._execute = execute}
