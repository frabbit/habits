module Habits.UseCases.Register.Live where

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.AccountNew (AccountNew (AccountNew))
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.AccountRepo (AddError)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    add,
  )
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
import Haskus.Utils.Variant.Excepts (catchLiftLeft, throwE)
import Data.Function ((&))
import qualified Veins.Data.ComposableEnv as CE
import Control.Monad.Reader (ReaderT)



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


mkLive :: forall n m. (Monad n, Monad m, AccountRepo m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[R.Register m])
mkLive = pure $ CE.empty & CE.insert R.Register {R._execute = execute}
