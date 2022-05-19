module Habits.UseCases.Register.Live where

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.AccountNew (AccountNew (AccountNew))
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    add,
    getByEmail
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
import Haskus.Utils.Variant.Excepts (catchLiftLeft, throwE, failureE)
import Data.Function ((&))
import qualified Veins.Data.ComposableEnv as CE
import Control.Monad.Reader (ReaderT)
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(..))
import Control.Monad (when)
import Data.Maybe (isJust)
import Habits.Domain.PasswordHash (mkFromPassword)
import Control.Monad.IO.Class (MonadIO)




execute :: (Monad m, MonadIO m, AccountRepo m) => Execute m
execute req = (S.do
  account <- getByEmail (req ^. RR.email)
  when (isJust account) $ failureE EmailAlreadyUsedError
  pwHash <- S.coerce $ mkFromPassword (req ^. RR.password)
  accountId <- add
    ( AccountNew
        { AN._email = req ^. RR.email,
          AN._name = req ^. RR.name,
          AN._password = pwHash
        }
    )
  S.pure $ RegisterResponse { _accountId = accountId })
  & catchLiftLeft (\(_ :: RepositoryError) -> throwE RegisterError)


mkLive :: forall n m. (Monad n, Monad m, MonadIO m, AccountRepo m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[R.Register m])
mkLive = pure $ CE.empty & CE.insert R.Register {R._execute = execute}
