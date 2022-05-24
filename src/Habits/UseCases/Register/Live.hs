module Habits.UseCases.Register.Live where

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.AccountNew (AccountNew (AccountNew))
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
    add,
    getByEmail
  )
import Habits.UseCases.Register
  ( Execute,
    RegisterResponse
      ( RegisterResponse
      ),
  )
import qualified Habits.UseCases.Register as R
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Data.Function ((&))
import qualified Veins.Data.ComposableEnv as CE
import Control.Monad.Reader (ReaderT)
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(..))
import Control.Monad (when)
import Data.Maybe (isJust)
import Habits.Domain.PasswordHash (mkFromPassword)
import Control.Monad.IO.Class (MonadIO)

execute :: (MonadIO m, AccountRepo m) => Execute m
execute req = liftE $ S.do
  account <- getByEmail req.email
  when (isJust account) $ failureE EmailAlreadyUsedError
  pwHash <- S.coerce $ mkFromPassword req.password
  accountId <- add
    ( AccountNew
        { email = req.email,
          name = req.name,
          password = pwHash
        }
    )
  S.pure $ RegisterResponse { accountId }


mkLive :: forall n m. (Monad n, MonadIO m, AccountRepo m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[R.Register m])
mkLive = pure $ CE.empty & CE.insert R.Register {R._execute = execute}
