module Habits.UseCases.Register.Live where

import Habits.Prelude
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.AccountNew (AccountNew (AccountNew))
import qualified Habits.Domain.AccountNew as AN
import Habits.UseCases.Register
  ( RegisterExec,
    RegisterResponse
      ( RegisterResponse
      ),
  )
import qualified Habits.UseCases.Register as R
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(..))
import Control.Monad (when)
import Habits.Domain.PasswordHash (mkFromPassword)
import qualified Habits.Domain.AccountRepo as AR

mkRegister :: (MonadIO m, Monad n) => ReaderT (CE.MkSorted '[AR.AccountRepo m]) n (RegisterExec m)
mkRegister = do
  ar <- AR.getAccountRepo
  pure $ \req -> liftE $ S.do
    account <- AR.getByEmail ar req.email
    when (isJust account) $ failureE EmailAlreadyUsedError
    pwHash <- S.coerce $ mkFromPassword req.password
    accountId <- AR.add ar
      ( AccountNew
          { email = req.email,
            name = req.name,
            password = pwHash
          }
      )
    S.pure $ RegisterResponse { accountId }

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted '[AR.AccountRepo m]) n (CE.ComposableEnv '[R.Register m])
mkLive = CE.do
  f <- mkRegister
  CE.pure $ CE.empty & CE.insert (R.Register f)
