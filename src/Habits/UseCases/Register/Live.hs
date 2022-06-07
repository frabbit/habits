{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
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
import Habits.Domain.PasswordHash (mkFromPassword)
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.EmailService (EmailService)
import qualified Habits.Domain.EmailService as ES
import Habits.Domain.Emails.RegistrationEmail (mkRegistrationEmail)
import Habits.Domain.EmailConfirmationNonce (newEmailConfirmationNonce)
import Habits.Domain.EmailConfirmationNew (EmailConfirmationNew(EmailConfirmationNew), email, accountId, emailConfirmationNonce)
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo, getEmailConfirmationRepo)

type Deps m = '[AR.AccountRepo m, EmailService m, EmailConfirmationRepo m]

mkRegister :: (MonadIO m, Monad n) => ReaderT (CE.MkSorted (Deps m)) n (RegisterExec m)
mkRegister = do
  ar <- AR.getAccountRepo
  es <- ES.getEmailService
  ecr <- getEmailConfirmationRepo
  pure $ \req -> liftE $ S.do
    account <- ar.getByEmail req.email
    when (isJust account) $ failureE EmailAlreadyUsedError
    pwHash <- S.coerce $ mkFromPassword req.password
    accountId <- ar.add
      ( AccountNew
          { email = req.email,
            emailConfirmed = False,
            name = req.name,
            password = pwHash
          }
      )
    nonce <- S.liftIO newEmailConfirmationNonce
    ecr.add $ EmailConfirmationNew { email = req.email, accountId, emailConfirmationNonce = nonce }
    es.sendMessage $ mkRegistrationEmail req.email nonce
    S.pure $ RegisterResponse { accountId }

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted (Deps m)) n (CE.ComposableEnv '[R.Register m])
mkLive = CE.singleton . R.Register <$> mkRegister
