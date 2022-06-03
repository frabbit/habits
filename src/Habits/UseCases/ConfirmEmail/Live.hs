{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Habits.UseCases.ConfirmEmail.Live where

import Habits.Prelude
import qualified Veins.Data.ComposableEnv as CE

import Habits.UseCases.ConfirmEmail (ConfirmEmailExec, ConfirmEmail (ConfirmEmail))
import Habits.Domain.EmailConfirmationNotFoundError (EmailConfirmationNotFoundError(EmailConfirmationNotFoundError))
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import qualified Habits.Domain.AccountRepo as AccountRepo
import Habits.Domain.AccountRepo (AccountRepo)
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo, getEmailConfirmationRepo, getByNonce, getByNonceOrFail)
import Habits.Domain.AccountUpdate (AccountUpdate(AccountUpdate), emailConfirmed, email)
import Habits.UseCases.ConfirmEmail.ConfirmEmailRequest (ConfirmEmailRequest)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.Account (Account)
import Habits.UseCases.ConfirmEmail.ConfirmEmailResponse (ConfirmEmailResponse(ConfirmEmailResponse))

mkExecute :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.MkSorted '[AccountRepo m, EmailConfirmationRepo m]) n (ConfirmEmailExec m)
mkExecute = do
  ar <- AccountRepo.getAccountRepo
  er <- getEmailConfirmationRepo
  let
      f :: ConfirmEmailExec m
      f req = S.do
        conf <- getByNonceOrFail er (req.nonce)
        AccountRepo.update ar (AccountUpdate { emailConfirmed = Just True, email = Just conf.email}) conf.accountId
        S.pure ConfirmEmailResponse
  pure f

mkLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.ComposableEnv '[AccountRepo m, EmailConfirmationRepo m]) n (CE.ComposableEnv '[ConfirmEmail m])
mkLive = CE.singleton . ConfirmEmail <$> mkExecute
