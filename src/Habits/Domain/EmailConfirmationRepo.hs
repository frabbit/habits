module Habits.Domain.EmailConfirmationRepo where

import Habits.Prelude
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Habits.Domain.EmailConfirmationId (EmailConfirmationId)
import Habits.Domain.EmailConfirmation (EmailConfirmation)
import Habits.Domain.EmailConfirmationNew (EmailConfirmationNew)
import Habits.Domain.EmailConfirmationNonce (EmailConfirmationNonce)
import Habits.Domain.EmailConfirmationNotFoundError (EmailConfirmationNotFoundError (EmailConfirmationNotFoundError))
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

type GetById m =
  EmailConfirmationId ->
  Excepts '[RepositoryError] m (Maybe EmailConfirmation)

type GetByNonce m =
  EmailConfirmationNonce ->
  Excepts '[RepositoryError] m (Maybe EmailConfirmation)

type GetByNonceOrFail m =
  EmailConfirmationNonce ->
  Excepts '[RepositoryError, EmailConfirmationNotFoundError] m EmailConfirmation

type Add m =
  EmailConfirmationNew ->
  Excepts '[RepositoryError] m EmailConfirmationId

data EmailConfirmationRepo m = EmailConfirmationRepo
  {
    getById :: GetById m,
    getByNonce :: GetByNonce m,
    add :: Add m
  }

type instance ToSymbol (EmailConfirmationRepo m) = "EmailConfirmationRepo"

getEmailConfirmationRepo :: (MonadReader r n, Has.Has (EmailConfirmationRepo m) r) => n (EmailConfirmationRepo m)
getEmailConfirmationRepo = asks Has.get

getByNonceOrFail :: (Monad m) => EmailConfirmationRepo m -> GetByNonceOrFail m
getByNonceOrFail repo e = S.do
  e1 <- getByNonce repo e
  case e1 of
    Just a -> liftE $ S.pure a
    Nothing -> failureE EmailConfirmationNotFoundError