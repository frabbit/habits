module Habits.Web.Utils where

import Habits.Prelude
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts.Utils (catchExcepts)
import Servant (ServerError, err400, err401, err500, err409)
import qualified Veins.Data.Codec as Codec
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError)
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError)
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError)

mapAllErrorsToServerError :: (Monad m) => Excepts '[RefreshTokenExpiredError,  RefreshTokenInvalidError, RefreshTokenIssuedNotFoundError, EmailAlreadyUsedError, Codec.ValidationError, RepositoryError, AccountNotFoundError, PasswordIncorrectError] m a -> Excepts '[ServerError] m a
mapAllErrorsToServerError e =
  e
    & catchExcepts (\(_ :: Codec.ValidationError) -> failureE err400)
    & catchExcepts (\(_ :: RepositoryError) -> failureE err500)
    & catchExcepts (\(_ :: AccountNotFoundError) -> failureE err400)
    & catchExcepts (\(_ :: PasswordIncorrectError) -> failureE err401)
    & catchExcepts (\(_ :: EmailAlreadyUsedError) -> failureE err409)
    & catchExcepts (\(_ :: RefreshTokenIssuedNotFoundError) -> failureE err401)
    & catchExcepts (\(_ :: RefreshTokenInvalidError) -> failureE err401)
    & catchExcepts (\(_ :: RefreshTokenExpiredError) -> failureE err401)