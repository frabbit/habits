module Habits.UseCases.ConfirmEmail where

import Habits.Prelude
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)
import Habits.UseCases.ConfirmEmail.ConfirmEmailRequest (ConfirmEmailRequest)
import Habits.UseCases.ConfirmEmail.ConfirmEmailResponse (ConfirmEmailResponse)
import Habits.Domain.EmailConfirmationNotFoundError (EmailConfirmationNotFoundError)

type ConfirmEmailExec m =
  ConfirmEmailRequest ->
  Excepts '[RepositoryError, EmailConfirmationNotFoundError] m ConfirmEmailResponse

newtype ConfirmEmail m = ConfirmEmail { confirmEmail :: ConfirmEmailExec m }

makeLensesWithoutUnderscoreAndWithSuffixL ''ConfirmEmail

type instance ToSymbol (ConfirmEmail m) = "ConfirmEmail"

askConfirmEmail :: (MonadReader r n, Has.Has (ConfirmEmail m) r) => n (ConfirmEmail m)
askConfirmEmail = asks Has.get
