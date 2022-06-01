module Habits.UseCases.Refresh where

import Habits.Prelude
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest)
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse)
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError)
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError)
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError)
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)

type RefreshExec m =
  RefreshRequest ->
  Excepts '[RepositoryError, RefreshTokenIssuedNotFoundError, RefreshTokenInvalidError, RefreshTokenExpiredError] m RefreshResponse

newtype Refresh m = Refresh { unRefresh :: RefreshExec m }

makeLensesWithoutUnderscoreAndWithSuffixL ''Refresh

type instance ToSymbol (Refresh m) = "Refresh"

askRefresh :: (MonadReader r n, Has.Has (Refresh m) r) => n (Refresh m)
askRefresh = asks Has.get

refresh :: forall m. Refresh m -> RefreshExec m
refresh = (.unRefresh)



