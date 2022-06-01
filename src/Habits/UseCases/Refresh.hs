module Habits.UseCases.Refresh where

import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts)
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Control.Monad.Reader (MonadReader, asks)
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

refresh :: forall m env. (Has.Has (Refresh m) env, MonadReader env m) => RefreshExec m
refresh r = do
  Refresh f <- asks (Has.get @(Refresh m))
  f r

