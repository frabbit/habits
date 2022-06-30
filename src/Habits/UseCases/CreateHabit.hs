module Habits.UseCases.CreateHabit where

import Habits.Prelude
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Veins.Control.Lens.Utils (makeLensesWithoutUnderscoreAndWithSuffixL)
import Habits.UseCases.CreateHabit.CreateHabitRequest (CreateHabitRequest)
import Habits.UseCases.CreateHabit.CreateHabitResponse (CreateHabitResponse)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.RepositoryError (RepositoryError)

type CreateHabitExec m =
  CreateHabitRequest ->
  Excepts '[RepositoryError, AccountNotFoundError] m CreateHabitResponse

newtype CreateHabit m = CreateHabit { createHabit :: CreateHabitExec m }

makeLensesWithoutUnderscoreAndWithSuffixL ''CreateHabit

type instance ToSymbol (CreateHabit m) = "CreateHabit"

askCreateHabit :: (MonadReader r n, Has.Has (CreateHabit m) r) => n (CreateHabit m)
askCreateHabit = asks Has.get
