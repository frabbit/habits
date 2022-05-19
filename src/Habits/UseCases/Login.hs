module Habits.UseCases.Login where
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Habits.UseCases.Login.LoginRequest (LoginRequest)
import Habits.UseCases.Login.LoginResponse (LoginResponse)
import Habits.Domain.RepositoryError (RepositoryError)
import Veins.Data.ToSymbol (ToSymbol)
import Control.Lens (Lens', lens)
import Control.Monad.Reader (MonadReader, asks)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)


type Execute m =
  LoginRequest ->
  Excepts '[RepositoryError, AccountNotFoundError, PasswordIncorrectError] m LoginResponse

newtype Login m = Login
  { _execute :: Execute m
  }

type instance ToSymbol (Login m) = "Login"

executeL :: forall m. Lens' (Login m) (Execute m)
executeL = lens get set
  where
    set :: Login m -> Execute m -> Login m
    set ar a = ar {_execute = a}
    get :: Login m -> Execute m
    get Login {_execute = a} = a

execute :: forall m env. (Has.Has (Login m) env, MonadReader env m) => Execute m
execute r = do
  Login {_execute = f} <- asks (Has.get @(Login m))
  f r
