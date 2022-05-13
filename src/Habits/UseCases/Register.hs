module Habits.UseCases.Register
  ( RegisterError (..),
    RegisterResponse (..),
    RegisterRequest (..),
    Register (..),
    Execute,
    execute,
  )
where

import Control.Lens
  ( Lens',
    lens,
  )
import Control.Monad.Reader (MonadReader, asks)
import Habits.UseCases.Register.RegisterError
  ( RegisterError (..),
  )
import Habits.UseCases.Register.RegisterRequest
  ( RegisterRequest (..),
  )
import Habits.UseCases.Register.RegisterResponse
  ( RegisterResponse (..),
  )
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

type Execute m =
  RegisterRequest ->
  Excepts '[RegisterError] m RegisterResponse

newtype Register m = Register
  { _execute :: Execute m
  }

type instance ToSymbol (Register m) = "Register"

executeL :: forall m. Lens' (Register m) (Execute m)
executeL = lens get set
  where
    set :: Register m -> Execute m -> Register m
    set ar a = ar {_execute = a}
    get :: Register m -> Execute m
    get Register {_execute = a} = a

execute :: forall m env. (Has.Has (Register m) env, MonadReader env m) => Execute m
execute r = do
  Register {_execute = f} <- asks (Has.get @(Register m))
  f r
