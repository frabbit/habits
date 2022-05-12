module Habits.UseCases.Register
  ( RegisterError(..)
  , RegisterResponse(..)
  , RegisterRequest(..)
  , Register(..)
  , Execute
  , execute
  , ExecuteW(..)
  ) where

import           Control.Lens                   ( Lens'
                                                , lens
                                                , makeLenses
                                                )
import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Variant                   ( CouldBe
                                                , Variant
                                                )
import           Habits.UseCases.Register.RegisterError
                                                ( RegisterError(..) )
import           Habits.UseCases.Register.RegisterRequest
                                                ( RegisterRequest(..) )
import           Habits.UseCases.Register.RegisterResponse
                                                ( RegisterResponse(..) )
import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Control.Monad.Reader (MonadReader, asks)

type Execute m
  =  forall e
   . (e `CouldBe` RegisterError)
  => RegisterRequest
  -> ExceptT (Variant e) m RegisterResponse

newtype ExecuteW m = ExecuteW { unExecuteW :: Execute m}

newtype Register m = Register {
  _execute :: ExecuteW m
}

type instance ToSymbol (Register m) = "Register"

executeL :: forall m . Lens' (Register m) (ExecuteW m)
executeL = lens get set
 where
  set :: Register m -> ExecuteW m -> Register m
  set ar a = ar { _execute = a }
  get :: Register m -> ExecuteW m
  get Register { _execute = a } = a


execute :: forall m env . (Has.Has (Register m) env, MonadReader env m) => Execute m
execute r = do
  Register { _execute = ExecuteW f } <- asks (Has.get @(Register m))
  f r

