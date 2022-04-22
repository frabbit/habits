module Habits.UseCases.Register
  ( RegisterError(..)
  , RegisterResponse(..)
  , RegisterRequest(..)
  , Register(..)
  , Execute
  , execute
  , WrapExecute(..)
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

type Execute m
  =  forall e
   . (e `CouldBe` RegisterError)
  => RegisterRequest
  -> ExceptT (Variant e) m RegisterResponse

newtype WrapExecute m = WrapExecute { unWrapExecute :: Execute m}

newtype Register m = Register {
  _execute :: WrapExecute m
}

execute :: forall m . Lens' (Register m) (WrapExecute m)
execute = lens get set
 where
  set :: Register m -> WrapExecute m -> Register m
  set ar a = ar { _execute = a }
  get :: Register m -> WrapExecute m
  get Register { _execute = a } = a


