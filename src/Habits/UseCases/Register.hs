module Habits.UseCases.Register
  ( RegisterError(..)
  , RegisterResponse(..)
  , RegisterRequest(..)
  , Register(..)
  , RegisterExec
  ) where

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

type RegisterExec m
  =  forall e
   . (e `CouldBe` RegisterError)
  => RegisterRequest
  -> ExceptT (Variant e) m RegisterResponse

newtype Register m = Register {
  execute :: RegisterExec m
}
