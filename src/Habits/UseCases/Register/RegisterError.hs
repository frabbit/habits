module Habits.UseCases.Register.RegisterError where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data RegisterError = RegisterError
  deriving (Show, Typeable)

instance Exception RegisterError
