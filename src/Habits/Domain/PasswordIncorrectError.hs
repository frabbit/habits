module Habits.Domain.PasswordIncorrectError
  ( PasswordIncorrectError (..),
  )
where

import Habits.Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data PasswordIncorrectError = PasswordIncorrectError
  deriving (Show, Eq, Ord, Typeable)

instance Exception PasswordIncorrectError