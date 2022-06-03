module Habits.Domain.EmailConfirmationNotFoundError
  ( EmailConfirmationNotFoundError (..),
  )
where

import Habits.Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data EmailConfirmationNotFoundError = EmailConfirmationNotFoundError
  deriving (Show, Eq, Ord, Typeable)

instance Exception EmailConfirmationNotFoundError