module Habits.Domain.EmailNotConfirmedError
  ( EmailNotConfirmedError (..),
  )
where

import Habits.Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data EmailNotConfirmedError = EmailNotConfirmedError
  deriving (Show, Eq, Ord, Typeable)

instance Exception EmailNotConfirmedError