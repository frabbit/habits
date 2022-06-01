module Habits.Domain.EmailAlreadyUsedError
  ( EmailAlreadyUsedError (..),
  )
where

import Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data EmailAlreadyUsedError = EmailAlreadyUsedError
  deriving (Show, Typeable)

instance Exception EmailAlreadyUsedError