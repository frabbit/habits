module Habits.Domain.AccountNotFoundError
  ( AccountNotFoundError (..),
  )
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data AccountNotFoundError = AccountNotFoundError
  deriving (Show, Eq, Ord, Typeable)

instance Exception AccountNotFoundError