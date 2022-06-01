module Habits.Domain.RepositoryError where

import Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data RepositoryError = RepositoryError deriving (Show, Typeable)

instance Exception RepositoryError