module Habits.Web.UnauthorizedError where

import Habits.Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data UnauthorizedError = UnauthorizedError deriving (Show, Typeable)

instance Exception UnauthorizedError