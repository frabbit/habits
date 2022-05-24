module Habits.Domain.RefreshTokenInvalidError
  ( RefreshTokenInvalidError (..),
  )
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data RefreshTokenInvalidError = RefreshTokenInvalidError
  deriving (Show, Eq, Ord, Typeable)

instance Exception RefreshTokenInvalidError