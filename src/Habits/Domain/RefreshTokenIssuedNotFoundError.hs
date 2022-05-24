module Habits.Domain.RefreshTokenIssuedNotFoundError
  ( RefreshTokenIssuedNotFoundError (..),
  )
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data RefreshTokenIssuedNotFoundError = RefreshTokenIssuedNotFoundError
  deriving (Show, Eq, Ord, Typeable)

instance Exception RefreshTokenIssuedNotFoundError