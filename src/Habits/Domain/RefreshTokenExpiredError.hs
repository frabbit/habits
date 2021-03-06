module Habits.Domain.RefreshTokenExpiredError
  ( RefreshTokenExpiredError (..),
  )
where

import Habits.Prelude

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data RefreshTokenExpiredError = RefreshTokenExpiredError
  deriving (Show, Eq, Ord, Typeable)

instance Exception RefreshTokenExpiredError