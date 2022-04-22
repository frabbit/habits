module Habits.Domain.AccountId where

import Data.Text (Text)

newtype AccountId = AccountId Text deriving (Eq, Show, Ord)
