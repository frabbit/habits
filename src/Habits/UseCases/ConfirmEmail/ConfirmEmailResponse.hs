module Habits.UseCases.ConfirmEmail.ConfirmEmailResponse where

import Habits.Prelude


data ConfirmEmailResponse = ConfirmEmailResponse
  deriving (Show, Eq, Ord)

instance Arbitrary ConfirmEmailResponse where
  arbitrary = do
    pure $ ConfirmEmailResponse {}
