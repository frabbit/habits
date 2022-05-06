{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Domain.AccountRepo.InMemory where

import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.AccountRepo (Add)

add :: (Monad m) => Add m
add _ = do
  pure $ AccountId "ID"