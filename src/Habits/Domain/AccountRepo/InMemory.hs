{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Habits.Domain.AccountRepo.InMemory where
import Habits.Domain.AccountRepo (Add)
import Habits.Domain.AccountId (AccountId(AccountId))

add :: (Monad m) => Add m
add accNew = do
  pure $ AccountId "ID"